(in-package #:miao)

;;; One session per id, so successive forms see the state the last one left.
;;; Each session is its own meow service, mounted under tool-repl's context,
;;; so each id evaluates independently of the others: tool-repl only
;;; routes, handing its caller's reply cell to the session with
;;; M:DEFER-REPLY, and the session answers once its own worker replies.
;;; Calls on one id still run in order, since a session's own mailbox
;;; serialises them.
;;;
;;; Sessions start on first use; :pristine replaces one under the same id,
;;; and so does a lapsed deadline or a cancel, which kills the worker it
;;; belongs to. A
;;; worker inherited through a saved core is stale: its session is reported
;;; lost once and starts empty on the next call.
;;;
;;; An id with no activity for :IDLE seconds (600 by default; nil disables)
;;; is dropped the same way an unmounted tool-repl drops every session --
;;; see the idle timer below.
;;;
;;; Trust posture: arbitrary evaluation. Trusted operator only. A model
;;; reaches evaluation through tool-gated-eval, which checks a form against
;;; an allowlist; no gated form of this tool exists.

(defstruct worker-box
  "A session's current worker, boxed so tool-repl's cleanup can read and
kill it directly rather than asking the session -- which would queue behind
whatever call it is busy answering. CLOSED is set by that same cleanup, so
an eval already queued behind it when the session is torn down is refused
rather than started against a worker with nothing left to kill it.

PENDING and IDLE-SINCE track activity for the idle timer below: PENDING is
the count of evals in flight, and IDLE-SINCE the time the last one finished.
Set from two processes -- tool-repl's own, and the session's -- so both are
read and written under LOCK."
  worker closed
  (pending 0) (idle-since (get-internal-real-time))
  (lock (bt:make-lock :name "miao-repl-session-activity")))

(defun %box-begin-eval (box)
  (bt:with-lock-held ((worker-box-lock box))
    (incf (worker-box-pending box))))

(defun %box-end-eval (box)
  (bt:with-lock-held ((worker-box-lock box))
    (decf (worker-box-pending box))
    (setf (worker-box-idle-since box) (get-internal-real-time))))

(defun %box-idle-p (box)
  "Two values: whether BOX has no eval in flight, and how many seconds it
has been idle if so."
  (bt:with-lock-held ((worker-box-lock box))
    (values (zerop (worker-box-pending box))
            (/ (- (get-internal-real-time) (worker-box-idle-since box))
               internal-time-units-per-second))))

;;; Unnamed, like an M:AGENT: several ids each mount one, and none is meant
;;; to be looked up by name. Unnamed children are invisible to CHECKPOINT
;;; (“Checkpoints and rollback”'s %CONTEXT-ENTRIES), which is right -- a REPL
;;; session holds no state of its own worth snapshotting, only a worker.
(m:defservice repl-session ()
  ((box :initarg :box :reader session-box))
  (:default-initargs :name nil))

(defmethod m:dispose ((service repl-session) reason)
  (declare (ignore reason))
  (kill-worker (worker-box-worker (session-box service)))
  nil)

(defmethod m:handle ((service repl-session) message)
  ;; The only message a session receives: tool-repl's cast, carrying the
  ;; deferred reply cell for its own caller. If %SESSION-EVAL signals, the
  ;; session crashes and the cell is settled (:down ...) by the M:DEFER-REPLY
  ;; :UNTIL hook tool-repl installed -- no reply is dropped either way.
  (when (and (consp message) (eq (first message) :eval))
    (destructuring-bind (cell form pristine timeout cancel) (rest message)
      (let ((box (session-box service)))
        (unwind-protect (m:reply cell (%session-eval service form pristine timeout cancel))
          ;; Unconditional, so a signalling %SESSION-EVAL still marks the
          ;; session idle rather than pinning PENDING above zero forever.
          (%box-end-eval box)))))
  nil)

(defun %session-eval (service form pristine timeout cancel)
  "Evaluate FORM in SERVICE's worker, started lazily and kept in its box
across calls. A cancel of CANCEL kills the worker, as a lapsed deadline does."
  (let ((box (session-box service)))
    ;; Queued behind another eval on this id while the agent gave up on it.
    (when (and cancel (cancelled-p cancel))
      (return-from %session-eval (fail :cancelled)))
    (when pristine
      (kill-worker (worker-box-worker box))
      (setf (worker-box-worker box) nil))
    (cond
      ;; The session's owning tool-repl was unmounted while this eval sat
      ;; queued behind another on the same id; its cleanup already killed
      ;; the worker, and starting a fresh one here would outlive tool-repl
      ;; with nothing left to kill it.
      ((worker-box-closed box) (fail :unavailable))
      (t
       (let ((worker (or (worker-box-worker box)
                         (setf (worker-box-worker box) (start-worker)))))
         (cond
           ((null worker) (fail :unavailable))
           ((worker-stale-p worker)
            ;; KILL-WORKER unregisters it without signalling it -- it belongs
            ;; to a process image that no longer exists here.
            (kill-worker worker)
            (setf (worker-box-worker box) nil)
            (fail (list :error "session lost to an image relaunch; it starts empty on the next call")))
           (t
            (let ((result (worker-eval worker form timeout cancel)))
              ;; A worker that missed its deadline or was cancelled was
              ;; killed; forget it so the id starts empty rather than
              ;; answering :unavailable for ever.
              (unless (worker-alive-p worker)
                (setf (worker-box-worker box) nil))
              result))))))))

(defstruct repl-entry
  "One id's bookkeeping, held by tool-repl itself -- read and written only
from tool-repl's own process, so no lock is needed here."
  session release box (timer nil))

(define-tool :tool-repl
    (:trust :operator
     :summary "Evaluate a Lisp form in a persistent session, one per id"
     :slots ((sessions :initform (make-hash-table :test #'equal) :reader repl-sessions)
             (idle :initarg :idle :initform 600 :reader repl-idle
                   :documentation "Seconds an id may sit with no eval in
flight before its session is dropped, or nil to keep every session until
tool-repl itself stops."))
     :params ((:id string :default "default" :doc "session id")
              (:form string :required t :doc "source text of one form")
              (:pristine boolean :default nil
               :doc "restart the session's worker first")
              (:timeout (integer 1) :default +default-tool-timeout+
               :doc "kill the worker after this many milliseconds")))
  (:invoke (id form pristine timeout)
    (let ((entry (repl-entry-for service id)))
      (if (null entry)
          (fail :unavailable)
          (let ((session (repl-entry-session entry)))
            (%box-begin-eval (repl-entry-box entry))
            (m:cast session (list :eval (m:defer-reply :until session)
                                   form pristine timeout cancel-token)))))))

(defun repl-entry-for (service id)
  "ID's entry, mounted on first use or after its previous session died.
Each session is held as an effect, so stopping the service kills every one
it has, and the worker each is holding along with it."
  (let ((entry (gethash id (repl-sessions service))))
    (when (and entry (not (m:process-alive-p (repl-entry-session entry))))
      (drop-repl-session service id)
      (setf entry nil))
    (or entry
        (a:when-let* ((context (m:service-context service))
                      (process (m:service-process context)))
          (let* ((box (make-worker-box))
                 (session (m:mount process 'repl-session :box box :restart :temporary))
                 (release (m:effect service
                                    (lambda ()
                                      (lambda ()
                                        ;; CLOSED first: an eval already
                                        ;; queued behind this session's
                                        ;; mailbox must not start a worker
                                        ;; that nothing will be left to kill.
                                        (setf (worker-box-closed box) t)
                                        (kill-worker (worker-box-worker box))
                                        (m:stop session)))
                                    :label (list :repl-session id)))
                 (new-entry (make-repl-entry :session session :release release :box box)))
            (setf (gethash id (repl-sessions service)) new-entry)
            (%arm-idle-timer service id new-entry)
            new-entry)))))

(defun %arm-idle-timer (service id entry)
  "Arm ENTRY's idle timer for the id's next possible reap, if tool-repl's
:IDLE names one. Only callable from SERVICE's own process, which is where
the timer fires too -- see %IDLE-TIMER-FIRED."
  (a:when-let ((idle (repl-idle service)))
    (setf (repl-entry-timer entry)
          (m:after service idle (lambda () (%idle-timer-fired service id))
                   :label (list :repl-idle id)))))

(defun %idle-timer-fired (service id)
  "Runs on SERVICE's own process, the same as :INVOKE, so this never races
a session being newly mounted or dropped for ID. A session with an eval in
flight, or one that saw its last eval finish inside :IDLE, is re-armed
rather than dropped -- for less than :IDLE seconds left, remaining is
clamped so a base that moved backward (get-internal-real-time's origin
resets across a relaunched image, see images.md) can't stall the reap."
  (let ((entry (gethash id (repl-sessions service))))
    (when entry
      (setf (repl-entry-timer entry) nil)
      (multiple-value-bind (idle-p seconds) (%box-idle-p (repl-entry-box entry))
        (let ((remaining (if idle-p
                              (- (repl-idle service) seconds)
                              (repl-idle service))))
          (if (plusp remaining)
              (setf (repl-entry-timer entry)
                    (m:after service (min remaining (repl-idle service))
                             (lambda () (%idle-timer-fired service id))
                             :label (list :repl-idle id)))
              (drop-repl-session service id)))))))

(defun drop-repl-session (service id)
  (let ((entry (gethash id (repl-sessions service))))
    (when entry
      (remhash id (repl-sessions service))
      (a:when-let ((cancel (repl-entry-timer entry))) (funcall cancel))
      (funcall (repl-entry-release entry)))))
