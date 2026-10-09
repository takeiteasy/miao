(in-package #:miao)

;;; The meow side of a completion. The contract, wire protocols, providers and
;;; transport live in cl-inference/client; this file mounts a backend as a
;;; service, runs each completion as a pool job, and delivers its events to a
;;; sink. See docs/protocols.md.

(defun protocols (&key (registry m:*registry*))
  "Every mounted protocol's name, sorted."
  (%registered-of-kind :protocol :registry registry))

(defun providers (&key (registry m:*registry*))
  "Every mounted provider's name, sorted."
  (%registered-of-kind :provider :registry registry))

(defun %protocol-process (name &key (registry m:*registry*))
  "NAME's process and its registration props."
  (multiple-value-bind (process props) (m:lookup name :registry registry)
    (unless process (error "Nothing registered under ~s." name))
    (values process props)))

;;; A request's :DEPTH, stamped by NESTED-REQUEST, picks the pool its job runs
;;; in. Nothing else sets it, and it never reaches the wire.

(defvar *completion-depth* nil
  "The depth of the completion job this thread is running, or nil outside one.")

(defun carry-completion-depth (function)
  "FUNCTION as a closure that runs at the completion depth of the thread that
called this, for a thread a protocol body spawns to make its own completions."
  (let ((depth *completion-depth*))
    (lambda ()
      (let ((*completion-depth* depth))
        (funcall function)))))

(defun describe-protocol (name &key (registry m:*registry*))
  "The metadata of the mounted protocol NAME."
  (m:call (%protocol-process name :registry registry) '(:describe)))

(defun describe-provider (name &key (registry m:*registry*))
  "The metadata of the mounted provider NAME."
  (describe-protocol name :registry registry))

(defun nested-request (request)
  "REQUEST as a completion made from this thread: one deeper than the job
running it, or unchanged in depth when made outside a job."
  (let ((request (a:remove-from-plist request :depth)))
    (if *completion-depth*
        (list* :depth (1+ *completion-depth*) request)
        request)))

(defun %completion-call (name request &key (registry m:*registry*))
  "What to send NAME for REQUEST: (values process message timeout), TIMEOUT
in seconds. A request that fails its pre-flight, or nests too deep, answers
(values nil result) instead. Signals when nothing is registered under NAME."
  (let* ((request (nested-request request))
         (problem (or (check-request request)
                      (and (> (getf request :depth 0) *max-completion-depth*)
                           (format nil "completions nested past depth ~d"
                                   *max-completion-depth*)))))
    (if problem
        (values nil (bad-request "~a" problem))
        (let ((process (%protocol-process name :registry registry)))
          (values process
                  (list* :complete request)
                  (%caller-timeout request))))))

(defun complete (name &rest request)
  "Perform one turn against protocol or provider NAME. Returns (:ok plist) or
(:error reason)."
  (multiple-value-bind (process message timeout) (%completion-call name request)
    (if process
        (multiple-value-call #'%call-result (m:call process message :timeout timeout))
        message)))

;;; --- streaming --------------------------------------------------------

;;; The event vocabulary is the client's: a turn ends with exactly one :DONE,
;;; whose reason is the finish reason or, when the exchange failed, the failed
;;; result.

(defstruct (emitter (:constructor %make-emitter (sink)))
  sink (queue '()) (lock (bt:make-lock))
  scheduled stopping dead finished job thread
  (drained (bt:make-semaphore)))

(defstruct (fanout (:constructor make-fanout (&optional targets recording)))
  (lock (bt:make-lock)) targets recording
  ;; Newest first, kept only when RECORDING.
  history)

(defun delta-field (event)
  "The key of EVENT's streamed fragment when EVENT is a delta, else nil."
  (case (getf event :type)
    (:text-delta :text)
    (:tool-call-delta :arguments)))

(defun continues-p (last event field)
  "Whether EVENT continues the delta LAST, FIELD being the key of its fragment."
  (and (eq (getf event :type) (getf last :type))
       (equal (getf event :ref) (getf last :ref))
       (eq (and (member :parent event) t) (and (member :parent last) t))
       (stringp (getf event field))
       (stringp (getf last field))
       (or (eq field :text)
           (and (getf event :id)
                (equal (getf event :id) (getf last :id))
                (equal (getf event :name) (getf last :name))))))

;; TODO: a run with many separate events or tool calls still stays in the
;; history until it ends; cap it with a truncation marker in the replay if
;; that grows too large (#220).
(defun record-event (fanout event)
  "Add EVENT to FANOUT's history, merging a streamed text or tool-call
fragment into the last entry when it continues it. The merged entry is a new
plist: the one it replaces may still be queued at a sink. Called holding the
lock."
  (let* ((last (first (fanout-history fanout)))
         (field (delta-field event)))
    (if (and last field (continues-p last event field))
        (setf (first (fanout-history fanout))
              (list* field (concatenate 'string (getf last field) (getf event field))
                     (a:remove-from-plist last field)))
        (push event (fanout-history fanout)))))

(defun fanout-add (fanout target &key replay)
  "Add TARGET. With REPLAY, it first gets the events recorded so far, so it
hears each event once, in order: those recorded before now by the replay and
those after by the fanout itself."
  (bt:with-lock-held ((fanout-lock fanout))
    (when (and replay (fanout-recording fanout))
      (dolist (event (reverse (fanout-history fanout)))
        (deliver-event target event)))
    (pushnew target (fanout-targets fanout))))

(defun fanout-remove (fanout target)
  (bt:with-lock-held ((fanout-lock fanout))
    (setf (fanout-targets fanout) (remove target (fanout-targets fanout)))))

(defun fanout-listening-p (fanout)
  "Whether anything is on the far end of FANOUT, however deeply nested."
  (some (lambda (target)
          (if (typep target 'fanout) (fanout-listening-p target) t))
        (bt:with-lock-held ((fanout-lock fanout))
          (fanout-targets fanout))))

(defun deliver-event (sink event)
  "Deliver EVENT to SINK, a function, a meow process, an emitter or a fanout
of those. A null sink drops it."
  (etypecase sink
    (null nil)
    (m:process (m:send sink event))
    (emitter (emitter-send sink event))
    (fanout (dolist (target (bt:with-lock-held ((fanout-lock sink))
                              (when (fanout-recording sink)
                                (record-event sink event))
                              (reverse (fanout-targets sink))))
              (deliver-event target event)))
    ((or function symbol) (funcall sink event)))
  event)

;;; --- concurrent completions ------------------------------------------------

;;; Each completion runs as a job on the shared worker pool (pool.lisp), so a
;;; protocol or provider answers :DESCRIBE and further completions while one
;;; is in flight. The service still checks and layers the request on its own
;;; process; only the blocking work moves. :MAX-IN-FLIGHT caps how many of a
;;; service's completions run at once, and the rest queue, their time queued
;;; counting against their :TIMEOUT. Stopping the service cancels what is in
;;; flight and what is queued.

(defclass completion-host ()
  ((max-in-flight :initarg :max-in-flight :initform nil
                  :type (or null (integer 1)) :reader host-max-in-flight)
   (in-flight :initform '() :accessor host-in-flight)
   (closing :initform nil :accessor host-closing)
   (drained :initform (bt:make-semaphore) :reader host-drained)
   (in-flight-lock :initform (bt:make-lock) :reader host-lock))
  (:documentation "The state a service needs to run completions concurrently:
its cap, the cancel tokens of those in flight or queued, and whether the
service is stopping."))


(defun track-completion (host token)
  (bt:with-lock-held ((host-lock host))
    (push token (host-in-flight host))))

(defun untrack-completion (host token)
  (bt:with-lock-held ((host-lock host))
    (setf (host-in-flight host) (remove token (host-in-flight host)))
    (when (host-closing host)
      (bt:signal-semaphore (host-drained host)))))

(defparameter *drain-timeout* 5
  "Seconds a stopping service waits for its cancelled completions to answer.")

(defmethod m:dispose ((host completion-host) reason)
  (declare (ignore reason))
  ;; The service's exit settles every call waiting on it as :down, so the
  ;; cancelled workers get to answer first.
  (let ((tokens (bt:with-lock-held ((host-lock host))
                  (setf (host-closing host) t)
                  (copy-list (host-in-flight host))))
        (deadline (+ (get-internal-real-time)
                     (* *drain-timeout* internal-time-units-per-second))))
    (mapc #'cancel tokens)
    (dolist (token tokens)
      (declare (ignore token))
      (bt:wait-on-semaphore
       (host-drained host)
       :timeout (max 0 (/ (- deadline (get-internal-real-time))
                          internal-time-units-per-second)))))
  (call-next-method))

(defun defer-completion (host request function)
  "Call from HANDLE while answering a :complete call: queue FUNCTION on
REQUEST as a pool job and answer the call from there. FUNCTION's request
carries a :CANCEL token of the job's own, which cancelling the caller's token
or stopping HOST also cancels, and the :TIMEOUT left once it starts. The job
runs in the pool of REQUEST's :DEPTH, and completions it makes run one deeper."
  (a:when-let ((cell (m:defer-reply)))
    (let* ((token (make-cancel-token))
           (timeout (getf request :timeout +default-timeout+))
           (depth (getf request :depth 0))
           (queued-at (get-internal-real-time))
           (settled nil)
           (settled-lock (bt:make-lock))
           (job nil))
      (labels ((claim ()
                 (bt:with-lock-held (settled-lock)
                   (unless settled (setf settled t))))
               (answer (result)
                 (when (claim)
                   (m:reply cell result)
                   (untrack-completion host token)))
               (answer-unrun (result)
                 ;; FUNCTION never ran to its end, so nothing ended the stream.
                 (when (claim)
                   (emit-done-detached (getf request :stream) (getf request :ref) result)
                   (m:reply cell result)
                   (untrack-completion host token)))
               (abandon ()
                 (when (pool-abandon job)
                   (bt:make-thread (lambda () (answer-unrun (fail :timeout)))
                                   :name "miao-abandon")))
               (remaining ()
                 (- timeout (floor (* 1000 (- (get-internal-real-time) queued-at))
                                   internal-time-units-per-second))))
        (flet ((withdraw (reason)
                 (when (pool-withdraw job)
                   (answer-unrun (fail reason)))))
          (setf job (make-pool-job
                     (lambda ()
                       (let ((result (fail :cancelled))
                             (ran nil)
                             (disarm nil))
                         (unwind-protect
                              (setf result
                                    (let ((left (remaining)))
                                      (if (plusp left)
                                          (handler-case
                                              (let ((*completion-depth* depth))
                                                (setf ran left
                                                      disarm (m:schedule (+ (/ left 1000) *pool-abandon-grace*)
                                                                         #'abandon))
                                                (funcall function
                                                         (list* :cancel token :timeout left
                                                                (a:remove-from-plist request :cancel :timeout :depth))))
                                            (error (e) (fail (list :error (princ-to-string e)))))
                                          (fail :timeout))))
                           (when disarm (funcall disarm))
                           (if ran (answer result) (answer-unrun result)))))
                     :key host :limit (host-max-in-flight host)
                     :registry (m:service-registry host)))
          (if (pool-overloaded-p (pool-for depth))
              (answer-unrun (fail :unavailable))
              (progn
                (track-completion host token)
                (when (pool-submit (pool-for depth) job)
                  (m:after host (/ timeout 1000) (lambda () (withdraw :timeout))))
                (on-cancel token (lambda () (withdraw :cancelled)))
                (a:when-let ((caller (getf request :cancel)))
                  (on-cancel caller (lambda () (cancel token))))))))))
  nil)

;;; --- emitters -----------------------------------------------------------

;;; A function sink is called through a queue drained by a job on the sink
;;; pool, so a sink that blocks never holds up whoever emits, and every event
;;; reaches it one at a time, in order. A drain holds a pooled thread only
;;; while events wait, and an emitter that stalls is thrown out of its job.

(defparameter *emitter-grace* 5
  "Seconds an emitter has to deliver what it was sent once stopped.")

(defvar *emitting* nil
  "The emitter this thread is draining, so an interrupt meant for one that has
finished does nothing.")

(defun start-emitter (sink)
  "An emitter calling SINK, or nil when SINK is not a function."
  (when (and sink (typep sink '(or function symbol)))
    (%make-emitter sink)))

(defun finish-emitter (emitter)
  "Called holding EMITTER's lock."
  (unless (emitter-finished emitter)
    (setf (emitter-finished emitter) t)
    (bt:signal-semaphore (emitter-drained emitter))))

(defun drain-emitter (emitter)
  (catch emitter
    (let ((*emitting* emitter))
      (bt:with-lock-held ((emitter-lock emitter))
        (setf (emitter-thread emitter) (bt:current-thread)))
      (loop
        (let ((event (bt:with-lock-held ((emitter-lock emitter))
                       (if (emitter-queue emitter)
                           (list (pop (emitter-queue emitter)))
                           (progn
                             (setf (emitter-scheduled emitter) nil
                                   (emitter-job emitter) nil
                                   (emitter-thread emitter) nil)
                             (when (emitter-stopping emitter)
                               (finish-emitter emitter))
                             nil)))))
          (unless event (return))
          (ignore-errors (funcall (emitter-sink emitter) (first event))))))))

(defun emitter-send (emitter event)
  (bt:with-lock-held ((emitter-lock emitter))
    (unless (or (emitter-dead emitter) (emitter-stopping emitter))
      (setf (emitter-queue emitter) (nconc (emitter-queue emitter) (list event)))
      (unless (emitter-scheduled emitter)
        (setf (emitter-scheduled emitter) t
              (emitter-job emitter) (make-pool-job (lambda () (drain-emitter emitter))))
        (pool-submit (pool-for :sink) (emitter-job emitter))))))

(defun emit-done-detached (sink ref result)
  "End a turn that never ran with its one :DONE, without waiting on SINK."
  (a:if-let ((emitter (start-emitter sink)))
    (progn (emitter-send emitter (done ref (done-reason result)))
           (stop-emitter emitter)
           (reap-emitter emitter *emitter-grace*))
    (deliver-event sink (done ref (done-reason result)))))

(defun stop-emitter (emitter)
  "Have EMITTER finish once it has delivered everything sent before this."
  (bt:with-lock-held ((emitter-lock emitter))
    (setf (emitter-stopping emitter) t)
    (unless (or (emitter-scheduled emitter) (emitter-queue emitter))
      (finish-emitter emitter))))

(defun await-emitter (emitter seconds)
  "True once EMITTER has finished, waiting at most SECONDS."
  (bt:wait-on-semaphore (emitter-drained emitter) :timeout (max 0 seconds)))

(defun kill-emitter (emitter)
  "Drop what EMITTER has queued and throw its drain out of the sink it is
stuck in, or withdraw it if it has not started."
  (bt:with-lock-held ((emitter-lock emitter))
    (setf (emitter-dead emitter) t
          (emitter-queue emitter) nil)
    (a:when-let ((job (emitter-job emitter)))
      (unless (pool-withdraw job)
        (a:when-let ((thread (emitter-thread emitter)))
          (ignore-errors
           (bt:interrupt-thread thread
                                (lambda ()
                                  (when (eq *emitting* emitter)
                                    (throw emitter nil))))))))
    (finish-emitter emitter)))

(defun reap-emitter (emitter seconds)
  "Kill EMITTER unless it has finished within SECONDS."
  (m:schedule (max 0 seconds)
              (lambda ()
                (unless (await-emitter emitter 0)
                  (kill-emitter emitter)))))

;;; --- streamed turns ---------------------------------------------------

;;; The worker's deltas and the caller's :DONE reach the sink through one
;;; gate, so the sink sees exactly one :DONE and nothing after it, however
;;; the worker and the deadline race. A function sink is called from an
;;; emitter, so a sink that blocks never holds up the worker or the deadline.
;;; An emitter that has not delivered :DONE by the turn's deadline plus
;;; *SINK-GRACE* is killed, and the events queued behind it with it.

(defstruct (sink-gate (:conc-name gate-))
  sink (lock (bt:make-lock)) closed emitter)

(defun gate-deliver (gate event)
  (if (gate-emitter gate)
      (emitter-send (gate-emitter gate) event)
      (deliver-event (gate-sink gate) event)))

(defun gate-emitter-function (gate)
  (lambda (event)
    (bt:with-lock-held ((gate-lock gate))
      (unless (gate-closed gate)
        (gate-deliver gate event)))))

(defun close-gate (gate ref result &optional wait)
  "End the turn: emit its :DONE, then drop whatever the worker still sends.
WAIT, in seconds, bounds how long to wait for the sink to have seen it. True
when the sink has, or has no emitter to wait for."
  (bt:with-lock-held ((gate-lock gate))
    (unless (gate-closed gate)
      (setf (gate-closed gate) t)
      (gate-deliver gate (done ref (done-reason result)))
      (a:when-let ((emitter (gate-emitter gate)))
        (stop-emitter emitter))))
  (or (null (gate-emitter gate))
      (and wait (await-emitter (gate-emitter gate) wait))))


;;; --- the completion ---------------------------------------------------------

(defun perform-completion (backend request)
  "Run REQUEST against BACKEND under the caller's deadline and cancel token. A
request with a :STREAM sink ends it with exactly one :DONE, delivered through
an emitter so a sink that blocks never holds up the exchange."
  (let* ((sink (getf request :stream))
         (gate (make-sink-gate :sink sink :emitter (start-emitter sink)))
         (timeout (getf request :timeout +default-timeout+))
         (started (get-internal-real-time))
         (deltas (gate-emitter-function gate))
         (result (apply #'ci:complete backend
                        (if sink
                            (list* :stream (lambda (event)
                                             (unless (eq (getf event :type) :done)
                                               (funcall deltas event)))
                                   request)
                            request))))
    (when sink
      (flet ((remaining ()
               (- (/ timeout 1000)
                  (/ (- (get-internal-real-time) started)
                     internal-time-units-per-second))))
        (unless (close-gate gate (getf request :ref) result
                            (unless (member (ci:result-error result) '(:timeout :cancelled))
                              (remaining)))
          (reap-emitter (gate-emitter gate) (+ (remaining) *emitter-grace*)))))
    result))

;;; --- the handler ------------------------------------------------------

(defmacro define-protocol-handler (class (service request) &body body)
  "Define HANDLE for CLASS: (:describe) answers METADATA, (:snapshot) and
(:restore) go through SNAPSHOT and RESTORE, and (:complete . plist) runs BODY
as a pool job, DEFER-COMPLETION, with REQUEST bound to the plist, checked
against the contract. CLASS must inherit COMPLETION-HOST. Meow intercepts
%update-config, %effects and %timer-fire before HANDLE, so a service must not
use those heads."
  (a:with-gensyms (problem)
    `(defmethod m:handle ((,service ,class) message)
       (case (first message)
         (:describe (m:metadata ,service))
         (:snapshot (snapshot ,service))
         (:restore (restore ,service (second message)))
         ;; COMPLETE checks too; doing it here as well means a service
         ;; reached by a bare M:CALL sees the same checked request.
         (:complete (let* ((,request (rest message))
                           (,problem (check-request ,request)))
                      (declare (ignorable ,request))
                      (if ,problem
                          (bad-request "~a" ,problem)
                          (defer-completion ,service ,request
                            (lambda (,request)
                              (declare (ignorable ,request))
                              ,@body)))))
         (t (bad-request "unknown message ~s" (first message)))))))

;;; --- the backend service ----------------------------------------------

;;; One service class for every backend of the client: a protocol such as
;;; :PROTOCOL-OPENAI or a provider such as :OLLAMA. It is mounted under the
;;; backend's keyword, so the registry names what a caller asks for. A
;;; provider's :BASE-URL, :MODEL and :API-KEY override its declaration; a
;;; protocol takes :BASE-URL and :MODEL as defaults under each request.

(m:defservice backend-service (completion-host)
  ((backend-name :initarg :backend :initform nil :reader service-backend-name)
   (backend :reader service-backend)
   (base-url :initarg :base-url :initform nil :reader backend-base-url)
   (model :initarg :model :initform nil :reader backend-model)
   (api-key :initarg :api-key :initform nil :reader backend-api-key))
  (:name nil))

(defmethod secret-initargs ((service backend-service)) '(:api-key))

(defun resolve-backend (service)
  (let* ((name (or (service-backend-name service) (m:service-name service)))
         (registered (and name (ci:find-backend name))))
    (cond
      ((null registered) (error "No backend registered under ~s." name))
      ((typep registered 'ci:provider)
       (ci:make-provider name :base-url (backend-base-url service)
                              :model (backend-model service)
                              :api-key (backend-api-key service)))
      ((backend-api-key service)
       (error "~s is a protocol; only a provider takes :api-key." name))
      (t registered))))

(defmethod initialize-instance :after ((service backend-service) &key)
  (setf (slot-value service 'backend) (resolve-backend service)))

(defmethod m:metadata ((service backend-service))
  (let ((metadata (ci:describe-backend (service-backend service))))
    (list* :name (m:service-name service)
           (a:remove-from-plist metadata :name))))

(defun layered-defaults (service request)
  "REQUEST with the :BASE-URL and :MODEL a protocol's service was mounted with
under it, where the caller names none. A provider holds its own."
  (if (typep (service-backend service) 'ci:provider)
      request
      (append request
              (when (backend-base-url service) (list :base-url (backend-base-url service)))
              (when (backend-model service) (list :model (backend-model service))))))

(define-protocol-handler backend-service (service request)
  (perform-completion (service-backend service) (layered-defaults service request)))
