(in-package #:miao/ui)

;;; A client attached to a named agent: it folds the agent's events into a
;;; state and sends the operator's commands back. No rendering.

(defvar *command-timeout* 5
  "Seconds a command waits for its agent while it is not mounted or is being restarted.")

(define-condition agent-unavailable (error)
  ((agent :initarg :agent :reader unavailable-agent)
   (status :initarg :status :reader unavailable-status))
  (:report (lambda (condition stream)
             (format stream "agent ~s did not answer: ~s"
                     (unavailable-agent condition) (unavailable-status condition)))))

(defstruct (client (:constructor %make-client (agent registry on-change)))
  agent registry on-change sink
  (lock (bt:make-lock :name "miao/ui client"))
  (current (make-state))
  (folded 0))

(defun client-state (client)
  "The state so far: an immutable snapshot, safe to draw from any thread."
  (bt:with-lock-held ((client-lock client))
    (client-current client)))

(defun fold-into (client event)
  (let ((state (bt:with-lock-held ((client-lock client))
                 (incf (client-folded client))
                 (setf (client-current client)
                       (fold-event (client-current client) event)))))
    (a:when-let ((on-change (client-on-change client)))
      (funcall on-change state))))

(defun seed (client answer)
  "Take what a :subscribe ANSWER says of a run already under way, so the state
is running before the replay of the run so far arrives, unless events have
arrived first and say more."
  (destructuring-bind (&key running turn &allow-other-keys) (second answer)
    (when running
      (bt:with-lock-held ((client-lock client))
        (when (zerop (client-folded client))
          (let* ((old (client-current client))
                 (root (copy-node (state-root old)))
                 (next (copy-state old)))
            (setf (node-status root) :running (node-turn root) (or turn 0)
                  (state-nodes next) (substitute root (state-root old) (state-nodes old))
                  (client-current client) next)))))))

(defun send (client message)
  "MESSAGE to the client's agent, waiting out a restart. Signals
AGENT-UNAVAILABLE if it stays unreachable."
  (let ((deadline (+ (get-internal-real-time)
                     (* *command-timeout* internal-time-units-per-second)))
        (status nil))
    (loop
      (let ((process (m:lookup (client-agent client) :registry (client-registry client))))
        (if process
            (multiple-value-bind (answer failure) (m:call process message)
              (cond ((null failure) (return answer))
                    ((and (consp failure) (eq :down (first failure))) (setf status failure))
                    (t (error 'agent-unavailable :agent (client-agent client) :status failure))))
            (setf status :not-mounted)))
      (when (> (get-internal-real-time) deadline)
        (error 'agent-unavailable :agent (client-agent client) :status status))
      (sleep 0.01))))

(defun attach (agent &key on-change (registry m:*registry*))
  "A client of the agent mounted as AGENT. ON-CHANGE, if given, is called with
each new state from the thread that delivered the event."
  (let ((client (%make-client agent registry on-change)))
    (setf (client-sink client) (lambda (event) (fold-into client event)))
    (let ((answer (send client (list :subscribe (client-sink client)))))
      (seed client answer))
    client))

(defun detach (client)
  "Stop hearing the agent. Events already on their way may still arrive."
  (send client (list :unsubscribe (client-sink client))))

;;; --- commands -------------------------------------------------------------

(defun user-messages (text)
  (list (list :role :user :content text)))

(defun run (client text)
  "Start a run with TEXT as the user's message."
  (send client (list :run :messages (user-messages text))))

(defun continue-run (client text)
  "Start a run that carries on from the agent's own conversation."
  (send client (list :run :continue t :messages (user-messages text))))

(defun steer (client text &key interrupt)
  "Fold TEXT into the run under way; with INTERRUPT, abandon the turn in flight."
  (send client (append (list :steer :content text) (and interrupt (list :interrupt t)))))

(defun cancel (client)
  (send client '(:cancel)))
