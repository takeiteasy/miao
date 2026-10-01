(in-package #:miao)

;;; The operator approval hook (~takeiteasy/miao#118). A before-tool-call hook
;;; that holds a call until the operator answers it. It is a service written
;;; out by hand rather than with DEFINE-HOOK, which answers from its body and so
;;; cannot defer a reply. See docs/approvals.md.
;;;
;;; The hook asks through its request's :EMIT, as an :APPROVAL-REQUEST event,
;;; and is answered with (:ANSWER approval decision), which ANSWER-APPROVAL
;;; sends. The approval ends in an :APPROVAL-DONE event: answered, or withdrawn
;;; when the agent stopped waiting (the hook's cancel token).

(m:defservice hook-approval ()
  ((tools :initarg :tools :initform nil :reader approval-tools)
   (timeout :initarg :timeout :initform 600000 :reader approval-timeout)
   (seq :initform 0 :accessor %approval-seq)
   ;; (approval . plist) of each question still open, newest first.
   (pending :initform nil :accessor %approvals)
   ;; run id -> the tool names approved for the rest of that run.
   ;; TODO: nothing tells the hook a run ended, so an entry stays for the hook's
   ;; life; prune on a run-end notice, or key a weak table by the run handle (#212).
   (always :initform (make-hash-table) :reader %approved-always))
  (:name :hook-approval))

(defmethod m:metadata ((service hook-approval))
  (list :kind :hook :name :hook-approval
        :summary "Hold a call to an operator tool until the operator approves it"
        :phases '(:before-tool-call) :on-error :deny :timeout (approval-timeout service)))

(defun approval-gated-p (service name)
  "Whether the call to tool NAME needs the operator: NAME is one of :TOOLS or, with
none given, a tool of :OPERATOR trust."
  (if (approval-tools service)
      (member name (approval-tools service))
      (eq :operator (tool-trust (nth-value 1 (m:lookup name :registry (m:service-registry service)))))))

(defun approval-emit (approval type &rest event)
  (a:when-let ((emit (getf approval :emit)))
    (funcall emit (list* :type type :approval (getf approval :approval) event))))

(defun ask-approval (service request)
  "Hold the call REQUEST describes until it is answered or withdrawn."
  (a:if-let ((cell (m:defer-reply)))
    (let* ((id (incf (%approval-seq service)))
           (self (m:self))
           (approval (list :approval id :cell cell :emit (getf request :emit)
                           :run (getf request :run) :id (getf request :id)
                           :name (getf request :name) :arguments (getf request :arguments)
                           :agent (getf request :agent))))
      (push (cons id approval) (%approvals service))
      (on-cancel (getf request :cancel) (lambda () (m:cast self (list :withdraw id))))
      (approval-emit approval :approval-request :id (getf approval :id)
                     :name (getf approval :name) :arguments (getf approval :arguments))
      :deferred)
    '(:deny "an approval needs a call to answer")))

(defun settle-approval (service id decision)
  "Answer the approval ID with :ALLOW, :ALWAYS or :DENY, and tell the front end
before the hook answers, so the event is ahead of the call's own."
  (let ((approval (cdr (assoc id (%approvals service)))))
    (setf (%approvals service) (remove id (%approvals service) :key #'car))
    (when (eq decision :always)
      (pushnew (getf approval :name) (gethash (getf approval :run) (%approved-always service))))
    (approval-emit approval :approval-done :answer decision)
    (m:reply (getf approval :cell)
             (if (eq decision :deny) '(:deny "denied by the operator") :pass))))

(defun withdraw-approval (service id)
  (a:when-let ((approval (cdr (assoc id (%approvals service)))))
    (setf (%approvals service) (remove id (%approvals service) :key #'car))
    (approval-emit approval :approval-done :answer :withdrawn)))

(defmethod m:handle ((service hook-approval) message)
  (case (first message)
    (:describe (m:metadata service))
    (:intercept
     (let* ((request (rest message))
            (name (getf request :name)))
       (if (and (approval-gated-p service name)
                (not (member name (gethash (getf request :run) (%approved-always service)))))
           (ask-approval service request)
           :pass)))
    (:answer
     (destructuring-bind (id decision) (rest message)
       (cond ((not (member decision '(:allow :deny :always)))
              (bad-request "an answer is :allow, :deny or :always, not ~s" decision))
             ((not (assoc id (%approvals service)))
              (bad-request "no approval ~s is waiting" id))
             (t (settle-approval service id decision)
                :ok))))
    (:withdraw (withdraw-approval service (second message))
     nil)
    (:pending (loop for (nil . approval) in (reverse (%approvals service))
                    collect (a:remove-from-plist approval :cell :emit)))
    (:snapshot (snapshot service))
    (:restore (restore service (second message)))
    (t (bad-request "unknown message ~s" (first message)))))

(register-definition :hook-approval :hook 'hook-approval)

(defun answer-approval (approval decision &key (hook :hook-approval) (registry m:*registry*))
  "Answer the operator approval APPROVAL, as an :APPROVAL-REQUEST event names it,
with DECISION: :ALLOW, :DENY, or :ALWAYS, which allows it and every later call to
that tool for the rest of the run. Answers :OK, or a bad-request tool error when no
such approval is waiting."
  (m:call (m:lookup hook :registry registry) (list :answer approval decision)))
