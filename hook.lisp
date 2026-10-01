(in-package #:miao)

;;; The hook convention. An interceptor hook is a meow service named
;;; :HOOK-<name> whose METADATA carries :KIND :HOOK, and which answers
;;; (:describe) and (:intercept . plist). The agent loop runs the hooks its
;;; :HOOKS names, in order, off its own process. See docs/hooks.md.
;;;
;;; An :INTERCEPT's plist holds :PHASE, :AGENT, :PARENT (a sub-agent's only),
;;; :CANCEL (a cancel token, cancelled once the agent stops waiting) and the
;;; phase's subject, which a hook may rewrite:
;;;   :BEFORE-TURN       :MESSAGES the conversation
;;;   :BEFORE-TOOL-CALL  :ID, :NAME and :ARGUMENTS
;;;   :AFTER-TOOL-RESULT :ID, :NAME and :RESULT
;;; and answers :PASS, (:REWRITE value) or, before a tool call only, (:DENY reason).

(defparameter +hook-phases+ '(:before-turn :before-tool-call :after-tool-result))

(defparameter +hook-subject-keys+
  '(:before-turn :messages :before-tool-call :arguments :after-tool-result :result))

(defun hooks (&key (registry m:*registry*))
  "Every registered hook name, sorted."
  (%registered-of-kind :hook :registry registry))

(defun describe-hook (name &key (registry m:*registry*))
  "NAME's metadata plist."
  (multiple-value-bind (process props) (m:lookup name :registry registry)
    (declare (ignore props))
    (unless process (error "No hook registered under ~s." name))
    (m:call process '(:describe))))

(defun %check-hook-options (phases on-error)
  "A problem string for PHASES or ON-ERROR, or nil."
  (cond ((not (and (a:proper-list-p phases) (subsetp phases +hook-phases+)))
         (format nil "a hook's :phases are some of ~s, not ~s" +hook-phases+ phases))
        ((not (member on-error '(:deny :pass)))
         (format nil "a hook's :on-error is :deny or :pass, not ~s" on-error))))

(defun %hook-answer (request thunk)
  "THUNK's value, or (:error detail) when it signals, so a hook that fails
answers the loop and its service stays up. A call whose :CANCEL token is already
cancelled answers :cancelled without running THUNK."
  (let ((token (getf request :cancel)))
    (if (and token (cancelled-p token))
        (fail :cancelled)
        (handler-case (funcall thunk)
          (error (e) (fail (list :error (princ-to-string e))))))))

(defmacro define-hook (name (&key summary phases (on-error :deny)
                               (timeout '+default-tool-timeout+) slots)
                       &body intercept)
  "Define the hook NAME, a keyword: a service class, its METADATA and its
:INTERCEPT handler, in one form.

PHASES defaults to every phase. ON-ERROR is how the agent treats a hook that
signals, times out or answers badly: :DENY fails closed, :PASS carries on
without it. TIMEOUT is the milliseconds the agent waits on one answer; the
run's :DEADLINE still bounds it. SLOTS is passed through to DEFSERVICE.

INTERCEPT is exactly one (:INTERCEPT (phase request) . body) clause. PHASE is
the phase and REQUEST the whole :INTERCEPT plist; SERVICE is bound
anaphorically, as in DEFINE-TOOL. The body answers :PASS, (:REWRITE value) or
(:DENY reason). A condition it signals is answered as an error."
  (let ((phases (or phases +hook-phases+)))
    (let ((problem (%check-hook-options phases on-error)))
      (when problem (error "~a" problem)))
    (destructuring-bind (head (phase-var request-var) &body body) (first intercept)
      (unless (eq head :intercept)
        (error "DEFINE-HOOK's body must be one (:intercept (phase request) . body) clause, got ~s."
               head))
      (let ((class (%tool-class-name name)))
        `(progn
           (m:defservice ,class () ,slots
             (:name ,name))
           (register-definition ,name :hook ',class)
           (defmethod m:metadata ((service ,class))
             (list :kind :hook :name ,name :summary ,summary :phases ',phases
                   :on-error ,on-error :timeout ,timeout))
           (defmethod m:handle ((service ,class) message)
             (case (first message)
               (:describe (m:metadata service))
               (:intercept (let* ((,request-var (rest message))
                                  (,phase-var (getf ,request-var :phase)))
                             (declare (ignorable ,phase-var ,request-var))
                             (%hook-answer ,request-var (lambda () ,@body))))
               (:snapshot (snapshot service))
               (:restore (restore service (second message)))
               (t (bad-request "unknown message ~s" (first message))))))))))

;;; --- a function as a hook ---------------------------------------------------

;;; Unnamed, like REPL-SESSION: an agent mounts one for each function its
;;; :HOOKS holds, and it is not meant to be looked up by name.

(m:defservice hook-function ()
  ((fn :initarg :fn :reader hook-fn)
   (label :initarg :label :initform :function :reader hook-label)
   (phases :initarg :phases :initform +hook-phases+ :reader hook-phases)
   (on-error :initarg :on-error :initform :deny :reader hook-on-error)
   (timeout :initarg :timeout :initform +default-tool-timeout+ :reader hook-timeout))
  (:default-initargs :name nil))

(defmethod m:metadata ((service hook-function))
  (list :kind :hook :name (hook-label service) :phases (hook-phases service)
        :on-error (hook-on-error service) :timeout (hook-timeout service)))

(defmethod m:handle ((service hook-function) message)
  (case (first message)
    (:describe (m:metadata service))
    (:intercept (let ((request (rest message)))
                  (%hook-answer request
                                (lambda () (funcall (hook-fn service) (getf request :phase) request)))))
    (t (bad-request "unknown message ~s" (first message)))))

;;; --- reading an answer ----------------------------------------------------

(defun %valid-subject-p (phase value)
  "Whether VALUE can stand as PHASE's subject after a rewrite."
  (flet ((plist-p (x) (and (a:proper-list-p x) (evenp (length x)))))
    (ecase phase
      (:before-turn (and (a:proper-list-p value)
                         (every (lambda (message) (and (plist-p message) (getf message :role)))
                                value)))
      (:before-tool-call (and (plist-p value)
                              (loop for key in value by #'cddr always (keywordp key))))
      (:after-tool-result (and (consp value)
                               (or (and (eq (first value) :ok) (plist-p (second value)))
                                   (and (eq (first value) :error) (consp (cdr value)))))))))

(defun interpret-hook-answer (phase answer)
  "ANSWER, a hook's reply to PHASE, as (values :pass), (values :rewrite value),
(values :deny reason) or (values :failed reason)."
  (cond ((eq answer :pass) :pass)
        ((tool-error-p answer) (values :failed (tool-error answer)))
        ((and (consp answer) (eq (first answer) :rewrite) (consp (cdr answer)))
         (if (%valid-subject-p phase (second answer))
             (values :rewrite (second answer))
             (values :failed (list :bad-rewrite phase))))
        ((and (consp answer) (eq (first answer) :deny) (eq phase :before-tool-call))
         (values :deny (second answer)))
        (t (values :failed (list :bad-answer answer)))))
