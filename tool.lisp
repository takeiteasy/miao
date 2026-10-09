(in-package #:miao)

;;; The tool convention. A tool is a meow service named :TOOL-<name> whose
;;; METADATA carries :KIND :TOOL, and which answers (:describe) and
;;; (:invoke . plist). See docs/tools.md.

(defun %registered-of-kind (kind &key (registry m:*registry*))
  "Every name registered under KIND, sorted. Discovery is a scan of
registration props: meow has no props-filtered lookup. Shared by TOOLS,
PROTOCOLS, PROVIDERS and AGENTS, one per :KIND."
  (sort (loop for name in (m:names :registry registry)
              for props = (nth-value 1 (m:lookup name :registry registry))
              when (eq (getf props :kind) kind)
                collect name)
        #'string< :key #'string))

(defun tools (&key (registry m:*registry*))
  "Every registered tool name, sorted."
  (%registered-of-kind :tool :registry registry))

(defun %tool-process (name &key (registry m:*registry*))
  "NAME's process and its registration props, which carry the metadata."
  (multiple-value-bind (process props) (m:lookup name :registry registry)
    (unless process (error "No tool registered under ~s." name))
    (values process props)))

(defun describe-tool (name &key (registry m:*registry*))
  "NAME's metadata plist."
  (m:call (%tool-process name :registry registry) '(:describe)))

(defun tool-metadata (name &key (registry m:*registry*))
  "NAME's metadata as registered, read without a call to the tool, so a tool
busy with a long call cannot hold up the caller."
  (nth-value 1 (%tool-process name :registry registry)))

(defconstant +default-tool-timeout+ 30000
  "Milliseconds a tool gives its own work when the caller names no deadline.")

(defun %caller-timeout (args &optional (hops 1))
  "Seconds to wait on M:CALL, read from ARGS after coercion. A tool bounds
its own work, so the caller must outlast it -- otherwise M:CALL's 5s default
aborts the caller while the tool runs on, and the tool's own (:error
:timeout) is never seen. HOPS is the number of services the call passes
through, each of which needs that margin over the one it waits on."
  (+ (* 5 hops) (/ (getf args :timeout +default-tool-timeout+) 1000)))

(defun %call-result (reply status)
  "M:CALL's two values, folded into miao's own (:ok ...) | (:error ...)
vocabulary. STATUS is nil when the tool answered; otherwise the call itself
failed below the tool -- its process exited, the deadline lapsed, or the
call would have deadlocked -- and REPLY carries nothing useful. A process
or condition inside STATUS is not fit to print readably, so a shape M:CALL
doesn't already give a plain reason for is stringified."
  (if (null status)
      reply
      (case (and (consp status) (first status))
        (:down (fail :unavailable))
        (t (if (eq status :timeout)
               (fail :timeout)
               (fail (list :error (princ-to-string status))))))))

(defun %tool-call (name args &key (registry m:*registry*))
  "What to send NAME to invoke it with ARGS: (values process message timeout),
TIMEOUT in seconds. Arguments that fail their schema answer (values nil
result) instead. Signals when no tool is registered under NAME."
  (multiple-value-bind (process props) (%tool-process name :registry registry)
    (multiple-value-bind (coerced problem)
        (coerce-args (tool-schema props) (a:remove-from-plist args :cancel))
      (if problem
          (values nil (bad-request "~a" problem))
          (values process
                  (list* :invoke :cancel (call-cancel-token args) coerced)
                  (%caller-timeout coerced))))))

(defun invoke-tool (name &rest args)
  "Invoke NAME with ARGS, a plist. Returns (:ok plist) or (:error reason).
:CANCEL is reserved: a cancel token, passed to the tool rather than coerced."
  (multiple-value-bind (process message timeout) (%tool-call name args)
    (if process
        (multiple-value-call #'%call-result (m:call process message :timeout timeout))
        message)))

;;; Where a tool call is, for a caller deciding whether one that ignored its
;;; cancel is stuck. Kept beside the token rather than on it: the token is the
;;; client's, and only calls a plan step dispatches are watched.

(defstruct (call-state (:constructor make-call-state ()))
  (phase :queued) (settled (bt:make-semaphore)))

(defvar *call-states* (make-hash-table :test 'eq :weakness :key :synchronized t))

(defun watch-call (token)
  "Track the tool call TOKEN is passed to, so STUCK-P can tell whether it is
running. Answers TOKEN."
  (setf (gethash token *call-states*) (make-call-state))
  token)

(defun call-started (token)
  (a:when-let ((state (gethash token *call-states*)))
    (setf (call-state-phase state) :running)))

(defun call-settled (token)
  (a:when-let ((state (gethash token *call-states*)))
    (setf (call-state-phase state) :settled)
    (bt:signal-semaphore (call-state-settled state))))

(defun stuck-p (token grace)
  "True when the call TOKEN was passed to is running and, GRACE seconds on,
has still not returned. A call that has not started is not stuck: it sees
TOKEN cancelled and answers without running."
  (a:when-let ((state (gethash token *call-states*)))
    (and (eq (call-state-phase state) :running)
         (not (bt:wait-on-semaphore (call-state-settled state) :timeout grace)))))

(defun call-cancel-token (args)
  "ARGS' :CANCEL, when it is a cancel token."
  (let ((token (getf args :cancel)))
    (and (cancel-token-p token) token)))

;;; --- results ---------------------------------------------------------

;;; One error vocabulary across every tool and adapter:
;;;   (:bad-request msg) | :timeout | :unavailable | (:error detail)

(defun ok (&rest plist) (list :ok plist))
(defun tool-trust (metadata)
  "METADATA's :TRUST, or :AGENT when it names none. :OPERATOR marks a tool
that only a trusted operator may reach."
  (getf metadata :trust :agent))

;;; --- checkpoints (~takeiteasy/miao#11) --------------------------------

;;; Declared here, ahead of DEFINE-TOOL-HANDLER below, which every tool's
;;; :SNAPSHOT/:RESTORE case calls. See checkpoint.lisp for the generation
;;; file format and the CHECKPOINT/ROLLBACK API built on these.

(defgeneric snapshot (service)
  (:documentation "SERVICE's own declared state, a plain value fit to print
and read back -- no process, no closure. NIL by default: most services hold
nothing worth carrying across a restart.")
  (:method ((service m:service)) nil))

(defgeneric restore (service state)
  (:documentation "Apply STATE, as SNAPSHOT last returned it, onto SERVICE.
NIL by default.")
  (:method ((service m:service) state)
    (declare (ignore state))
    nil))

;;; --- the handler -----------------------------------------------------

(defmacro define-tool-handler (class (service args &optional (cancel (gensym "CANCEL")))
                               &body body)
  "Define HANDLE for CLASS: (:describe) answers METADATA and (:invoke . plist)
runs BODY with ARGS bound to the plist, coerced against the metadata schema,
and CANCEL to its :CANCEL token or NIL. A call whose token is already
cancelled -- one queued behind another -- answers (:error :cancelled) without
running BODY. Meow intercepts %update-config, %effects and %timer-fire before
HANDLE, so a tool must not use those heads.

Mounting the tool also checks that every :DEFAULT in its metadata prints and
reads back, as DEFINE-TOOL does at load: a call carries the schema into a
generation file."
  (a:with-gensyms (problem)
    `(progn
      (defmethod m:init :before ((,service ,class))
        (%check-readable-defaults (m:service-name ,service)
                                  (tool-schema (m:metadata ,service))))
      (defmethod m:handle ((,service ,class) message)
         (case (first message)
           (:describe (m:metadata ,service))
           ;; INVOKE-TOOL coerces too; doing it here as well means a tool
           ;; reached by a bare M:CALL sees the same checked arguments.
           (:invoke (let ((,cancel (call-cancel-token (rest message))))
                      (declare (ignorable ,cancel))
                      (if (and ,cancel (cancelled-p ,cancel))
                          (fail :cancelled)
                          (multiple-value-bind (,args ,problem)
                              (coerce-args (tool-schema (m:metadata ,service))
                                           (a:remove-from-plist (rest message) :cancel))
                            (declare (ignorable ,args))
                            (if ,problem
                                (bad-request "~a" ,problem)
                                (unwind-protect
                                     (progn (when ,cancel (call-started ,cancel))
                                            ,@body)
                                  (when ,cancel (call-settled ,cancel))))))))
           ;; Checkpoints (~takeiteasy/miao#11): every tool answers these
           ;; through SNAPSHOT/RESTORE, which default to NIL, so a tool that
           ;; holds no state worth carrying needs no method of its own.
           (:snapshot (snapshot ,service))
           (:restore (restore ,service (second message)))
           (t (bad-request "unknown message ~s" (first message))))))))

;;; --- define-tool -------------------------------------------------------

;;; One form in place of the three above: DEFSERVICE, a METADATA method and
;;; DEFINE-TOOL-HANDLER. The name is given once, as the leading keyword, so it
;;; cannot drift from the class or the registration -- unlike DEFSERVICE alone,
;;; which defaults :NAME to the class symbol.

(defun %tool-class-name (name)
  "NAME, a keyword such as :TOOL-SHELL, as the class symbol TOOL-SHELL, in
MIAO. A tool defined outside this package must still name a symbol reachable
from here, since DEFINE-TOOL always expands in the current package."
  (intern (symbol-name name)))

(defmacro define-tool (name (&key trust summary params slots background resumable) &body invoke)
  "Define the tool NAME, a keyword: a service class, its METADATA and its
:INVOKE handler, in one form. NAME is used once, for the class, the
registration and the metadata, and cannot drift between them.

PARAMS is a literal schema, checked by VALIDATE-SCHEMA at macroexpansion --
a bad specifier is a compile-time error. SLOTS is passed through to
DEFSERVICE, as for TOOL-FS's sandbox root. A :DEFAULT must print and read
back, since a call carries the schema into a generation file; the definition
signals at load otherwise, and a mount of the tool does too. BACKGROUND true
puts :BACKGROUND T in the metadata, so an agent detaches each call to it as
soon as it is dispatched. RESUMABLE true puts :RESUMABLE T there: a call to it
that a crash or restore cut short may be run again, so every op must be safe to
run twice.

INVOKE is exactly one (:INVOKE (name...) . body) clause. Each NAME binds
(getf args :name), already coerced against PARAMS; SERVICE is bound
anaphorically, as TOOL-FS and TOOL-REPL both need, and so is CANCEL-TOKEN,
the call's cancel token or NIL. A tool needing another HANDLE clause --
%UPDATE-CONFIG and friends stay off limits regardless -- falls back to
DEFSERVICE and DEFINE-TOOL-HANDLER directly."
  (validate-schema params)
  (destructuring-bind (head arg-names &body body) (first invoke)
    (unless (eq head :invoke)
      (error "DEFINE-TOOL's body must be one (:invoke (arg...) . body) clause, got ~s."
             head))
    (let ((class (%tool-class-name name)))
      `(progn
         (%check-readable-defaults ,name (list ,@(mapcar #'%param-form params)))
         (m:defservice ,class () ,slots
           (:name ,name))
         (register-definition ,name :tool ',class)
         (defmethod m:metadata ((service ,class))
           (list :kind :tool
                 :name ,name
                 :trust ,trust
                 :summary ,summary
                 ,@(and background '(:background t))
                 ,@(and resumable '(:resumable t))
                 :params (list ,@(mapcar #'%param-form params))))
         (define-tool-handler ,class (service args cancel-token)
           (let (,@(mapcar (lambda (arg-name)
                              `(,arg-name (getf args ,(a:make-keyword arg-name))))
                            arg-names))
             ,@body))))))

(defun %param-form (param)
  "PARAM, a literal schema entry, as a form that rebuilds it: the specifier
and option keys are quoted, since PARAMS is checked at macroexpansion, but
option values -- a :default naming a constant such as
+DEFAULT-TOOL-TIMEOUT+ -- are left to evaluate, except :REQUIRED-WHEN, which
is data."
  `(list* ,(param-name param) ',(param-type param)
          (list ,@(loop for (key value) on (param-options param) by #'cddr
                        collect `',key
                        collect (if (eq key :required-when) `',value value)))))
