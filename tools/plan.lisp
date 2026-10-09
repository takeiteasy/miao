(in-package #:miao)

;;; The DSL gate for untrusted input (~takeiteasy/miao#6). A plan may only
;;; express a sequence of declared tool calls with typed arguments, never a
;;; form: there is no EVAL and no host-side reader, so the audit surface is
;;; this interpreter plus each named tool's own schema.
;;;
;;; This gates *what a plan may call*, not arbitrary evaluation: tool-eval
;;; and tool-repl stay :trust :operator, untouched. The other shape #6
;;; named -- an allowlist over raw Lisp, so tool-eval itself could be
;;; reached -- is not this ticket's; tracked as a follow-up in
;;; ~takeiteasy/miao#44.
;;;
;;; A step:
;;;
;;;   (:as "name" :tool "tool-fs" :args (:op :read :path "README.md"))
;;;
;;; :AS binds the step's result plist under a name a later step's :ARGS may
;;; reach with (:ref "name.key"), substituted before that step runs.
;;; (:quote X) passes X as it is, so a step can hand a tool the shape
;;; (:ref "...") itself. :TOOL
;;; must be in this service's own :ALLOW *and* the named tool's own
;;; :trust must be :agent -- an :ALLOW naming an operator-trusted tool is
;;; refused, so the gate cannot be used to re-export tool-shell. tool-plan
;;; is never itself reachable from a plan, so plans do not nest.
;;;
;;; The whole plan is checked before any step runs: every :tool resolvable,
;;; allowed and agent-trusted; every :as unique; every :ref naming an
;;; earlier step. A step that errors ends the plan, with the results so far.
;;;
;;; :timeout bounds the whole plan, each step included: a step's own :timeout
;;; is clamped to the time left, the wait on it ends when that lapses, and
;;; its cancel token is then cancelled. A tool that honours neither is killed
;;; a grace period later, and its supervisor restarts it (~takeiteasy/miao#145)
;;; -- unless it would not be restarted, in which case it is left running
;;; (~takeiteasy/miao#147).

(define-tool :tool-plan
    (:trust :agent
     :summary "Run a checked sequence of declared tool calls"
     :slots ((allow :initarg :allow :initform nil :reader plan-allow)
             (max-steps :initarg :max-steps :initform 16 :reader plan-max-steps))
     :params ((:steps (array-of
                       (object (:as (or null string)
                                :doc "name to bind this step's result under")
                               (:tool string :required t
                                :doc "tool name, e.g. \"tool-fs\"")
                               (:args any
                                :doc "arguments for the tool; (:ref \"name.key\")
substitutes an earlier step's result, (:quote x) passes x as it is")))
               :required t :doc "the steps to run, in order")
              (:timeout (integer 1) :default +default-tool-timeout+
               :doc "whole-plan deadline in milliseconds; bounds each step")))
  (:invoke (steps timeout)
    ;; INVOKE-TOOL has no registry argument of its own -- it reads
    ;; M:*REGISTRY*, which this service's own thread does not inherit from
    ;; whichever thread mounted it. Rebind it here, as the agent loop does
    ;; before its own calls back into INVOKE-TOOL.
    (let ((m:*registry* (m:service-registry service)))
      (run-plan service steps timeout cancel-token))))

;;; --- validation, before any step runs ---------------------------------

(defun run-plan (service steps timeout-ms &optional cancel)
  (a:if-let (problem (validate-plan service steps))
    (bad-request "~a" problem)
    (execute-plan service steps timeout-ms cancel)))

(defun validate-plan (service steps)
  "NIL when STEPS may run as given, or a message naming the problem."
  (unless (listp steps)
    (return-from validate-plan "steps must be a list"))
  (when (> (length steps) (plan-max-steps service))
    (return-from validate-plan
      (format nil "plan exceeds ~d steps" (plan-max-steps service))))
  (let ((seen '()))
    (dolist (step steps)
      (a:when-let (problem (or (validate-step service step seen)
                               (check-refs (getf step :args) seen)))
        (return-from validate-plan problem))
      (a:when-let (as (getf step :as)) (push as seen))))
  nil)

(defun validate-step (service step seen)
  (let ((as (getf step :as))
        (tool-name (getf step :tool)))
    (cond
      ((null tool-name) "a step is missing :tool")
      ((and as (member as seen :test #'string=))
       (format nil "duplicate step name ~s" as))
      ((string-equal tool-name "tool-plan") "plans do not nest")
      (t (multiple-value-bind (process props)
             (m:lookup (lisp-tool-name tool-name) :registry (m:service-registry service))
           (cond
             ((not (member (lisp-tool-name tool-name) (plan-allow service)))
              (format nil "~a is not in this plan's allow-list" tool-name))
             ((null process) (format nil "no tool named ~a" tool-name))
             ((not (eq (tool-trust props) :agent))
              (format nil "~a is not agent-trusted" tool-name))
             (t nil)))))))

(defun ref-form-p (value)
  "True when VALUE is (:ref \"name.key\") -- a one-key plist, so it survives
the JSON round trip unchanged."
  (and (consp value) (eq (first value) :ref) (stringp (second value))
       (null (cddr value))))

(defun quote-form-p (value)
  "True when VALUE is (:quote X), which passes X through as it is."
  (and (consp value) (eq (first value) :quote) (consp (cdr value))
       (null (cddr value))))

(defun parse-ref (text)
  "TEXT, \"name.key\", as (name . key) split on its first \".\", or NIL if
TEXT does not have that shape."
  (let ((dot (position #\. text)))
    (and dot (plusp dot) (< (1+ dot) (length text))
         (cons (subseq text 0 dot) (subseq text (1+ dot))))))

(defun check-refs (args seen)
  "NIL when every (:ref ...) in ARGS names one of SEEN, the steps declared
before this one, or a message naming the first problem found. ARGS' own
elements are checked, never ARGS itself, and a list is walked by element so
a plist tail is never taken for a marker."
  (some (lambda (value) (check-value value seen))
        (and (listp args) args)))

(defun check-value (value seen)
  (cond
    ((quote-form-p value) nil)
    ((ref-form-p value)
     (let ((parsed (parse-ref (second value))))
       (cond
         ((null parsed) (format nil "malformed ref ~s" (second value)))
         ((not (member (car parsed) seen :test #'string=))
          (format nil "ref ~s names an unknown or later step" (second value)))
         (t nil))))
    ((a:proper-list-p value) (some (lambda (v) (check-value v seen)) value))
    (t nil)))

;;; --- execution ---------------------------------------------------------

(defun execute-plan (service steps timeout-ms cancel)
  "Run STEPS in order, handing each a token cancelled with CANCEL, so a
cancelled plan stops the step in flight and refuses the rest."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms internal-time-units-per-second) 1000)))
        (results (make-hash-table :test #'equal))
        (n 0))
    (flet ((fail-step (tool-name reason)
             (fail (list :step n :tool tool-name :reason reason
                         :results (plan-results-plist results)))))
      (dolist (step steps)
        (incf n)
        (let* ((as (getf step :as))
               (tool-name (getf step :tool))
               (left (floor (* 1000 (- deadline (get-internal-real-time)))
                            internal-time-units-per-second)))
          (when (<= left 0)
            (return-from execute-plan (fail-step tool-name :timeout)))
          (let ((result (invoke-step (lisp-tool-name tool-name)
                                     (resolve-refs (getf step :args) results)
                                     cancel left service)))
            (when (result-error-p result)
              (return-from execute-plan (fail-step tool-name (result-error result))))
            (when as
              (setf (gethash as results) (second result))))))
      (ok :results (plan-results-plist results) :steps n))))

(defconstant +plan-kill-grace+ 1
  "Seconds a timed-out step's tool gets to notice its cancel token before it
is killed.")

(defun invoke-step (name args cancel left-ms service)
  "INVOKE-TOOL for one step, held to LEFT-MS: a :TIMEOUT the tool declares is
clamped to it, and the wait on the tool ends with it, cancelling the tool's
own token. A tool that is no longer registered, such as one restarting, answers
:UNAVAILABLE. A tool still running +PLAN-KILL-GRACE+ seconds after that is
killed if SERVICE's context would restart it. That token is cancelled with
CANCEL."
  (let ((token (watch-call (make-cancel-token))))
    (when cancel
      (on-cancel cancel (lambda () (cancel token))))
    (multiple-value-bind (process message timeout)
        (handler-case (%tool-call name (list* :cancel token (clamp-timeout name args left-ms)))
          (error () (values nil (fail :unavailable))))
      (if (null process)
          message
          (multiple-value-bind (reply status)
              (m:call process message :timeout (min timeout (/ left-ms 1000.0)))
            (when (eq status :timeout)
              (cancel token)
              (when (and (stuck-p token +plan-kill-grace+)
                         (restartable-p service process))
                (m:kill process)))
            (%call-result reply status))))))

(defun restartable-p (service process)
  "True when PROCESS is a child, at any depth, of SERVICE's context that its
supervisor restarts after a kill. A tool mounted :TEMPORARY, or not mounted
under that context, would be gone for good."
  (labels ((search-children (context-process)
             (dolist (child (m:children context-process))
               (cond ((eq (getf child :process) process)
                      (return (and (member (getf child :restart) '(:transient :permanent)) t)))
                     ((and (getf child :process) (subtypep (getf child :class) 'm:context))
                      (a:when-let ((found (search-children (getf child :process))))
                        (return found)))))))
    (a:when-let ((context (m:service-context service)))
      (search-children (m:service-process context)))))

(defun clamp-timeout (name args left-ms)
  "ARGS with :TIMEOUT held to LEFT-MS, when NAME declares one and ARGS' is
absent or a number. Anything else is left for coercion to refuse."
  ;; TODO: TOOL-METADATA here looks in the global registry, not the plan's (#215).
  (let ((own (getf args :timeout +default-tool-timeout+)))
    (if (and (find :timeout (tool-schema (tool-metadata name)) :key #'param-name)
             (realp own))
        (list* :timeout (min own left-ms) (a:remove-from-plist args :timeout))
        args)))

(defun resolve-refs (args results)
  "ARGS with each (:ref ...) replaced by its value and each (:quote X) by X."
  (if (listp args)
      (mapcar (lambda (value) (resolve-value value results)) args)
      args))

(defun resolve-value (value results)
  (cond
    ((quote-form-p value) (second value))
    ((ref-form-p value) (resolve-ref (second value) results))
    ((a:proper-list-p value)
     (mapcar (lambda (v) (resolve-value v results)) value))
    (t value)))

(defun resolve-ref (text results)
  "TEXT's value, read out of RESULTS -- VALIDATE-PLAN has already checked
TEXT names an earlier step; the key within that step's own result is not
checked ahead of time, and is simply absent if TEXT names none it has."
  (let ((parsed (parse-ref text)))
    (getf (gethash (car parsed) results) (a:make-keyword (string-upcase (cdr parsed))))))

(defun plan-results-plist (results)
  (let ((out '()))
    (maphash (lambda (name value)
               ;; PUSH is LIFO: the value goes on first, so each pair comes
               ;; back out as (key value), not reversed.
               (push value out)
               (push (a:make-keyword (string-upcase name)) out))
             results)
    out))
