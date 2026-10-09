(in-package #:miao/cli)

;;; `miao run`: one agent run to completion. docs/cli.md.

(defparameter *usage*
  "usage: miao run PROMPT [--model PROVIDER:MODEL] [--tools NAME,...]
                 [--system-file FILE] [--system-replace] [--max-turns N] [-v]
       miao chat [--model PROVIDER:MODEL] [--tools NAME,...]
                 [--system-file FILE] [--system-replace] [--max-turns N]
       miao chat --resume [ID]
       miao chats")

(defparameter *default-system*
  "You are miao, an agent run from the command line. Do the task, then answer briefly.")

(defparameter *default-model* "ollama:llama3.2")

(define-condition usage-error (error)
  ((message :initarg :message :reader usage-message))
  (:report (lambda (condition stream) (write-string (usage-message condition) stream))))

(defun usage-error (format &rest args)
  (error 'usage-error :message (apply #'format nil format args)))

;;; --- arguments ------------------------------------------------------------

(defun parse-args (args &key (takes-prompt t))
  "ARGS after `run` or `chat`, as a plist: :prompt :model :tools :system-file
:system-replace :max-turns :verbose :resume. Without TAKES-PROMPT a prompt is an
error and --resume, answered as :LATEST or an id, is allowed, but not with the
options that describe a new agent."
  (let (prompt (model *default-model*) tools system-file system-replace max-turns verbose resume
        (described '()))
    (flet ((value (flag)
             (or (pop args) (usage-error "~a needs a value" flag))))
      (loop while args
            do (let ((arg (pop args)))
                 (cond ((string= arg "--model") (push arg described) (setf model (value arg)))
                       ((string= arg "--tools")
                        (push arg described)
                        (setf tools (uiop:split-string (value arg) :separator ",")))
                       ((string= arg "--system-file") (push arg described) (setf system-file (value arg)))
                       ((string= arg "--system-replace") (push arg described) (setf system-replace t))
                       ((and (string= arg "--resume") (not takes-prompt))
                        (setf resume (if (and args (plusp (length (first args))) (char/= #\- (char (first args) 0)))
                                         (pop args)
                                         :latest)))
                       ((string= arg "--max-turns")
                        (push arg described)
                        (let ((n (parse-integer (value arg) :junk-allowed t)))
                          (unless (and n (plusp n)) (usage-error "--max-turns wants a positive integer"))
                          (setf max-turns n)))
                       ((member arg '("-v" "--verbose") :test #'string=) (setf verbose t))
                       ((string= arg "--") (when args (setf prompt (pop args))))
                       ((and (> (length arg) 1) (char= (char arg 0) #\-))
                        (usage-error "unknown option ~a" arg))
                       ((not takes-prompt) (usage-error "unexpected argument ~s" arg))
                       (prompt (usage-error "more than one prompt: ~s and ~s" prompt arg))
                       (t (setf prompt arg))))))
    (when (and resume described)
      (usage-error "--resume takes its settings from the saved chat, not ~a" (first (last described))))
    (when (and takes-prompt (not prompt)) (usage-error "no prompt"))
    (when (and system-replace (not system-file))
      (usage-error "--system-replace needs --system-file"))
    (list :prompt prompt :model model :tools tools :system-file system-file
          :system-replace system-replace :max-turns max-turns :verbose verbose
          :resume resume)))

(defun split-model (spec)
  "SPEC, PROVIDER:MODEL, split on the first colon."
  (let ((colon (position #\: spec)))
    (unless (and colon (plusp colon) (< (1+ colon) (length spec)))
      (usage-error "--model wants PROVIDER:MODEL, got ~s" spec))
    (values (subseq spec 0 colon) (subseq spec (1+ colon)))))

(defun named (prefix name)
  (a:make-keyword (string-upcase (format nil "~a~a" prefix name))))

(defun system-prompt (system-file replace)
  (let ((text (and system-file
                   (or (probe-file system-file) (usage-error "no such file: ~a" system-file))
                   (uiop:read-file-string system-file))))
    (cond ((null text) *default-system*)
          (replace text)
          (t (format nil "~a~%~%~a" *default-system* text)))))

;;; --- running --------------------------------------------------------------

(defun load-init (home)
  (let ((init (merge-pathnames "init.lisp" home)))
    (when (probe-file init) (load init))))

(defun verbose-sink (stream done)
  "A sink printing events to STREAM; DONE, a semaphore, is signalled once the
run's :RUN-DONE is printed, since sinks drain after RUN-AGENT returns."
  (lambda (event)
    (case (getf event :type)
      (:text-delta (write-string (getf event :text "") stream))
      (:tool-call (format stream "~&[tool ~(~a~) ~s]~%" (getf event :name) (getf event :arguments)))
      (:tool-result (format stream "~&[result ~s]~%" (getf event :result)))
      (:turn-retry (format stream "~&[retry ~s]~%" (getf event :reason)))
      (:run-done (format stream "~&[done ~(~a~)]~%" (getf event :reason))))
    (finish-output stream)
    (when (eq (getf event :type) :run-done)
      (bt:signal-semaphore done))))

(defun exit-code (result)
  "RESULT, run-agent's answer, as an exit code: 0 for a run that stopped, 3
for one cut short by :max-turns or :timeout, 1 for anything else."
  (if (eq (first result) :ok)
      (case (getf (second result) :stop-reason)
        (:stop 0)
        ((:max-turns :timeout) 3)
        (t 1))
      1))

(defun report (result out err)
  (if (eq (first result) :ok)
      (let ((text (miao:content-text (getf (second result) :content)))
            (reason (getf (second result) :stop-reason)))
        (when (plusp (length text)) (format out "~a~%" text))
        (unless (eq reason :stop)
          (format err "miao: run ended: ~(~a~)~%" reason)))
      (format err "miao: ~a~%" (second result))))

(defun prepare (options context)
  "Mount the provider and tools OPTIONS name on CONTEXT. Answers the
provider's service name, the tool names and the system prompt."
  (multiple-value-bind (provider model) (split-model (getf options :model))
    (let ((provider-name (named "" provider))
          (tools (mapcar (lambda (name) (named "" name)) (getf options :tools))))
      (unless (member provider-name (miao:definitions :kind :provider))
        (usage-error "no provider ~s; defined: ~{~(~a~)~^, ~}" provider
                     (miao:definitions :kind :provider)))
      (dolist (tool tools)
        (unless (member tool (miao:definitions :kind :tool))
          (usage-error "no tool ~(~a~); defined: ~{~(~a~)~^, ~}" tool (miao:definitions :kind :tool))))
      (miao:ensure-mounted context provider-name :model model)
      (dolist (tool tools)
        (if (eq tool :tool-fs)
            (miao:ensure-mounted context tool :root (namestring (uiop:getcwd)))
            (miao:ensure-mounted context tool)))
      (values provider-name tools
              (system-prompt (getf options :system-file) (getf options :system-replace))))))

(defun agent-options (options tools)
  "The agent initargs OPTIONS and TOOLS add to a model and a system prompt."
  (append (and tools (list :tools tools))
          (and (getf options :max-turns) (list :max-turns (getf options :max-turns)))))

(defun run (options context out err)
  (multiple-value-bind (provider-name tools system) (prepare options context)
    (let* ((printed (bt:make-semaphore))
           (result (apply #'miao:run-agent context
                         :model provider-name :system system
                         :messages (list (list :role :user :content (getf options :prompt)))
                         (append (agent-options options tools)
                                 (and (getf options :verbose)
                                      (list :sink (verbose-sink err printed)))))))
      (when (getf options :verbose)
        (bt:wait-on-semaphore printed :timeout 5))
      (report result out err)
      (exit-code result))))
