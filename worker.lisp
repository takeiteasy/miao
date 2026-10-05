(in-package #:miao)

;;; Workers: a bare Lisp child process evaluating forms over stdio, so a
;;; crash or a hang costs at most a deadline and never the host image.
;;;
;;; The child loads nothing -- no Quicklisp, no meow, no miao. Loading
;;; Quicklisp alone costs ten times a bare launch, which is what makes a
;;; fresh process per evaluation cheaper than pooling one.
;;;
;;; Protocol, one exchange per line, read on both sides with *READ-EVAL*
;;; bound to nil:
;;;
;;;   host   -> (:eval "<source>")
;;;   worker -> (:ready)                              once, at boot
;;;          -> (:ok ("<value>" ...) "<output>" <elided>)
;;;          -> (:error "<message>" "<output>")
;;;          -> (:reader-error "<message>")
;;;
;;; The source travels as a string rather than a form, and <elided> is
;;; either the keyword :ELIDED or NIL: the envelope then holds nothing but
;;; keywords, strings and a list of strings, so source that does not read
;;; costs one reply instead of desynchronising the stream.
;;;
;;; ("<value>" ...) is EVAL's every value, printed in order -- empty for a
;;; form returning none. <elided> is set when any value's printed form was
;;; cut by the worker's character cap or its *PRINT-LENGTH*/*PRINT-LEVEL*,
;;; or when there were more values or more combined characters than the
;;; worker keeps. The values themselves stay reachable either way: the
;;; worker keeps a REPL history under *, ** and *** for the first value and
;;; /, // and /// for the whole list, so a caller that gets :ELIDED can
;;; inspect them with a further :eval rather than lose the rest.
;;;
;;; A worker leads its own process group (see process.lisp), so a form that
;;; backgrounds a process is signalled along with the worker at kill time,
;;; the same as tools/shell.lisp's commands.

(defmacro worker-program ()
  "The child's loop, read from worker-program.lisp as text when this file is
compiled: it is source the child evaluates, not source the host loads.
ASDF does not know that, so editing the program means touching this file too."
  (uiop:read-file-string
   (merge-pathnames "worker-program.lisp"
                    (or *compile-file-truename* *load-truename*))))

(defparameter *worker-program* (worker-program)
  "The child's read/eval/print loop, passed on its command line.")

(defparameter *worker-command* nil
  "Argv that starts a bare Lisp, or NIL for the host's own SBCL binary. The
program is appended as the final argument. Set this to run workers under a
different binary or with different flags than the host's own invocation.")

(defconstant +worker-start-timeout+ 5000
  "Milliseconds a worker gets to answer its handshake.")

(defun worker-argv (&optional heap)
  "The host's bare, quiet, non-interactive invocation, with a HEAP megabyte
dynamic space when HEAP is given. A memory-exhausted worker exits rather than
waiting in the debugger for its deadline."
  (or *worker-command*
      (append (list (namestring sb-ext:*runtime-pathname*))
              (when heap
                (list "--dynamic-space-size" (princ-to-string heap)
                      "--disable-ldb" "--lose-on-corruption"))
              (list "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit"
                    "--eval"))))

(defvar *boot* (list :boot)
  "Identifies this process image. A worker records the value it started
under, so one inherited through a saved core reads as stale.")

(defvar *live-workers* '()
  "Workers started and not yet killed, so RELAUNCH can kill them before it
replaces the process.")

(defvar *live-workers-lock* (bt:make-lock :name "miao-live-workers"))

(defstruct (worker (:constructor %make-worker (process &aux (boot *boot*))))
  process boot)

(defun worker-stale-p (worker)
  "True for a worker inherited from an earlier process image: its pid and
pipes belong to that image, so nothing here may signal or touch them."
  (not (eq (worker-boot worker) *boot*)))

(defun %register-worker (worker)
  (bt:with-lock-held (*live-workers-lock*) (push worker *live-workers*)))

(defun %unregister-worker (worker)
  "True when WORKER was registered, so exactly one caller gets to kill it."
  (bt:with-lock-held (*live-workers-lock*)
    (when (member worker *live-workers*)
      (setf *live-workers* (remove worker *live-workers*))
      t)))

(defun start-worker (&key heap)
  "A running worker, or NIL if the child never answered its handshake. HEAP
caps its heap in megabytes; *WORKER-COMMAND* is used as given and ignores it."
  (let ((worker (ignore-errors
                 (%make-worker
                  (launch-in-process-group
                   (append (worker-argv heap) (list *worker-program*))
                   :input :stream :output :stream
                   ;; Diagnostics are reported in band; anything else the
                   ;; child writes to stderr would corrupt the protocol.
                   :error-output nil)))))
    (when worker
      (%register-worker worker)
      (if (equal '(:ready) (read-reply worker +worker-start-timeout+))
          worker
          (progn (kill-worker worker) nil)))))

(defun worker-alive-p (worker)
  (and worker
       (not (worker-stale-p worker))
       (uiop:process-alive-p (worker-process worker))))

(defun kill-worker (worker)
  "Kill WORKER once: a second call finds it unregistered and leaves alone a
pid the first has already reaped."
  (when (and worker (%unregister-worker worker) (not (worker-stale-p worker)))
    (terminate-process-group (worker-process worker)))
  nil)

(defun kill-live-workers ()
  "Kill every registered worker. The list is copied first: KILL-WORKER
takes the same, non-recursive, lock."
  (dolist (worker (bt:with-lock-held (*live-workers-lock*) (copy-list *live-workers*)))
    (kill-worker worker)))

(defun forget-workers ()
  "Start a new process image's worker bookkeeping: every worker held so far
reads as stale, and none is signalled. SBCL's own list of child processes
survives a save too; its entries for these workers go, so a pid a new child
reuses is never reaped through them."
  (let ((pids (mapcar (lambda (w) (uiop:process-info-pid (worker-process w)))
                      *live-workers*)))
    (sb-thread:with-recursive-lock (sb-impl::*active-processes-lock*)
      (setf sb-impl::*active-processes*
            (remove-if (lambda (p) (member (sb-ext:process-pid p) pids))
                       sb-impl::*active-processes*))))
  (setf *boot* (list :boot))
  (bt:with-lock-held (*live-workers-lock*) (setf *live-workers* '())))

(defun worker-eval (worker source timeout-ms &optional cancel)
  "Evaluate SOURCE in WORKER. Returns a tool result; a worker that missed
its deadline, was cancelled through CANCEL, or died is dead afterwards and
the caller must not reuse it."
  (if (not (worker-alive-p worker))
      (fail :unavailable)
      (let ((stream (uiop:process-info-input (worker-process worker))))
        (handler-case (progn (prin1 (list :eval source) stream)
                             (terpri stream)
                             (finish-output stream))
          (error () (return-from worker-eval (fail :unavailable))))
        (interpret-reply (read-reply worker timeout-ms cancel)))))

(defun read-reply (worker timeout-ms &optional cancel)
  "One reply form, :TIMEOUT or :CANCELLED, or NIL on EOF or a malformed
reply. A lapsed deadline or a cancel of CANCEL kills the worker, which closes
the pipe and ends the reader. A cancel only wakes this wait, so the kill
always comes from here."
  (let ((reply nil)
        (read nil)
        (done (bt:make-semaphore))
        (stream (uiop:process-info-output (worker-process worker))))
    (bt:make-thread
     (lambda ()
       (unwind-protect
            (setf reply (handler-case (let ((*read-eval* nil))
                                        (read stream nil nil))
                          (error () nil))
                  read t)
         (bt:signal-semaphore done)))
     :name "miao-worker-reply")
    (when cancel
      (on-cancel cancel (lambda () (bt:signal-semaphore done))))
    (bt:wait-on-semaphore done :timeout (/ timeout-ms 1000))
    (cond (read reply)
          (t (kill-worker worker)
             (if (and cancel (cancelled-p cancel)) :cancelled :timeout)))))

(defun interpret-reply (reply)
  (case (and (consp reply) (first reply))
    (:ok (let ((values (second reply)))
           (ok :value (or (first values) "NIL") :values values
               :out (or (third reply) "") :elided (eq (fourth reply) :elided))))
    (:error (fail (list :error (second reply))))
    (:reader-error (bad-request "~a" (second reply)))
    (t (if (member reply '(:timeout :cancelled)) (fail reply) (fail :unavailable)))))
