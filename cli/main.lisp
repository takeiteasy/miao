(in-package #:miao/cli)

;;; The command line's entry point. docs/cli.md.

(defun main (args &key context (home (miao/launcher:miao-home))
                    (in *standard-input*) (out *standard-output*) (err *error-output*))
  "Run the command ARGS names and return its exit code. CONTEXT is the
context to mount into; by default one is started and stopped here. HOME is
where init.lisp is read from and chats are saved. IN is what `chat` reads lines
from."
  (handler-case
      (cond
        ((null args) (usage-error "no command"))
        ((member (first args) '("-h" "--help") :test #'string=)
         (format out "~a~%" *usage*) 0)
        ((member (first args) '("run" "chat") :test #'string=)
         (let* ((chatting (string= (first args) "chat"))
                (options (parse-args (rest args) :takes-prompt (not chatting))))
           (load-init home)
           (flet ((command (context)
                    (if chatting
                        (chat options context in out err home)
                        (run options context out err))))
             (if context
                 (command context)
                 (let ((own (m:start-service (make-instance 'm:context :name :cli))))
                   (unwind-protect (command own)
                     (m:stop own)))))))
        ((string= (first args) "chats")
         (when (rest args) (usage-error "chats takes no arguments"))
         (list-chats home out)
         0)
        (t (usage-error "unknown command ~a" (first args))))
    (usage-error (e)
      (format err "miao: ~a~%~a~%" e *usage*)
      2)
    (error (e)
      (format err "miao: ~a~%" e)
      1)))
