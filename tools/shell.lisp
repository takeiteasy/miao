(in-package #:miao)

;;; Shell tool: one command through `sh -c`, merged output, bounded by a
;;; caller deadline and its cancel token. Either kills the command's whole
;;; process group (see process.lisp), so a backgrounded descendant is
;;; signalled too.
;;;
;;; Trust posture: arbitrary command execution. Trusted operator only.

(define-tool :tool-shell
    (:trust :operator
     :summary "Run a shell command (sh -c) and capture merged output"
     :params ((:cmd string :required t :doc "command string to run")
              (:timeout (integer 1) :default +default-tool-timeout+
               :doc "kill the command after this many milliseconds")))
  (:invoke (cmd timeout)
    (run-command cmd timeout cancel-token)))

(defvar *live-commands* '()
  "Processes of commands still running, so RELAUNCH can kill them before it
replaces the process.")

(defvar *live-commands-lock* (bt:make-lock :name "miao-live-commands"))

(defun kill-live-commands ()
  "Kill every running command's process group. Each RUN-COMMAND unregisters
its own entry as it unwinds."
  (dolist (process (bt:with-lock-held (*live-commands-lock*) (copy-list *live-commands*)))
    (terminate-process-group process)))

(defun run-command (cmd timeout-ms &optional cancel)
  (let ((process (launch-in-process-group (list "/bin/sh" "-c" cmd)
                                          :output :stream
                                          :error-output :output)))
    (bt:with-lock-held (*live-commands-lock*) (push process *live-commands*))
    (unwind-protect (await-command process timeout-ms cancel)
      (bt:with-lock-held (*live-commands-lock*)
        (setf *live-commands* (remove process *live-commands*))))))

(defun await-command (process timeout-ms cancel)
  "Wait for PROCESS, killing its group at the deadline or when CANCEL, a
cancel token or NIL, is cancelled. A cancel only wakes this wait, so the kill
always comes from here and never after the process is reaped."
  (let ((done (bt:make-semaphore))
        (output nil)
        (drained nil))
    ;; Drain concurrently: a command that outruns the pipe buffer would
    ;; otherwise block on write while we block waiting for it to exit.
    (spawn-thread
     (lambda ()
       (unwind-protect
            ;; A timeout's TERMINATE-PROCESS-GROUP reaps the process from
            ;; the main thread, which can close this stream while a read
            ;; here is still blocked on it -- a race, not an EOF, and it
            ;; surfaces as a stream error rather than a clean end of file.
            ;; Losing the last fragment of output to it is fine: the call
            ;; is about to fail with :TIMEOUT anyway.
            (setf output (handler-case
                             (uiop:slurp-stream-string
                              (uiop:process-info-output process))
                           (stream-error () output))
                  drained t)
         (bt:signal-semaphore done)))
     :name "miao-shell-drain")
    (when cancel
      (on-cancel cancel (lambda () (bt:signal-semaphore done))))
    (bt:wait-on-semaphore done :timeout (/ timeout-ms 1000))
    (if drained
        (ok :exit (uiop:wait-process process) :out (or output ""))
        (progn
          ;; Killing the whole group closes the pipe, which ends the drain
          ;; thread on its own -- true even when a backgrounded descendant
          ;; held the write end open, since it dies with the group too.
          (terminate-process-group process)
          (fail (if (and cancel (cancelled-p cancel)) :cancelled :timeout))))))
