(in-package #:miao)

;;; A subprocess launched in its own process group, so a deadline can signal
;;; everything it spawned rather than the direct child alone. Shared by
;;; `run-command` (tools/shell.lisp) and the worker launch/kill pair
;;; (worker.lisp) -- one helper instead of two copies of the same trick.
;;;
;;; Four strategies, tried in order and settled once at load time:
;;;
;;;   :native -- some hosts already put a launched child in its own process
;;;              group (its pgid comes back equal to its pid) with no help
;;;              needed. Nothing to prefix; TERMINATE-PROCESS-GROUP just
;;;              signals -pid.
;;;   :perl   -- macOS has no `setsid`, so a
;;;              `perl -e 'setpgrp; exec @ARGV'` wrapper sets the group on
;;;              the spawn path instead, ahead of the real command.
;;;   :setsid -- Linux's `setsid` does the same job without needing perl.
;;;              It refuses to fork a new session when the caller is
;;;              already a group leader, which is not this launch path, so
;;;              the wrapped pid stays the group id.
;;;   :tree   -- no grouping mechanism at all.
;;;
;;; TERMINATE-PROCESS-GROUP does not trust a grouping mechanism to have
;;; reached every descendant on its own: it always ALSO walks `ps`'s ppid
;;; column and kills the descendant tree by hand, on every strategy, not
;;; just :TREE. That walk is not atomic -- a child forked between it and
;;; the kill can still escape -- so the group kill above it still matters
;;; where one is available; the tree walk is insurance underneath it.

(defun unix-pgid-of (pid)
  "PID's process group id, or NIL if it cannot be read."
  (let ((out (ignore-errors
              (uiop:run-program (list "ps" "-o" "pgid=" "-p" (princ-to-string pid))
                                :output '(:string :stripped t)
                                :ignore-error-status t))))
    (and out (plusp (length out)) (parse-integer out :junk-allowed t))))

(defun on-path-p (command)
  "True when COMMAND is found via the shell's own lookup, run through
/bin/sh rather than as a literal argv so a shell builtin (Linux has no
/usr/bin/command) is tried too."
  (zerop (nth-value 2 (uiop:run-program (list "/bin/sh" "-c" (format nil "command -v ~a" command))
                                        :ignore-error-status t))))

(defun native-process-group-p ()
  "True when UIOP:LAUNCH-PROGRAM already puts a child in its own process
group with no help -- some hosts do this without asking."
  (let ((process (ignore-errors
                  (uiop:launch-program (list "/bin/sh" "-c" "sleep 2")))))
    (when process
      (unwind-protect
           (let ((pid (uiop:process-info-pid process)))
             (eql pid (unix-pgid-of pid)))
        (ignore-errors (uiop:terminate-process process :urgent t))
        (ignore-errors (uiop:wait-process process))))))

(defun detect-process-group-strategy ()
  (cond ((native-process-group-p) :native)
        ((on-path-p "perl") :perl)
        ((on-path-p "setsid") :setsid)
        (t :tree)))

(defparameter *process-group-strategy* (detect-process-group-strategy)
  "One of :NATIVE, :PERL, :SETSID or :TREE -- see the strategies above.
Resolved once, at load time; tests rebind it to force a branch.")

(defun process-group-wrapper ()
  "Argv prefix that makes a launched command lead its own process group
under the current *PROCESS-GROUP-STRATEGY*, or NIL when the strategy needs
none (:NATIVE) or none is available to wrap with (:TREE)."
  (case *process-group-strategy*
    (:perl (list "perl" "-e" "setpgrp; exec @ARGV"))
    (:setsid (list "setsid"))
    (t nil)))

(defun launch-in-process-group (argv &rest keys &key &allow-other-keys)
  "UIOP:LAUNCH-PROGRAM on ARGV, prefixed with PROCESS-GROUP-WRAPPER when
one applies, so the child leads its own process group."
  (apply #'uiop:launch-program
         (append (process-group-wrapper) argv)
         keys))

(defun descendant-pids (pid)
  "Every live descendant of PID, found by walking `ps`'s pid/ppid columns.
Collecting this before any kill matters: killing the leader first would
let a still-live child reparent and escape the walk entirely."
  (let ((rows (ignore-errors
               (uiop:run-program '("ps" "-A" "-o" "pid=,ppid=")
                                 :output :lines :ignore-error-status t))))
    (let ((by-parent (make-hash-table)))
      (dolist (row rows)
        (multiple-value-bind (child next) (parse-integer row :junk-allowed t)
          (let ((parent (and child (parse-integer row :start next :junk-allowed t))))
            (when (and child parent)
              (push child (gethash parent by-parent))))))
      (let ((seen '()))
        (labels ((walk (p)
                   (dolist (child (gethash p by-parent))
                     (unless (member child seen)
                       (push child seen)
                       (walk child)))))
          (walk pid))
        seen))))

(defun terminate-process-group (process)
  "Signal PROCESS's whole process group, walk and kill its descendant tree
by hand, and only then reap the leader with the existing leader-only
TERMINATE-PROCESS. Both the group kill and the tree walk are best-effort
and run unconditionally rather than picking one by *PROCESS-GROUP-
STRATEGY*: a grouping mechanism can fail to reach every descendant in
ways this code cannot always tell apart from the host's own quirks (a
`setsid` that forks instead of exec'ing in place, a container's PID
namespace, ...), so the tree walk is cheap insurance underneath it
rather than a fallback reserved for hosts with no grouping mechanism at
all. Descendants are collected before any kill: killing the leader first
would let a still-live child reparent and escape the walk."
  (let* ((pid (uiop:process-info-pid process))
         (descendants (descendant-pids pid)))
    (unless (eq *process-group-strategy* :tree)
      (ignore-errors
       (uiop:run-program (list "/bin/kill" "-9" (format nil "-~d" pid))
                         :ignore-error-status t)))
    (dolist (d descendants)
      (ignore-errors
       (uiop:run-program (list "/bin/kill" "-9" (princ-to-string d))
                         :ignore-error-status t))))
  (ignore-errors (uiop:terminate-process process :urgent t))
  (ignore-errors (uiop:wait-process process)))
