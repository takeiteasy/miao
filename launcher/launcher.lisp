(in-package #:miao/launcher)

;;; Choosing and starting a core, shared by miao.ros and RELAUNCH.
;;; Depends on nothing but uiop, so the launcher starts without loading miao.

(defun miao-home ()
  "$MIAO_HOME, default ~/.miao/."
  (uiop:ensure-directory-pathname
   (or (uiop:getenv-pathname "MIAO_HOME")
       (merge-pathnames ".miao/" (user-homedir-pathname)))))

(defun generations-directory (&optional (home (miao-home)))
  (merge-pathnames "generations/" home))

(defun recovery-core (&optional (home (miao-home)))
  (merge-pathnames "images/recovery.core" home))

(defun probe-core (core)
  "T if CORE loads and its toplevel runs cleanly under MIAO_IMAGE_PROBE, in a
throwaway subprocess of this runtime."
  (zerop (nth-value 2
          (uiop:run-program
           (list (namestring sb-ext:*runtime-pathname*) "--core" (namestring core)
                 "--noinform" "--non-interactive")
           :environment (list* "MIAO_IMAGE_PROBE=1" (sb-ext:posix-environ))
           :ignore-error-status t))))

(defun %newest-generation (home)
  (let ((cores (directory (merge-pathnames "*.core" (generations-directory home)))))
    (first (sort cores #'> :key #'file-write-date))))

(defun select-core (&key (home (miao-home)) core (probe #'probe-core))
  "The core to launch: CORE, else the newest generation, else the recovery
image. A core PROBE rejects falls back to the recovery image too, with a
warning on stderr. Signals for a CORE that does not exist and when there is
nothing to fall back to."
  (let ((recovery (recovery-core home)))
    (flet ((usable-recovery ()
             (or (probe-file recovery)
                 (error "no core to run -- try `miao install` first"))))
      (when (and core (not (probe-file core)))
        (error "no such core: ~a" core))
      (let ((candidate (or (and core (probe-file core)) (%newest-generation home))))
        (cond ((null candidate) (usable-recovery))
              ((funcall probe candidate) candidate)
              (t (format *error-output* "miao: ~a did not load cleanly, falling back to the recovery image~%"
                         candidate)
                 (usable-recovery)))))))

(defun launch-argv (core args)
  "The argv that runs CORE with ARGS, in the runtime that is running now."
  (list* (namestring sb-ext:*runtime-pathname*) "--core" (namestring core) args))
