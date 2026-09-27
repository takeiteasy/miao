(in-package #:miao/tests)
(in-suite :miao)

;;; Core selection and the launcher script (docs/launcher.md). A stand-in
;;; core is any file; the probe is injected, so no SBCL starts except in the
;;; one test that runs the real script.

(defun call-with-miao-home (function)
  (let ((home (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "miao-home-~36r/" (random (expt 36 8)))
                                (uiop:temporary-directory)))))
    (ensure-directories-exist (merge-pathnames "generations/" home))
    (ensure-directories-exist (merge-pathnames "images/" home))
    (unwind-protect (funcall function home)
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(defmacro with-miao-home ((home) &body body)
  `(call-with-miao-home (lambda (,home) ,@body)))

(defun touch-core (path &optional age)
  "A stand-in core at PATH, AGE seconds in the past."
  (ensure-directories-exist path)
  (alexandria:write-string-into-file "core" path :if-exists :supersede)
  (when age
    (let ((then (- (get-universal-time) age 2208988800)))
      (sb-posix:utimes (namestring path) then then)))
  path)

(defun accepting (core) (declare (ignore core)) t)
(defun rejecting (core) (declare (ignore core)) nil)

(defun select (home &rest args)
  (apply #'miao/launcher:select-core :home home args))

(test the-newest-generation-is-selected
  (with-miao-home (home)
    (touch-core (merge-pathnames "generations/old.core" home) 100)
    (let ((new (touch-core (merge-pathnames "generations/new.core" home) 10)))
      (is (equal (truename new) (truename (select home :probe #'accepting)))))))

(test an-explicit-core-beats-a-newer-generation
  (with-miao-home (home)
    (touch-core (merge-pathnames "generations/new.core" home))
    (let ((given (touch-core (merge-pathnames "given.core" home) 100)))
      (is (equal (truename given) (truename (select home :core given :probe #'accepting)))))))

(test a-missing-explicit-core-is-an-error
  (with-miao-home (home)
    (signals error (select home :core (merge-pathnames "nope.core" home) :probe #'accepting))))

(test a-core-that-fails-its-probe-falls-back-to-recovery
  (with-miao-home (home)
    (touch-core (merge-pathnames "generations/bad.core" home))
    (let ((recovery (touch-core (miao/launcher:recovery-core home))))
      (let ((warning (with-output-to-string (*error-output*)
                       (is (equal (truename recovery)
                                  (truename (select home :probe #'rejecting)))))))
        (is (search "falling back to the recovery image" warning))))))

(test no-generation-selects-recovery-without-probing
  (with-miao-home (home)
    (let ((recovery (touch-core (miao/launcher:recovery-core home))))
      (is (equal (truename recovery)
                 (truename (select home :probe (lambda (core) (declare (ignore core)) (error "probed")))))))))

(test nothing-to-run-points-at-install
  (with-miao-home (home)
    (handler-case (select home :probe #'accepting)
      (error (e) (is (search "miao install" (princ-to-string e)))))
    (touch-core (merge-pathnames "generations/bad.core" home))
    (signals error (with-output-to-string (*error-output*)
                     (select home :probe #'rejecting)))))

(test launch-argv-runs-the-core-in-this-runtime
  (is (equal (list (namestring sb-ext:*runtime-pathname*) "--core" "/x.core" "--eval" "1")
             (miao/launcher:launch-argv "/x.core" '("--eval" "1")))))

(test the-real-probe-rejects-a-file-that-is-not-a-core
  (with-miao-home (home)
    (is-false (miao/launcher:probe-core (touch-core (merge-pathnames "junk.core" home))))))

(test the-script-reports-a-bad-core-and-a-missing-recovery
  (if (zerop (nth-value 2 (uiop:run-program '("sh" "-c" "command -v ros") :ignore-error-status t)))
      (with-miao-home (home)
        (let ((junk (touch-core (merge-pathnames "junk.core" home))))
          (multiple-value-bind (out err code)
              (uiop:run-program
               (list "ros" (namestring (merge-pathnames "miao.ros" (asdf:system-source-directory :miao)))
                     "--core" (namestring junk))
               :environment (list* (format nil "MIAO_HOME=~a" (namestring home)) (sb-ext:posix-environ))
               :output :string :error-output :string :ignore-error-status t)
            (declare (ignore out))
            (is (= 1 code))
            (is (search "did not load cleanly" err))
            (is (search "miao install" err)))))
      (skip "ros is not installed")))

(test the-script-installs-a-recovery-image-and-runs-the-cli-from-it
  (if (and (zerop (nth-value 2 (uiop:run-program '("sh" "-c" "command -v ros") :ignore-error-status t)))
           (probe-file (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
      (with-miao-home (home)
        (flet ((miao (&rest args)
                 (uiop:run-program
                  (list* "ros" (namestring (merge-pathnames "miao.ros" (asdf:system-source-directory :miao)))
                         args)
                  :environment (list* (format nil "MIAO_HOME=~a" (namestring home)) (sb-ext:posix-environ))
                  :output :string :error-output :string :ignore-error-status t)))
          (multiple-value-bind (out err code) (miao "install")
            (declare (ignore out))
            (is (= 0 code) "install: ~a" err))
          (is-true (probe-file (miao/launcher:recovery-core home)))
          (multiple-value-bind (out err code) (miao "run" "hi" "--model" "nobody:x")
            (declare (ignore out))
            (is (= 2 code) "run: ~a" err)
            (is (search "no provider" err)))
          (multiple-value-bind (out err code) (miao "chat" "--model" "nobody:x")
            (declare (ignore out))
            (is (= 2 code) "chat: ~a" err)
            (is (search "no provider" err)))))
      (skip "needs ros and Quicklisp")))
