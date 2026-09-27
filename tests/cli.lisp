(in-package #:miao/tests)
(in-suite :miao)

;;; `miao run` through MIAO/CLI:MAIN against the echo provider, which
;;; answers with the last user message and a bang.

(defvar *home* nil "The MIAO_HOME a test's command line runs against, when it needs to keep it.")

(defun call-with-home (body)
  (let ((*home* (uiop:ensure-directory-pathname (make-sandbox-directory))))
    (unwind-protect (funcall body)
      (uiop:delete-directory-tree *home* :validate t :if-does-not-exist :ignore))))

(defmacro with-home (() &body body)
  `(call-with-home (lambda () ,@body)))

(defun cli (&rest args)
  "ARGS through MAIN, as (values exit-code stdout stderr). Against *HOME*, or an
empty home of its own."
  (flet ((in-home (home)
           (let* ((out (make-string-output-stream))
                  (err (make-string-output-stream))
                  (code (miao/cli:main args :context *protocol-context* :home home :out out :err err)))
             (values code (get-output-stream-string out) (get-output-stream-string err)))))
    (if *home*
        (in-home *home*)
        (with-home () (in-home *home*)))))

(test run-prints-the-answer-and-exits-zero
  (with-protocol
    (multiple-value-bind (code out) (cli "run" "hi" "--model" "test-echo:any")
      (is (= 0 code))
      (is (equal (format nil "hi!~%") out)))))

(test run-splits-the-model-on-the-first-colon
  (with-protocol
    (is (= 0 (cli "run" "hi" "--model" "test-echo:name:with:colons")))))

(test max-turns-reaches-the-agent
  (is (= 3 (getf (miao/cli::parse-args '("a" "--max-turns" "3")) :max-turns)))
  (is (null (getf (miao/cli::parse-args '("a")) :max-turns)))
  (with-protocol
    (is (= 0 (cli "run" "hi" "--model" "test-echo:x" "--max-turns" "1")))))

(test verbose-streams-events-to-stderr
  (with-protocol
    (multiple-value-bind (code out err) (cli "run" "hi" "--model" "test-echo:x" "-v")
      (declare (ignore out))
      (is (= 0 code))
      (is (search "hi!" err))
      (is (search "[done stop]" err)))))

(test a-system-file-is-appended-or-replaces
  (with-miao-home (home)
    (let ((file (merge-pathnames "system.txt" home)))
      (alexandria:write-string-into-file "Be terse." file)
      (is (search "Be terse." (miao/cli::system-prompt (namestring file) nil)))
      (is (search "miao" (miao/cli::system-prompt (namestring file) nil)))
      (is (equal "Be terse." (miao/cli::system-prompt (namestring file) t))))))

(test bad-usage-exits-two
  (with-protocol
    (dolist (args '(() ("frobnicate") ("run") ("run" "a" "b") ("run" "a" "--bogus")
                    ("run" "a" "--model") ("run" "a" "--model" "nocolon")
                    ("run" "a" "--model" "nobody:x") ("run" "a" "--system-replace")
                    ("run" "a" "--model" "test-echo:x" "--tools" "tool-nobody")
                    ("run" "a" "--system-file" "/no/such/file")
                    ("run" "a" "--max-turns" "0") ("run" "a" "--max-turns" "many")))
      (multiple-value-bind (code out err) (apply #'cli args)
        (is (= 2 code) "~s exited ~a" args code)
        (is (equal "" out))
        (is (search "usage:" err))))))

(test a-provider-whose-protocol-is-missing-is-a-run-error
  (with-protocol
    (multiple-value-bind (code out err) (cli "run" "hi" "--model" "test-orphan:x")
      (is (= 1 code))
      (is (equal "" out))
      (is (search "miao:" err)))))

(test the-exit-code-follows-the-stop-reason
  (is (= 0 (miao/cli:exit-code '(:ok (:stop-reason :stop)))))
  (is (= 3 (miao/cli:exit-code '(:ok (:stop-reason :max-turns)))))
  (is (= 3 (miao/cli:exit-code '(:ok (:stop-reason :timeout)))))
  (is (= 1 (miao/cli:exit-code '(:ok (:stop-reason :cancelled)))))
  (is (= 1 (miao/cli:exit-code '(:error :unavailable)))))

(test a-truncated-run-prints-partial-text-and-the-reason
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (miao/cli::report '(:ok (:content "partial" :stop-reason :max-turns)) out err)
    (is (equal (format nil "partial~%") (get-output-stream-string out)))
    (is (search "max-turns" (get-output-stream-string err)))))

(test init-lisp-is-loaded-before-the-model-is-resolved
  (with-miao-home (home)
    (alexandria:write-string-into-file
     "(miao:define-provider :test-from-init :protocol :protocol-echo :base-url \"http://127.0.0.1:1\")"
     (merge-pathnames "init.lisp" home))
    (with-protocol
      (let* ((out (make-string-output-stream))
             (code (miao/cli:main '("run" "hi" "--model" "test-from-init:x")
                                  :context *protocol-context* :home home :out out)))
        (is (= 0 code))
        (is (equal (format nil "hi!~%") (get-output-stream-string out)))))))
