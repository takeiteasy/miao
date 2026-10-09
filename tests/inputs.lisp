(in-package #:miao/tests)
(in-suite :miao)

;;; Idempotent input redelivery (~takeiteasy/miao#75): a :RUN or :STEER a caller
;;; keys with :INPUT-ID is accepted once, and a redelivery answers with the
;;; original's status. Replies are read through M:CALL.

(defun input-statuses (path)
  (mapcar (lambda (e) (getf e :status)) (miao:input-entries path)))

(defun keyed-run (child input-id &optional (content "go"))
  (m:call child (list :run :messages (list (list :role :user :content content))
                           :input-id input-id)))

(defun keyed-run-named (name input-id &optional (content "go"))
  "As KEYED-RUN against the agent mounted as NAME, tried again until it answers."
  (loop repeat 60
        for reply = (ignore-errors
                     (m:call (m:lookup name)
                             (list :run :messages (list (list :role :user :content content))
                                        :input-id input-id)))
        when reply return reply
        do (sleep 0.05)))

(defun wait-for-input-status (path status &optional (deadline 3.0))
  (loop repeat (ceiling deadline 0.05)
        when (equal (list status) (input-statuses path)) return t
        do (sleep 0.05)))

;;; --- the log API ---------------------------------------------------------

(test a-keyed-input-is-fresh-then-a-duplicate
  (with-vault-path (path)
    (multiple-value-bind (id duplicate) (miao::journal-input path :assistant "k" "d1")
      (is (stringp id))
      (is (null duplicate))
      (multiple-value-bind (again duplicate status digest)
          (miao::journal-input path :assistant "k" "d1")
        (is (equal id again))
        (is (eq :duplicate duplicate))
        (is (eq :running status))
        (is (equal "d1" digest))))
    (is (eql 1 (length (miao:input-entries path))))))

(test a-finished-input-reports-its-outcome
  (with-vault-path (path)
    (let ((id (miao::journal-input path :assistant "k" "d1")))
      (miao::journal-call-done path (list (list id :stop nil)))
      (is (equal '(:stop) (input-statuses path)))
      (is (eq :stop (nth-value 2 (miao::journal-input path :assistant "k" "d1")))))))

(test an-input-whose-owner-is-gone-reads-as-lost
  (with-vault-path (path)
    (with-open-file (out path :direction :output :if-does-not-exist :create)
      (prin1 (list :kind :input :id "x-in" :at "2026-01-01T00:00:00Z" :agent :assistant
                   :input-id "k" :digest "d1"
                   :by (list :pid 999999 :host (machine-instance) :start 1 :token "other"))
             out))
    (is (eq :lost (nth-value 2 (miao::journal-input path :assistant "k" "d1"))))))

(test an-unrecorded-input-appends-nothing
  (with-vault-path (path)
    (is (null (miao::journal-input path :assistant "k" "d1" :record nil)))
    (is (null (miao:input-entries path)))))

(test call-entries-leaves-inputs-out
  (with-vault-path (path)
    (miao::journal-input path :assistant "k" "d1")
    (is (null (miao:call-entries path)))))

(test compaction-drops-a-finished-input-and-keeps-an-open-one
  (with-vault-path (path)
    (let ((done (miao::journal-input path :assistant "a" "d"))
          (open (miao::journal-input path :assistant "b" "d")))
      (miao::journal-call-done path (list (list done :stop nil)))
      (miao:journal-compact path :max-age 0)
      (is (equal (list open) (mapcar (lambda (e) (getf e :id)) (miao:input-entries path)))))))

(test concurrent-inputs-of-one-id-are-accepted-once
  (with-vault-path (path)
    (let* ((fresh 0)
           (rlock (bt:make-lock))
           (threads (loop repeat 8
                          collect (bt:make-thread
                                   (lambda ()
                                     (unless (nth-value 1 (miao::journal-input
                                                           path :assistant "k" "d"))
                                       (bt:with-lock-held (rlock) (incf fresh))))))))
      (mapc #'bt:join-thread threads)
      (is (eql 1 fresh))
      (is (eql 1 (length (miao:input-entries path)))))))

(test a-keyed-steer-is-recorded-once
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "a" :input-id "k")))
      (multiple-value-bind (again duplicate status content)
          (miao:vault-record path :assistant "a" :input-id "k")
        (is (equal id again))
        (is (eq :duplicate duplicate))
        (is (eq :pending status))
        (is (equal "a" content)))
      (miao:vault-consume path id :folded)
      (is (eq :folded (nth-value 2 (miao:vault-record path :assistant "a" :input-id "k"))))
      (is (eql 1 (length (miao:vault-entries path))))
      (is (equal "k" (getf (first (miao:vault-entries path)) :input-id))))))

;;; --- the agent ---------------------------------------------------------------

(test a-redelivered-run-answers-the-original-and-does-not-run-again
  (let ((n 0))
    (with-vault-path (path)
      (with-agent ((lambda (&rest request) (declare (ignore request)) (incf n) (final-reply "done")))
        (m:mount *ctx* 'miao:agent :name :assistant :model :test-keyed :journal path)
        (is (eq :ok (keyed-run-named :assistant "k")))
        (is-true (wait-for-input-status path :stop))
        (is (equal '(:ok (:duplicate :stop)) (keyed-run-named :assistant "k")))
        (is (eql 1 n))
        (is (eql 1 (length (miao:input-entries path))))))))

(test a-redelivered-run-mid-run-answers-running
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (if (= 1 (incf n))
                                 (tool-call-reply "c1" "tool-gate" "{}")
                                 (final-reply "done")))
                      'tool-gate)
        (:tools '(:tool-gate))
      (is (eq :ok (keyed-run child "k")))
      (is (equal '(:ok (:duplicate :running)) (keyed-run child "k")))
      (is-true (m:receive :timeout 5))
      (is (equal '(:stop) (input-statuses path))))))

(test a-run-keyed-with-other-messages-is-a-bad-request
  (with-vault-path (path)
    (with-agent ((lambda (&rest request) (declare (ignore request)) (final-reply "done")))
      (m:mount *ctx* 'miao:agent :name :assistant :model :test-keyed :journal path)
      (keyed-run-named :assistant "k")
      (is-true (wait-for-input-status path :stop))
      (is (eq :bad-request
              (first (miao:result-error (keyed-run-named :assistant "k" "something else"))))))))

(test a-keyed-run-with-no-call-log-is-a-bad-request
  (with-agent ((lambda (&rest request) (declare (ignore request)) (final-reply "done")))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed)))
        (is (eq :bad-request (first (miao:result-error (keyed-run child "k")))))))))

(test a-fresh-keyed-run-on-a-busy-agent-is-not-recorded
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (if (= 1 (incf n))
                                 (tool-call-reply "c1" "tool-gate" "{}")
                                 (final-reply "done")))
                      'tool-gate)
        (:tools '(:tool-gate))
      (keyed-run child "a")
      (is (eq :bad-request (first (miao:result-error (keyed-run child "b")))))
      (is (eql 1 (length (miao:input-entries path))))
      (is-true (m:receive :timeout 5)))))

(test a-cancelled-input-is-finished
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (incf n)
                             (tool-call-reply "c1" "tool-gate" "{}"))
                      'tool-gate)
        (:tools '(:tool-gate))
      (keyed-run child "k")
      (m:cast child '(:cancel))
      (is-true (m:receive :timeout 5))
      (is (equal '(:cancelled) (input-statuses path))))))

(test run-agent-answers-a-redelivery-without-running
  (let ((n 0))
    (with-vault-path (path)
      (with-agent ((lambda (&rest request) (declare (ignore request)) (incf n) (final-reply "done")))
        (flet ((go-run ()
                 (miao:run-agent *ctx* :model :test-keyed :journal path
                                       :input-id "k"
                                       :messages '((:role :user :content "go")))))
          (is (eq :ok (first (go-run))))
          (is (equal '(:ok (:duplicate :stop)) (go-run)))
          (is (eql 1 n)))))))

(test a-redelivered-steer-is-queued-once
  (with-vault-path (path)
    (with-agent ((lambda (&rest request) (declare (ignore request)) (final-reply "done")))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed :vault path)))
          (is (eq :ok (m:call child '(:steer :content "x" :input-id "k"))))
          (is (equal '(:ok (:duplicate :pending)) (m:call child '(:steer :content "x" :input-id "k"))))
          (is (eq :bad-request (first (miao:result-error
                                       (m:call child '(:steer :content "y" :input-id "k"))))))
          (is (eql 1 (length (miao:vault-entries path)))))))))

(test a-keyed-steer-with-no-vault-is-a-bad-request
  (with-agent ((lambda (&rest request) (declare (ignore request)) (final-reply "done")))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed)))
        (is (eq :bad-request
                (first (miao:result-error (m:call child '(:steer :content "x" :input-id "k"))))))))))
