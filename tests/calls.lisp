(in-package #:miao/tests)
(in-suite :miao)

;;; The call log (~takeiteasy/miao#73): the log's own accept/running/done/fold
;;; API, and AGENT's use of it through :CALL-LOG.

(defun call-statuses (path)
  (mapcar (lambda (e) (getf e :status)) (miao:call-entries path)))

(defun one-call (path)
  (first (miao:call-entries path)))

(defun accept-one (path &key (name :tool-echo))
  (first (miao::call-log-accept path :assistant 1 (list (list :id "c1" :name name
                                                              :arguments '(:text "hi"))))))

;;; --- the log API ---------------------------------------------------------

(test a-call-folds-from-accepted-through-running-to-done
  (with-vault-path (path)
    (let ((id (accept-one path)))
      (is (equal '(:accepted) (call-statuses path)))
      (miao::call-log-running path (list id))
      (is (equal '(:running) (call-statuses path)))
      (miao::call-log-done path (list (list id :ok "{\"text\":\"hi\"}")))
      (let ((call (one-call path)))
        (is (eq :ok (getf call :status)))
        (is (equal "{\"text\":\"hi\"}" (getf call :content)))
        (is (equal "c1" (getf call :call-id)))
        (is (eq :tool-echo (getf call :name)))
        (is (equal "{\"text\":\"hi\"}" (getf call :arguments)))
        (is (eql 1 (getf call :turn)))
        (is (eq :assistant (getf call :agent)))
        (is (stringp (getf call :done-at)))))))

(test a-batch-gets-a-log-id-per-call-whatever-the-provider-ids
  (with-vault-path (path)
    (let ((ids (miao::call-log-accept
                path nil 1 (list (list :id "c1" :name :tool-echo :arguments nil)
                                 (list :id "c1" :name :tool-echo :arguments nil)))))
      (is (eql 2 (length (remove-duplicates ids :test #'equal)))))))

(test a-call-keeps-its-first-outcome
  (with-vault-path (path)
    (let ((id (accept-one path)))
      (miao::call-log-done path (list (list id :ok "a")))
      (miao::call-log-done path (list (list id :abandoned nil)))
      (is (equal '(:ok) (call-statuses path))))))

(test a-call-whose-owner-is-gone-reads-as-lost
  (with-vault-path (path)
    (with-open-file (out path :direction :output :if-does-not-exist :create)
      (prin1 (list :kind :call :id "x-0" :at "2026-01-01T00:00:00Z" :agent :assistant
                   :call-id "c1" :name :tool-echo :arguments "{}" :turn 1
                   :by (list :pid 999999 :host (machine-instance) :start 1 :token "other"))
             out))
    (is (equal '(:lost) (call-statuses path)))
    (miao::call-log-done path (list (list "x-0" :abandoned nil)))
    (is (equal '(:abandoned) (call-statuses path)))))

(test compaction-drops-finished-calls-and-keeps-the-rest
  (with-vault-path (path)
    (let ((done (accept-one path))
          (open (accept-one path)))
      (miao::call-log-done path (list (list done :ok "x")))
      (multiple-value-bind (dropped kept) (miao:call-log-compact path :max-age 0)
        (is (eql 1 dropped))
        (is (eql 1 kept)))
      (is (equal (list open) (mapcar (lambda (e) (getf e :id)) (miao:call-entries path)))))))

(test compaction-keeps-recently-finished-calls
  (with-vault-path (path)
    (miao::call-log-done path (list (list (accept-one path) :ok "x")))
    (is (eql 0 (miao:call-log-compact path)))
    (is (equal '(:ok) (call-statuses path)))))

(test a-malformed-call-log-line-is-skipped-and-blocks-compaction
  (with-vault-path (path)
    (miao::call-log-done path (list (list (accept-one path) :ok "x")))
    (with-open-file (out path :direction :output :if-exists :append)
      (write-line "(:kind :call :id" out))
    (is (equal '(:ok) (call-statuses path)))
    (is (null (miao:call-log-compact path :max-age 0)))))

(test call-log-reading-never-evaluates
  (with-vault-path (path)
    (accept-one path)
    (with-open-file (out path :direction :output :if-exists :append)
      (write-line "(:kind :call :id \"e\" :name #.(error \"evaluated\"))" out))
    (is (eql 1 (length (miao:call-entries path))))))

(test an-append-compacts-once-the-log-passes-its-size
  (with-vault-path (path)
    (let ((miao:*call-log-compact-size* 1)
          (miao:*call-log-max-age* 0))
      (miao::call-log-done path (list (list (accept-one path) :ok "x")))
      (is (null (miao:call-entries path))))))

;;; --- the agent's own use of the log -----------------------------------------

(defmacro with-call-agent ((path answer &rest tools) (&rest agent-args) &body body)
  "Run BODY in a with-process with an agent, bound to CHILD, recording to PATH."
  `(with-vault-path (,path)
     (with-agent (,answer ,@tools)
       (m:with-process (runner)
         (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                  :call-log ,path ,@agent-args)))
           ,@body)))))

(defun run-child (child)
  (m:cast child (list :run :messages '((:role :user :content "go")))))

(test a-tool-call-is-recorded-accepted-and-done
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (if (= 1 (incf n))
                                 (tool-call-reply "c1" "tool-echo" "{\"text\":\"hi\"}")
                                 (final-reply "done")))
                      'tool-echo)
        (:tools '(:tool-echo))
      (run-child child)
      (is-true (m:receive :timeout 5))
      (let ((call (one-call path)))
        (is (eq :ok (getf call :status)))
        (is (equal "c1" (getf call :call-id)))
        (is (eq :tool-echo (getf call :name)))
        (is (search "hi" (getf call :content)))
        (is (search "hi" (getf call :arguments)))
        (is (eql 1 (getf call :turn)))))))

(test a-call-outside-the-allow-list-is-recorded-as-an-error
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (if (= 1 (incf n))
                                 (tool-call-reply "c1" "tool-boom" "{}")
                                 (final-reply "done")))
                      'tool-echo)
        (:tools '(:tool-echo))
      (run-child child)
      (is-true (m:receive :timeout 5))
      (is (equal '(:error) (call-statuses path))))))

(test a-failing-tool-is-recorded-as-an-error
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (if (= 1 (incf n))
                                 (tool-call-reply "c1" "tool-boom" "{}")
                                 (final-reply "done")))
                      'tool-boom)
        (:tools '(:tool-boom))
      (run-child child)
      (is-true (m:receive :timeout 5))
      (is (equal '(:error) (call-statuses path))))))

(test a-running-call-shows-as-running
  (with-call-agent (path (tool-call-reply "c1" "tool-wait" "{}") 'tool-wait)
      (:tools '(:tool-wait))
    (run-child child)
    (is-true (eventually (lambda () (equal '(:running) (call-statuses path)))))
    (m:cast child '(:cancel))
    (is-true (m:receive :timeout 5))))

(test cancel-records-a-call-in-flight-as-interrupted
  (with-call-agent (path (tool-call-reply "c1" "tool-wait" "{}") 'tool-wait)
      (:tools '(:tool-wait))
    (run-child child)
    (is-true (eventually (lambda () (equal '(:running) (call-statuses path)))))
    (m:cast child '(:cancel))
    (is-true (m:receive :timeout 5))
    (is (equal '(:interrupted) (call-statuses path)))))

(test an-agent-killed-mid-call-leaves-it-abandoned
  (progn
    (with-call-agent (path (tool-call-reply "c1" "tool-wait" "{}") 'tool-wait)
        (:tools '(:tool-wait))
      (run-child child)
      (is-true (eventually (lambda () (equal '(:running) (call-statuses path)))))
      (m:kill child)
      (is-true (eventually (lambda () (equal '(:abandoned) (call-statuses path))))))
    ;; The killed agent's tool runs on until its own 3s wait lapses; later
    ;; tests must not meet its thread.
    (is-true (eventually (lambda ()
                           (notany (lambda (thread)
                                     (equal "meow tool-wait" (bt:thread-name thread)))
                                   (bt:all-threads)))
                         6))))

(test a-queued-call-is-accepted-until-a-slot-frees
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (if (= 1 (incf n))
                                 (json-response
                                  "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\",\"function\":{\"name\":\"tool-wait\",\"arguments\":\"{}\"}},{\"id\":\"c2\",\"type\":\"function\",\"function\":{\"name\":\"tool-wait\",\"arguments\":\"{}\"}}]},\"finish_reason\":\"tool_calls\"}]}")
                                 (final-reply "done")))
                      'tool-wait)
        (:tools '(:tool-wait) :max-parallel-tools 1)
      (run-child child)
      (is-true (eventually (lambda () (equal '(:running :accepted) (call-statuses path)))))
      (m:cast child '(:cancel))
      (is-true (m:receive :timeout 5))
      (is (equal '(:interrupted :interrupted) (call-statuses path))))))

(test a-call-result-is-cut-to-max-tool-result
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (if (= 1 (incf n))
                                 (tool-call-reply "c1" "tool-echo" "{\"text\":\"abcdefghijklmnop\"}")
                                 (final-reply "done")))
                      'tool-echo)
        (:tools '(:tool-echo) :max-tool-result 5)
      (run-child child)
      (is-true (m:receive :timeout 5))
      (is (search "truncated" (getf (one-call path) :content))))))

(test without-a-call-log-nothing-is-written
  (let ((n 0))
    (with-vault-path (path)
      (with-agent ((lambda (&rest request)
                     (declare (ignore request))
                     (if (= 1 (incf n))
                         (tool-call-reply "c1" "tool-echo" "{\"text\":\"hi\"}")
                         (final-reply "done")))
                  'tool-echo)
        (m:with-process (runner)
          (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                   :tools '(:tool-echo))))
            (run-child child)
            (is-true (m:receive :timeout 5)))))
      (is (null (probe-file path))))))

(test a-sub-agents-calls-land-in-the-parents-log
  (let ((n 0))
    (with-call-agent (path (lambda (&rest request)
                             (declare (ignore request))
                             (case (incf n)
                               (1 (tool-call-reply "c1" "agent-task" "{\"task\":\"help\"}"))
                               (2 (tool-call-reply "s1" "tool-echo" "{\"text\":\"hi\"}"))
                               (t (final-reply "done"))))
                      'tool-echo)
        (:tools '(:tool-echo) :sub-agents t)
      (run-child child)
      (is-true (m:receive :timeout 5))
      (is (equal '(:agent-task :tool-echo)
                 (sort (mapcar (lambda (e) (getf e :name)) (miao:call-entries path))
                       #'string< :key #'symbol-name)))
      (is (equal '(:ok :ok) (call-statuses path))))))

(test a-restore-records-calls-in-flight-as-interrupted
  (with-call-agent (path (tool-call-reply "c1" "tool-wait" "{}") 'tool-wait)
      (:tools '(:tool-wait))
    (run-child child)
    (is-true (eventually (lambda () (equal '(:running) (call-statuses path)))))
    (m:call child (list :restore (list :messages nil :turns 0)))
    (is (equal '(:interrupted) (call-statuses path)))))

(test the-call-log-option-is-in-the-metadata
  (with-vault-path (path)
    (with-agent ((final-reply "x"))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed :call-log path)))
          (is (equal path (getf (m:call child '(:describe)) :call-log))))))))
