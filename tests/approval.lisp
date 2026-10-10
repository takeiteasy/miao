(in-package #:miao/tests)
(in-suite :miao)

;;; The operator approval hook: what it holds, how it is
;;; answered, and that it never outlives the agent's wait.

(m:defservice tool-op () () (:name :tool-op))

(defmethod m:metadata ((service tool-op))
  (list :kind :tool :name :tool-op :trust :operator
        :summary "An operator tool: echo TEXT back"
        :params '((:text string :required t :doc "text to echo"))))

(miao::define-tool-handler tool-op (service args)
  (miao::ok :text (getf args :text)))

(defun op-call (id) (sse-tool-call id "tool-op" "{\"text\":\"ran\"}"))
(defun echo-call (id) (sse-tool-call id "tool-echo" "{\"text\":\"ran\"}"))
(defun done-reply () (streamed-reply "done"))

(defun call-with-approval-agent (replies hook-options agent-options body)
  "Mount :HOOK-APPROVAL with HOOK-OPTIONS and an agent gated by it, with
AGENT-OPTIONS, behind a model that answers REPLIES, then call BODY with the agent
and a recorder on its events."
  (let ((recorder (make-recorder)))
    (call-with-agent (apply #'scripted replies) '(tool-op tool-echo)
                     (lambda (*ctx*)
                       (apply #'m:mount *ctx* 'miao:hook-approval hook-options)
                       (let ((agent (apply #'m:mount *ctx* 'miao:agent :name :gated
                                           :model :test-keyed
                                           :tools '(:tool-op :tool-echo)
                                           :hooks '(:hook-approval)
                                           :sink (recorder-sink recorder)
                                           agent-options)))
                         (funcall body agent recorder))))))

(defmacro with-approval-agent ((agent recorder replies &key hook-options agent-options) &body body)
  `(call-with-approval-agent ,replies ,hook-options ,agent-options
                             (lambda (,agent ,recorder) (declare (ignorable ,agent ,recorder)) ,@body)))

(defun run-gated (agent &optional (text "go"))
  (m:cast agent (list :run :messages (list (list :role :user :content text)))))

(defun pending-approvals ()
  (m:call (m:lookup :hook-approval) '(:pending)))

(defun wait-for-pending (n)
  (is-true (eventually (lambda () (= n (length (pending-approvals)))) 3)))

(defun wait-for-run-done (recorder)
  (is-true (eventually (lambda () (recorder-has recorder :run-done)) 5)))

(defun events-of (recorder &rest types)
  (remove-if-not (lambda (event) (member (getf event :type) types)) (recorded-events recorder)))

(defun approval-event-types (recorder &rest types)
  (mapcar (lambda (event) (getf event :type)) (apply #'events-of recorder types)))

(defun tool-result-of (recorder id)
  (getf (find-if (lambda (event) (equal id (getf event :id)))
                 (events-of recorder :tool-result))
        :result))

(defun answer (decision &optional (n 0))
  (let ((approval (getf (nth n (pending-approvals)) :approval)))
    (miao:answer-approval approval decision)))

;;; --- what is asked ---------------------------------------------------------------

(test a-tool-that-is-not-an-operator-tool-runs-without-asking
  (with-approval-agent (agent recorder (list (echo-call "c1") (done-reply)))
    (run-gated agent)
    (wait-for-run-done recorder)
    (is (null (events-of recorder :approval-request)))
    (is (equal '(:ok (:text "ran")) (tool-result-of recorder "c1")))))

(test an-operator-tool-waits-for-the-operator-and-runs-once-allowed
  (with-approval-agent (agent recorder (list (op-call "c1") (done-reply)))
    (run-gated agent)
    (wait-for-pending 1)
    (is (null (events-of recorder :tool-call)))
    (let ((pending (first (pending-approvals))))
      (is (eq :tool-op (getf pending :name)))
      (is (equal "c1" (getf pending :id)))
      (is (eq :ok (answer :allow))))
    (wait-for-run-done recorder)
    (is (equal '(:approval-request :approval-done :tool-call)
               (approval-event-types recorder :approval-request :approval-done :tool-call)))
    (let ((request (first (events-of recorder :approval-request))))
      (is (equal "c1" (getf request :id)))
      (is (eq :tool-op (getf request :name)))
      (is (eq :hook-approval (getf request :hook)))
      (is (eq :gated (getf request :agent))))
    (is (eq :allow (getf (first (events-of recorder :approval-done)) :answer)))
    (is (equal '(:ok (:text "ran")) (tool-result-of recorder "c1")))))

(test a-denial-reaches-the-model-as-a-result-error
  (with-approval-agent (agent recorder (list (op-call "c1") (done-reply)))
    (run-gated agent)
    (wait-for-pending 1)
    (answer :deny)
    (wait-for-run-done recorder)
    (is (equal '(:error (:denied :hook-approval "denied by the operator"))
               (tool-result-of recorder "c1")))))

(test an-approval-that-is-not-waiting-or-not-a-decision-is-a-bad-request
  (with-approval-agent (agent recorder (list (op-call "c1") (done-reply)))
    (is (eq :bad-request (first (second (miao:answer-approval 99 :allow)))))
    (run-gated agent)
    (wait-for-pending 1)
    (is (eq :bad-request (first (second (answer :maybe)))))
    (is (= 1 (length (pending-approvals))))
    (answer :allow)
    (wait-for-run-done recorder)
    (is (eq :bad-request (first (second (miao:answer-approval 1 :allow)))))))

(test always-approves-the-tool-for-the-rest-of-the-run-only
  (with-approval-agent (agent recorder (list (op-call "c1") (op-call "c2") (done-reply)
                                             (op-call "c3") (done-reply)))
    (run-gated agent)
    (wait-for-pending 1)
    (answer :always)
    (wait-for-run-done recorder)
    (is (= 1 (length (events-of recorder :approval-request))))
    (is (equal '(:ok (:text "ran")) (tool-result-of recorder "c2")))
    (m:cast agent (list :run :continue t :messages '((:role :user :content "again"))))
    (wait-for-pending 1)
    (answer :allow)
    (is-true (eventually (lambda () (= 2 (length (events-of recorder :run-done)))) 5))
    (is (= 2 (length (events-of recorder :approval-request))))))

(defun approved-always-count ()
  (hash-table-count (miao::%approved-always (m:service-of (m:lookup :hook-approval)))))

(test an-always-entry-is-dropped-when-its-run-ends
  (with-approval-agent (agent recorder (list (op-call "c1") (done-reply)))
    (run-gated agent)
    (wait-for-pending 1)
    (answer :always)
    (wait-for-run-done recorder)
    (is-true (eventually (lambda () (zerop (approved-always-count))) 3))))

;; The handle is made on a thread that ends: a stale slot on the test thread's
;; stack, which the conservative GC scans, would keep it alive.
(defun add-orphan-approval (table)
  (bt:join-thread
   (bt:make-thread (lambda ()
                     (setf (gethash (miao::make-run-handle nil) table) '(:tool-op))))))

(test an-always-entry-does-not-keep-its-run-handle-alive
  (with-approval-agent (agent recorder nil)
    (let ((table (miao::%approved-always (m:service-of (m:lookup :hook-approval)))))
      (add-orphan-approval table)
      (is (= 1 (hash-table-count table)))
      (is-true (eventually (lambda () (sb-ext:gc :full t) (zerop (hash-table-count table))) 3)))))

(test an-explicit-tools-option-replaces-the-trust-rule
  (with-approval-agent (agent recorder (list (echo-call "c1") (op-call "c2") (done-reply))
                        :hook-options '(:tools (:tool-echo)))
    (run-gated agent)
    (wait-for-pending 1)
    (is (eq :tool-echo (getf (first (pending-approvals)) :name)))
    (answer :allow)
    (wait-for-run-done recorder)
    (is (= 1 (length (events-of recorder :approval-request))))
    (is (equal '(:ok (:text "ran")) (tool-result-of recorder "c2")))))

(defun two-op-calls ()
  (flet ((delta (index id)
           (format nil "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":~d,\"id\":\"~a\",~
                        \"function\":{\"name\":\"tool-op\",\"arguments\":\"{\\\"text\\\":\\\"x\\\"}\"}}]}}]}"
                   index id)))
    (sse-response (delta 0 "c1") (delta 1 "c2")
                  "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
                  "[DONE]")))

(test parallel-calls-wait-together-and-are-answered-in-any-order
  (with-approval-agent (agent recorder (list (two-op-calls) (done-reply)))
    (run-gated agent)
    (wait-for-pending 2)
    (answer :deny 0)
    (answer :allow 0)
    (wait-for-run-done recorder)
    (let ((results (list (tool-result-of recorder "c1") (tool-result-of recorder "c2"))))
      (is (= 1 (count :error results :key #'first)))
      (is (= 1 (count :ok results :key #'first))))))

;;; --- when the agent stops waiting ------------------------------------------------

(test a-cancelled-run-leaves-nothing-pending-and-no-event-after-run-done
  (with-approval-agent (agent recorder (list (op-call "c1") (done-reply)))
    (run-gated agent)
    (wait-for-pending 1)
    (m:cast agent '(:cancel))
    (wait-for-run-done recorder)
    (is-true (eventually (lambda () (null (pending-approvals)))))
    (sleep 0.2)
    (let ((events (recorded-events recorder)))
      (is (eq :run-done (getf (car (last events)) :type))))))

(test an-interrupting-steer-withdraws-the-approval-and-the-run-goes-on
  (with-approval-agent (agent recorder (list (op-call "c1") (done-reply)))
    (run-gated agent)
    (wait-for-pending 1)
    (m:cast agent (list :steer :content "never mind" :interrupt t))
    (wait-for-run-done recorder)
    (is (eq :withdrawn (getf (first (events-of recorder :approval-done)) :answer)))
    (is (null (pending-approvals)))
    (is (eq :stop (getf (first (events-of recorder :run-done)) :reason)))))

(test a-hook-timeout-fails-the-call-closed-and-withdraws-the-approval
  (with-approval-agent (agent recorder (list (op-call "c1") (done-reply))
                        :hook-options '(:timeout 300))
    (run-gated agent)
    (wait-for-run-done recorder)
    (is (equal '(:error (:hook-failed :hook-approval :timeout)) (tool-result-of recorder "c1")))
    (is-true (eventually (lambda () (null (pending-approvals)))))
    (is (eq :withdrawn (getf (first (events-of recorder :approval-done)) :answer)))))

;;; --- sub-agents ------------------------------------------------------------------

(defun task-call (id) (sse-tool-call id "agent-task" "{\"task\":\"help\"}"))

(test a-sub-agents-operator-call-asks-with-its-parent-named
  (with-approval-agent (agent recorder (list (task-call "c1") (op-call "k1") (done-reply)
                                             (done-reply))
                        :agent-options '(:sub-agents t))
    (run-gated agent)
    (wait-for-pending 1)
    (answer :allow)
    (wait-for-run-done recorder)
    (let ((request (first (events-of recorder :approval-request))))
      (is (eq :gated (getf request :parent)))
      (is (equal "k1" (getf request :id))))))

(test always-given-to-the-root-covers-its-sub-agents
  (with-approval-agent (agent recorder (list (op-call "c1") (task-call "c2") (op-call "k1")
                                             (done-reply) (done-reply))
                        :agent-options '(:sub-agents t))
    (run-gated agent)
    (wait-for-pending 1)
    (answer :always)
    (wait-for-run-done recorder)
    (is (= 1 (length (events-of recorder :approval-request))))
    (is (equal '(:ok (:text "ran")) (tool-result-of recorder "k1")))))

(test a-sub-agents-approval-is-withdrawn-when-the-root-steers-it-away
  (with-approval-agent (agent recorder (list (task-call "c1") (op-call "k1") (done-reply))
                        :agent-options '(:sub-agents t))
    (run-gated agent)
    (wait-for-pending 1)
    (m:cast agent (list :steer :content "never mind" :interrupt t))
    (wait-for-run-done recorder)
    (is (eq :withdrawn (getf (first (events-of recorder :approval-done)) :answer)))
    (is (null (pending-approvals)))))

;;; --- what a hook may emit --------------------------------------------------------

(defvar *forge-run* nil)

(miao:define-hook :hook-forge (:phases (:before-tool-call))
  (:intercept (phase request)
    (funcall (getf request :emit) '(:type :run-done :reason :forged))
    (funcall (getf request :emit) '(:type :custom :note "kept"))
    (setf *forge-run* (getf request :run))
    :pass))

(test a-hook-cannot-forge-the-loops-own-events
  (let ((recorder (make-recorder)))
    (setf *forge-run* nil)
    (call-with-agent (scripted (echo-call "c1") (done-reply)) '(tool-echo hook-forge)
                     (lambda (*ctx*)
                       (agent-turn :messages '((:role :user :content "go"))
                                   :tools '(:tool-echo) :hooks '(:hook-forge)
                                   :sink (recorder-sink recorder))))
    (is (= 1 (length (events-of recorder :run-done))))
    (is (eq :stop (getf (first (events-of recorder :run-done)) :reason)))
    (is (equal "kept" (getf (first (events-of recorder :custom)) :note)))
    (is (eq :hook-forge (getf (first (events-of recorder :custom)) :hook)))
    (is (integerp *forge-run*))))
