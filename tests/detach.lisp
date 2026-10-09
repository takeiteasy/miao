(in-package #:miao/tests)
(in-suite :miao)

;;; Detached tool calls (~takeiteasy/miao#76): a call past :TOOL-GRACE, or to a
;;; :BACKGROUND tool, is answered with a stub so the turn goes on, and its
;;; result folds in later.

(m:defservice tool-bg () () (:name :tool-bg))

(defmethod m:metadata ((service tool-bg))
  (list :kind :tool :name :tool-bg :trust :agent :background t
        :summary "Answer after a second, in the background" :params nil))

(miao::define-tool-handler tool-bg (service args)
  args
  (sleep 1)
  (miao::ok :bg t))

(defun scripted (&rest replies)
  "A backend answering each request with the next of REPLIES, the last one
repeating."
  (let ((n 0))
    (lambda (&rest request)
      (declare (ignore request))
      (let ((reply (nth (min n (1- (length replies))) replies)))
        (incf n)
        reply))))

(defun request-body (n) (getf (nth (1- n) (requests)) :body))

(defun run-detached (child &optional (timeout 8))
  (m:cast child (list :run :messages '((:role :user :content "go"))))
  (multiple-value-bind (message received) (m:receive :timeout timeout)
    (is-true received)
    (fourth message)))

(test a-slow-call-detaches-and-its-result-folds-into-a-later-turn
  (with-agent ((scripted (tool-call-reply "c1" "tool-slow" "{}")
                         (final-reply "moving on")
                         (final-reply "got it"))
               'tool-slow)
    (m:with-process (runner)
      (let* ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                :tools '(:tool-slow) :tool-grace 100))
             (result (run-detached child)))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (= 3 (getf (second result) :turns)))
        (is (equal "got it" (miao:content-text (getf (second result) :content))))
        (is (= 3 (length (requests))))
        (is (search "running" (request-body 2)))
        (is (not (search "finished" (request-body 2))))
        (is (search "tool call c1 (tool-slow) finished" (request-body 3)))
        (is (search "slow" (request-body 3)))))))

(test a-background-tool-detaches-without-a-grace
  (with-agent ((scripted (tool-call-reply "c1" "tool-bg" "{}")
                         (final-reply "moving on")
                         (final-reply "got it"))
               'tool-bg)
    (m:with-process (runner)
      (let* ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                :tools '(:tool-bg)))
             (result (run-detached child)))
        (is (= 3 (getf (second result) :turns)))
        (is (search "finished" (request-body 3)))))))

(test a-call-that-answers-within-the-grace-is-not-detached
  (with-agent ((scripted (tool-call-reply "c1" "tool-echo" "{\"text\":\"hi\"}")
                         (final-reply "done"))
               'tool-echo)
    (m:with-process (runner)
      (let* ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                :tools '(:tool-echo) :tool-grace 5000))
             (result (run-detached child)))
        (is (= 2 (getf (second result) :turns)))
        (is (not (search "running" (request-body 2))))
        (is (search "hi" (request-body 2)))))))

(test the-events-run-detached-then-result
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (case (incf n)
                     (1 (sse-response
                         "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"c1\",\"function\":{\"name\":\"tool-slow\",\"arguments\":\"{}\"}}]}}]}"
                         "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
                         "[DONE]"))
                     (t (sse-response
                         "{\"choices\":[{\"delta\":{\"content\":\"ok\"},\"finish_reason\":\"stop\"}]}"
                         "[DONE]"))))
                 'tool-slow)
      (m:with-process (runner)
        (let* ((recorder (make-recorder))
               (child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                  :tools '(:tool-slow) :tool-grace 100
                                  :sink (recorder-sink recorder))))
          (run-detached child)
          (let ((types (event-types (recorded-events recorder))))
            (is (< (position :tool-call types) (position :tool-detached types)))
            (is (< (position :tool-detached types) (position :tool-result types)))
            (is (eq :run-done (car (last types))))
            (is (not (member :steer types)) "a folded result is not a steer")))))))

(test a-cancel-while-a-call-is-detached-closes-it
  (setf *tool-wait-cancelled* nil)
  (with-vault-path (path)
    (with-agent ((scripted (tool-call-reply "c1" "tool-wait" "{}")
                           (final-reply "waiting"))
                 'tool-wait)
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                 :tools '(:tool-wait) :tool-grace 100 :journal path)))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually
                    (lambda ()
                      (getf (getf (m:call child '(:snapshot)) :in-flight) :detached))))
          (m:cast child '(:cancel))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :cancelled (getf (second (fourth message)) :stop-reason))))
          (is-true (eventually (lambda () *tool-wait-cancelled*)))
          (is (eq :interrupted (getf (first (miao:call-entries path)) :status))))))))

(test a-snapshot-with-a-detached-call-lists-it-and-keeps-the-stub
  (with-agent ((scripted (tool-call-reply "c1" "tool-wait" "{}")
                         (final-reply "waiting"))
               'tool-wait)
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                               :tools '(:tool-wait) :tool-grace 100)))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (is-true (eventually
                  (lambda ()
                    (getf (getf (m:call child '(:snapshot)) :in-flight) :detached))))
        (let* ((snapshot (m:call child '(:snapshot)))
               (stub (find :tool (getf snapshot :messages)
                           :key (lambda (m) (getf m :role)))))
          (is (equal '("c1") (getf (getf snapshot :in-flight) :detached)))
          (is (equal "c1" (getf stub :tool-call-id)))
          (is (search "running" (miao:content-text (getf stub :content)))))
        (m:cast child '(:cancel))))))

(test the-call-log-holds-a-detached-call-running-until-its-result-lands
  (with-vault-path (path)
    (with-agent ((scripted (tool-call-reply "c1" "tool-slow" "{}")
                           (final-reply "moving on")
                           (final-reply "got it"))
                 'tool-slow)
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                 :tools '(:tool-slow) :tool-grace 100 :journal path)))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually (lambda () (= 2 (length (requests))))))
          (is (eq :running (getf (first (miao:call-entries path)) :status)))
          (is-true (nth-value 1 (m:receive :timeout 8)))
          (let ((call (first (miao:call-entries path))))
            (is (eq :ok (getf call :status)))
            (is (search "slow" (getf call :content)))))))))

(test a-steer-while-the-run-waits-on-a-detached-call-gets-a-turn
  (with-agent ((scripted (tool-call-reply "c1" "tool-wait" "{}")
                         (final-reply "waiting")
                         (final-reply "steered"))
               'tool-wait)
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                               :tools '(:tool-wait) :tool-grace 100)))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (is-true (eventually (lambda () (= 2 (length (requests))))))
        (m:cast child (list :steer :content "hello again"))
        (is-true (eventually (lambda () (= 3 (length (requests))))))
        (is (search "hello again" (request-body 3)))
        (m:cast child '(:cancel))))))

(test two-detached-results-close-together-do-not-double-the-turn
  ;; Two tools, so both run at once and answer within milliseconds of each other.
  (with-agent ((scripted (tool-calls-reply '("c1" "tool-gate" "{}") '("c2" "tool-gate-b" "{}"))
                         (final-reply "waiting")
                         (final-reply "got both"))
               'tool-gate 'tool-gate-b)
    (m:with-process (runner)
      (let* ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                :tools '(:tool-gate :tool-gate-b) :tool-grace 50))
             (result (run-detached child))
             (turns (getf (second result) :turns)))
        (is (<= 3 turns 4))
        (is (= turns (length (requests))) "every turn is one request, none issued twice")
        (is (search "tool call c1" (request-body turns)))
        (is (search "tool call c2" (request-body turns)))))))

;;; A call's meow timeout is the tool's :TIMEOUT plus 5s. The agent times a call
;;; itself, only while it is attached (~takeiteasy/miao#183).

(m:defservice tool-unbounded () () (:name :tool-unbounded))

(defmethod m:metadata ((service tool-unbounded))
  (list :kind :tool :name :tool-unbounded :trust :agent :background t
        :summary "Answer after six seconds, whatever its timeout"
        :params '((:timeout (integer 1) :default 1 :doc "milliseconds"))))

(miao::define-tool-handler tool-unbounded (service args)
  args
  (sleep 6)
  (miao::ok :unbounded t))

(m:defservice tool-unbounded-attached () () (:name :tool-unbounded-attached))

(defmethod m:metadata ((service tool-unbounded-attached))
  (list :kind :tool :name :tool-unbounded-attached :trust :agent
        :summary "Answer after six seconds, whatever its timeout"
        :params '((:timeout (integer 1) :default 1 :doc "milliseconds"))))

(miao::define-tool-handler tool-unbounded-attached (service args)
  args
  (sleep 6)
  (miao::ok :unbounded t))

(test a-detached-call-outlasts-its-call-timeout
  (with-agent ((scripted (tool-call-reply "c1" "tool-unbounded" "{}")
                         (final-reply "moving on")
                         (final-reply "got it"))
               'tool-unbounded)
    (m:with-process (runner)
      (let* ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                :tools '(:tool-unbounded)))
             (result (run-detached child 15)))
        (is (= 3 (getf (second result) :turns)))
        (is (search "tool call c1 (tool-unbounded) finished" (request-body 3)))
        (is (search "unbounded" (request-body 3)))
        (is (not (search "error" (request-body 3))))))))

(test an-attached-call-still-times-out
  (with-agent ((scripted (tool-call-reply "c1" "tool-unbounded-attached" "{}")
                         (final-reply "done"))
               'tool-unbounded-attached)
    (m:with-process (runner)
      (let* ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                :tools '(:tool-unbounded-attached)))
             (result (run-detached child 15)))
        (is (= 2 (getf (second result) :turns)))
        (is (search "{\\\"error\\\":\\\"timeout\\\"}" (request-body 2)))))))

;;; :MAX-DETACHED (~takeiteasy/miao#184): a call past the cap stays attached
;;; and detaches when a slot frees.

(test a-call-past-max-detached-detaches-when-a-slot-frees
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (if (= (incf n) 1)
                       (sse-response
                        "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"c1\",\"function\":{\"name\":\"tool-slow\",\"arguments\":\"{}\"}}]}}]}"
                        "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":1,\"id\":\"c2\",\"function\":{\"name\":\"tool-wait\",\"arguments\":\"{}\"}}]}}]}"
                        "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
                        "[DONE]")
                       (sse-response
                        "{\"choices\":[{\"delta\":{\"content\":\"waiting\"},\"finish_reason\":\"stop\"}]}"
                        "[DONE]")))
                 'tool-slow 'tool-wait)
    (m:with-process (runner)
      (let* ((recorder (make-recorder))
             (child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                :tools '(:tool-slow :tool-wait) :tool-grace 100
                                :max-detached 1 :sink (recorder-sink recorder))))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (is-true (eventually (lambda () (= 2 (length (requests)))) 8))
        (is (search "tool call c1 (tool-slow) finished" (request-body 2)))
        (is (equal '((:tool-detached "c1") (:tool-result "c1") (:tool-detached "c2"))
                   (loop for event in (recorded-events recorder)
                         when (member (getf event :type) '(:tool-detached :tool-result))
                           collect (list (getf event :type) (getf event :id)))))
        (m:cast child '(:cancel)))))))
