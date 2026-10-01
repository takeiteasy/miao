(in-package #:miao/tests)
(in-suite :miao)

;;; Interceptor hooks (~takeiteasy/miao#117): the three phases, how a chain
;;; combines hooks, what a failed hook does, and that nothing a hook guards can
;;; be reached around.

(defvar *hook-seen* nil "What HOOK-RECORD was asked, newest first.")

(miao:define-hook :hook-deny-echo (:phases (:before-tool-call) :summary "Deny tool-echo")
  (:intercept (phase request)
    (if (eq (getf request :name) :tool-echo) '(:deny "no echo") :pass)))

(miao:define-hook :hook-deny-all (:phases (:before-tool-call))
  (:intercept (phase request) '(:deny "no calls")))

(miao:define-hook :hook-upcase (:phases (:before-tool-call))
  (:intercept (phase request)
    (if (eq (getf request :name) :tool-echo) '(:rewrite (:text "REWRITTEN")) :pass)))

(miao:define-hook :hook-redact (:phases (:after-tool-result))
  (:intercept (phase request) '(:rewrite (:ok (:text "[redacted]")))))

(miao:define-hook :hook-inject (:phases (:before-turn))
  (:intercept (phase request)
    `(:rewrite ,(append (getf request :messages)
                        (list (list :role :user :content "injected"))))))

(miao:define-hook :hook-record ()
  (:intercept (phase request)
    (push (list phase (getf request :name) (getf request :arguments)
                (and (member :parent request) t))
          *hook-seen*)
    :pass))

(miao:define-hook :hook-boom (:phases (:before-tool-call))
  (:intercept (phase request) (error "hook broke")))

(miao:define-hook :hook-boom-turn (:phases (:before-turn))
  (:intercept (phase request) (error "hook broke")))

(miao:define-hook :hook-boom-open (:phases (:before-tool-call) :on-error :pass)
  (:intercept (phase request) (error "hook broke")))

(miao:define-hook :hook-garbage (:phases (:before-tool-call))
  (:intercept (phase request) :maybe))

(miao:define-hook :hook-slow (:phases (:before-tool-call) :timeout 150)
  (:intercept (phase request) (sleep 1) :pass))

(miao:define-hook :hook-blocker (:phases (:before-tool-call) :timeout 5000)
  (:intercept (phase request) (sleep 3) :pass))

(defparameter +test-hooks+
  '(hook-deny-echo hook-deny-all hook-upcase hook-redact hook-inject hook-record hook-boom
    hook-boom-turn hook-boom-open hook-garbage hook-slow hook-blocker))

(defun echo-answer (&key sse)
  "A model that calls tool-echo once, then answers. A sink makes a request
stream, so SSE answers in its place."
  (let ((n 0))
    (lambda (&rest request)
      (declare (ignore request))
      (if (= 1 (incf n))
          (funcall (if sse #'sse-tool-call #'tool-call-reply) "c1" "tool-echo"
                   "{\"text\":\"hi\"}")
          (if sse (streamed-reply "done") (final-reply "done"))))))

(defun hooked-run (hooks &rest extra)
  "Run an echo agent against HOOKS and return its result."
  (setf *hook-seen* nil)
  (call-with-agent (echo-answer :sse (getf extra :sink)) (list* 'tool-echo +test-hooks+)
                   (lambda (*ctx*)
                     (apply #'agent-turn :messages '((:role :user :content "go"))
                            :tools '(:tool-echo) :hooks hooks extra))))

(defun tool-text (result)
  (miao:content-text (getf (find :tool (getf (second result) :messages)
                                 :key (lambda (m) (getf m :role)))
                           :content)))

;;; --- before a tool call -------------------------------------------------------

(test a-deny-answers-the-model-with-a-tool-error-and-the-tool-never-runs
  (let ((result (hooked-run '(:hook-deny-echo))))
    (is (eq :stop (getf (second result) :stop-reason)))
    (is (search "denied" (tool-text result)))
    (is (search "no echo" (tool-text result)))
    (is (not (search "\"text\"" (tool-text result))))))

(test a-rewrite-changes-the-arguments-the-tool-runs-with
  (let ((result (hooked-run '(:hook-upcase))))
    (is (search "REWRITTEN" (tool-text result)))))

(test a-hook-runs-for-the-reserved-sub-agent-call-too
  (let ((n 0))
    (call-with-agent
     (lambda (&rest request)
       (declare (ignore request))
       (if (= 1 (incf n))
           (tool-call-reply "c1" "agent-task" "{\"task\":\"help\"}")
           (final-reply "done")))
     (list* 'tool-echo +test-hooks+)
     (lambda (*ctx*)
       (let ((result (agent-turn :messages '((:role :user :content "go"))
                                 :tools '(:tool-echo) :sub-agents t :hooks '(:hook-deny-all))))
         (is (search "no calls" (tool-text result)))
         (is (= 2 (length (requests)))))))))

(test a-sub-agent-inherits-its-parents-hooks
  (setf *hook-seen* nil)
  (let ((n 0))
    (call-with-agent
     (lambda (&rest request)
       (declare (ignore request))
       (case (incf n)
         (1 (tool-call-reply "c1" "agent-task" "{\"task\":\"help\"}"))
         (2 (tool-call-reply "c2" "tool-echo" "{\"text\":\"hi\"}"))
         (t (final-reply "done"))))
     (list* 'tool-echo +test-hooks+)
     (lambda (*ctx*)
       (let ((result (agent-turn :messages '((:role :user :content "go"))
                                 :tools '(:tool-echo) :sub-agents t :hooks '(:hook-record))))
         (is (eq :stop (getf (second result) :stop-reason)))
         (let ((calls (remove :before-tool-call *hook-seen* :key #'first :test-not #'eq)))
           (is-true (find :agent-task calls :key #'second))
           (is-true (find-if (lambda (c) (and (eq :tool-echo (second c)) (fourth c))) calls))))))))

;;; --- a chain -------------------------------------------------------------------

(test hooks-run-in-order-and-each-sees-the-last-ones-rewrite
  (hooked-run '(:hook-upcase :hook-record))
  (is (equal '(:text "REWRITTEN")
             (third (find :before-tool-call *hook-seen* :key #'first)))))

(test a-deny-stops-the-chain
  (hooked-run '(:hook-deny-echo :hook-record))
  (is (null (find :before-tool-call *hook-seen* :key #'first))))

(test a-hook-that-names-no-phase-is-skipped-for-the-others
  (hooked-run '(:hook-upcase))
  (is (null *hook-seen*)))

;;; --- after a tool result ---------------------------------------------------------

(test a-rewritten-result-is-what-the-conversation-and-a-sink-see
  (let ((recorder (make-recorder)))
    (let ((result (hooked-run '(:hook-redact) :sink (recorder-sink recorder))))
      (is (search "[redacted]" (tool-text result)))
      (is (not (search "hi" (tool-text result))))
      (let ((event (find :tool-result (recorded-events recorder)
                         :key (lambda (e) (getf e :type)))))
        (is (equal '(:ok (:text "[redacted]")) (getf event :result)))))))

(test a-detached-result-passes-through-the-after-hooks
  (let ((n 0))
    (call-with-agent
     (lambda (&rest request)
       (declare (ignore request))
       (if (= 1 (incf n))
           (tool-call-reply "c1" "tool-hold" "{}")
           (final-reply "done")))
     (list* 'tool-hold +test-hooks+)
     (lambda (*ctx*)
       (let ((result (agent-turn :messages '((:role :user :content "go"))
                                 :tools '(:tool-hold) :tool-grace 50 :hooks '(:hook-redact))))
         (is (eq :stop (getf (second result) :stop-reason)))
         (let ((body (getf (car (last (requests))) :body)))
           (is (search "[redacted]" body))
           (is (not (search "slept" body)))))))))

;;; --- before a turn ------------------------------------------------------------------

(test a-before-turn-hook-changes-the-request-not-the-conversation
  (setf *hook-seen* nil)
  (call-with-agent (final-reply "hi") +test-hooks+
                   (lambda (*ctx*)
                     (let ((result (agent-turn :messages '((:role :user :content "go"))
                                               :hooks '(:hook-inject))))
                       (is (search "injected" (getf (first (requests)) :body)))
                       (is (not (find "injected" (message-texts result) :key #'cdr
                                                                         :test #'search)))))))

;;; --- a hook that fails -----------------------------------------------------------------

(test a-closed-hook-that-signals-denies-the-call
  (let ((result (hooked-run '(:hook-boom))))
    (is (search "hook-failed" (tool-text result)))
    (is (not (search "\"text\"" (tool-text result))))))

(test an-open-hook-that-signals-is-passed
  (let ((result (hooked-run '(:hook-boom-open))))
    (is (search "hi" (tool-text result)))))

(test a-mount-override-beats-the-hooks-own-policy
  (is (search "hi" (tool-text (hooked-run '((:hook-boom :on-error :pass))))))
  (is (search "hook-failed" (tool-text (hooked-run '((:hook-boom-open :on-error :deny)))))))

(test an-answer-that-is-not-one-is-a-failure
  (is (search "hook-failed" (tool-text (hooked-run '(:hook-garbage))))))

(test a-hook-past-its-timeout-fails-by-its-policy
  (is (search "hook-failed" (tool-text (hooked-run '(:hook-slow))))))

(test a-closed-before-turn-hook-that-fails-ends-the-run
  (let ((result (hooked-run '(:hook-boom-turn))))
    (is (eq :error (first result)))
    (is (eq :hook-failed (first (second result))))
    (is (null (requests)))))

(test a-hook-that-is-not-registered-refuses-the-run
  (let ((result (hooked-run '(:hook-nowhere))))
    (is (eq :error (first result)))
    (is (search "hook-nowhere" (princ-to-string result)))
    (is (null (requests)))))

(test a-bad-override-refuses-the-run
  (let ((result (hooked-run '((:hook-record :on-error :maybe)))))
    (is (eq :error (first result)))))

;;; --- a function as a hook ----------------------------------------------------------------

(test a-function-is-a-hook
  (let ((result (hooked-run (list (list (lambda (phase request)
                                          (declare (ignore phase request))
                                          '(:rewrite (:text "FN")))
                                        :phases '(:before-tool-call))))))
    (is (search "FN" (tool-text result)))))

(test a-function-hook-that-signals-fails-by-its-policy
  (flet ((run-with (&rest options)
           (tool-text (hooked-run (list (list* (lambda (phase request)
                                                 (declare (ignore phase request))
                                                 (error "broke"))
                                               :phases '(:before-tool-call) options))))))
    (is (search "hook-failed" (run-with)))
    (is (search "hi" (run-with :on-error :pass)))))

;;; --- not blocking the agent --------------------------------------------------------------------

(test a-hook-that-is-waiting-does-not-hold-up-cancel
  (setf *hook-seen* nil)
  (call-with-agent (echo-answer) (list* 'tool-echo +test-hooks+)
                   (lambda (*ctx*)
                     (m:with-process (runner)
                       (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                                :tools '(:tool-echo) :hooks '(:hook-blocker))))
                         (m:cast child (list :run :messages '((:role :user :content "go"))))
                         (is-true (eventually
                                   (lambda ()
                                     (getf (getf (m:call child '(:snapshot)) :in-flight)
                                           :tool-calls))))
                         (let ((start (get-internal-real-time)))
                           (m:cast child '(:cancel))
                           (multiple-value-bind (message received) (m:receive :timeout 2)
                             (is-true received)
                             (is (eq :cancelled (getf (second (fourth message)) :stop-reason)))
                             (is (< (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second)
                                    1.5))))
                         (sleep 3.2))))))

;;; --- resumed calls and late answers ---------------------------------------------------------

(test a-resumed-call-goes-through-the-before-tool-call-hooks
  (with-vault-path (path)
    (seed-call path "x-0" :call-id "c9")
    (with-agent ((scripted (final-reply "moving on") (final-reply "got it"))
                 'tool-again 'hook-deny-all)
      (m:with-process (runner)
        (let* ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                  :tools '(:tool-again) :call-log path :hooks '(:hook-deny-all)))
               (answer (call-child child (list :run :continue t :resume '("x-0")
                                                    :messages '((:role :user :content "go"))))))
          (is (equal '("x-0") (mapcar #'car (getf (second answer) :resumed))))
          (is-true (nth-value 1 (m:receive :timeout 8)))
          (is (search "tool call c9 (tool-again) finished" (request-body 2)))
          (is (search "no calls" (request-body 2)))
          (is (not (search "again\":true" (request-body 2)))))))))

(test an-answer-that-comes-after-an-interrupt-is-dropped
  (call-with-agent (echo-answer) (list* 'tool-echo +test-hooks+)
                   (lambda (*ctx*)
                     (let ((agent (m:mount *ctx* 'miao:agent :name :hooked
                                                             :model :provider-test-keyed
                                                             :tools '(:tool-echo)
                                                             :hooks '(:hook-blocker))))
                       (m:cast agent (list :run :messages '((:role :user :content "go"))))
                       (is-true (eventually (lambda ()
                                              (getf (getf (m:call agent '(:snapshot)) :in-flight)
                                                    :tool-calls))))
                       (m:cast agent (list :steer :content "change of plan" :interrupt t))
                       (is-true (eventually (lambda ()
                                              (null (getf (m:call agent '(:snapshot)) :in-flight)))))
                       (sleep 3.3)
                       (let ((messages (getf (m:call agent '(:snapshot)) :messages)))
                         (is (= 2 (length (requests))))
                         (is (equal "{\"error\":\"interrupted\"}"
                                    (miao:content-text
                                     (getf (find :tool messages :key (lambda (m) (getf m :role)))
                                           :content)))))))))

;;; --- what is recorded -------------------------------------------------------------------------------

(test a-hook-that-acts-is-announced-without-its-payload
  (let ((recorder (make-recorder)))
    (hooked-run '(:hook-upcase :hook-deny-echo) :sink (recorder-sink recorder))
    (let ((events (remove :hook (recorded-events recorder)
                          :key (lambda (e) (getf e :type)) :test-not #'eq)))
      (is (equal '(:rewrite :deny) (mapcar (lambda (e) (getf e :action)) events)))
      (is (equal '(:hook-upcase :hook-deny-echo) (mapcar (lambda (e) (getf e :hook)) events)))
      (is (equal "c1" (getf (first events) :id))))))

(test a-denied-call-is-announced-and-answered
  (let ((recorder (make-recorder)))
    (hooked-run '(:hook-deny-echo) :sink (recorder-sink recorder))
    (let ((types (mapcar (lambda (e) (getf e :type)) (recorded-events recorder))))
      (is (equal '(:hook :tool-call :tool-result) (remove-if-not
                                                    (lambda (type)
                                                      (member type '(:hook :tool-call :tool-result)))
                                                    types))))))

(test the-call-log-keeps-the-hooked-values-and-the-raw-ones-on-request
  (dolist (log-raw '(nil t))
    (with-call-agent (path (echo-answer) 'tool-echo 'hook-upcase 'hook-redact)
        (:tools '(:tool-echo) :hooks '(:hook-upcase :hook-redact) :log-raw log-raw)
      (run-child child)
      (is-true (m:receive :timeout 5))
      (let ((call (one-call path))
            (entries (miao::%read-log path)))
        (is (search "REWRITTEN" (getf call :arguments)))
        (is (search "redacted" (getf call :content)))
        (let ((accepted (find :call entries :key (lambda (e) (getf e :kind))))
              (done (find :done entries :key (lambda (e) (getf e :kind)))))
          (if log-raw
              (progn (is (search "hi" (getf accepted :raw-arguments)))
                     (is (search "REWRITTEN" (getf done :raw-content))))
              (progn (is (null (getf accepted :raw-arguments)))
                     (is (null (getf done :raw-content))))))))))

(test a-denied-call-is-logged-denied
  (with-call-agent (path (echo-answer) 'tool-echo 'hook-deny-echo)
      (:tools '(:tool-echo) :hooks '(:hook-deny-echo))
    (run-child child)
    (is-true (m:receive :timeout 5))
    (is (equal '(:denied) (call-statuses path)))))

;;; --- the convention ------------------------------------------------------------------------------------

(test hooks-are-discovered-and-described
  (call-with-agent (final-reply "hi") +test-hooks+
                   (lambda (*ctx*)
                     (is-true (member :hook-deny-echo (miao:hooks)))
                     (is (equal '(:before-tool-call)
                                (getf (miao:describe-hook :hook-deny-echo) :phases)))
                     (is (eq :deny (getf (miao:describe-hook :hook-deny-echo) :on-error)))
                     (is-true (member :hook-deny-echo (miao:definitions :kind :hook))))))

(test an-agent-lists-its-hooks-in-its-metadata
  (call-with-agent (final-reply "hi") +test-hooks+
                   (lambda (*ctx*)
                     (m:mount *ctx* 'miao:agent :name :hooked :model :provider-test-keyed
                                                :hooks '(:hook-record))
                     (is (equal '(:hook-record)
                                (getf (miao:describe-agent :hooked) :hooks))))))

(test an-answer-is-read-by-phase
  (is (eq :pass (miao::interpret-hook-answer :before-turn :pass)))
  (is (equal '(:rewrite (:a 1))
             (multiple-value-list (miao::interpret-hook-answer :before-tool-call '(:rewrite (:a 1))))))
  (is (eq :failed (miao::interpret-hook-answer :before-tool-call '(:rewrite 1))))
  (is (eq :deny (miao::interpret-hook-answer :before-tool-call '(:deny "x"))))
  (is (eq :failed (miao::interpret-hook-answer :after-tool-result '(:deny "x"))))
  (is (eq :failed (miao::interpret-hook-answer :before-turn nil)))
  (is (eq :failed (miao::interpret-hook-answer :after-tool-result '(:rewrite 5)))))
