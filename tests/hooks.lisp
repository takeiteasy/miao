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

(defvar *hook-ended* nil "The :RUN-DONE notices HOOK-ENDS got, newest first.")

(miao:define-hook :hook-ends (:phases (:before-tool-call))
  (:intercept (phase request)
    (push (list :intercept (getf request :handle) (getf request :run)) *hook-seen*)
    :pass)
  (:run-done (request)
    (push request *hook-ended*)))

(miao:define-hook :hook-ends-boom ()
  (:intercept (phase request) :pass)
  (:run-done (request) (error "cleanup broke")))

(defparameter +test-hooks+
  '(hook-ends hook-ends-boom hook-deny-echo hook-deny-all hook-upcase hook-redact hook-inject hook-record hook-boom
    hook-boom-turn hook-boom-open hook-garbage hook-slow hook-blocker hook-wait
    hook-wait-short hook-check-token))

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

(test a-deny-answers-the-model-with-a-result-error-and-the-tool-never-runs
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

;;; --- a function hook's service ----------------------------------------------------------------

(defun hook-function-count (context)
  (count 'miao::hook-function (m:children context) :key (lambda (child) (getf child :class))))

(defun pass-before-turn ()
  (list (lambda (phase request) (declare (ignore phase request)) :pass) :phases '(:before-turn)))

(test a-function-hooks-service-goes-with-the-run-that-mounted-it
  (call-with-agent (final-reply "hi") +test-hooks+
                   (lambda (*ctx*)
                     (dotimes (i 3)
                       (agent-turn :messages '((:role :user :content "go"))
                                   :hooks (list (pass-before-turn))))
                     (is-true (eventually (lambda () (zerop (hook-function-count *ctx*))))))))

(test unmounting-an-agent-stops-its-function-hook-and-the-context-still-stops-promptly
  (let ((start (get-internal-real-time)))
    (call-with-agent (final-reply "hi") +test-hooks+
                     (lambda (*ctx*)
                       (flet ((mount-and-run (name)
                                (let ((agent (m:mount *ctx* 'miao:agent :name name
                                                                         :model :test-keyed
                                                                         :hooks (list (pass-before-turn)))))
                                  (m:call agent (list :run :messages '((:role :user :content "go"))))
                                  (is-true (eventually (lambda ()
                                                         (= 1 (hook-function-count *ctx*))))))))
                         (mount-and-run :first)
                         (m:unmount *ctx* :first)
                         (is-true (eventually (lambda () (zerop (hook-function-count *ctx*)))))
                         (mount-and-run :second))))
    (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 3))))

;;; --- not blocking the agent --------------------------------------------------------------------

(test a-hook-that-is-waiting-does-not-hold-up-cancel
  (setf *hook-seen* nil)
  (call-with-agent (echo-answer) (list* 'tool-echo +test-hooks+)
                   (lambda (*ctx*)
                     (m:with-process (runner)
                       (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
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

;;; --- told to stop (~takeiteasy/miao#210) -----------------------------------------------------

(defvar *hook-told* nil "How each :HOOK-WAIT call ended, newest first.")

(miao:define-hook :hook-wait (:phases (:before-tool-call) :timeout 5000)
  (:intercept (phase request)
    (let ((semaphore (bt:make-semaphore)))
      (miao:on-cancel (getf request :cancel) (lambda () (bt:signal-semaphore semaphore)))
      (push (if (bt:wait-on-semaphore semaphore :timeout 4) :cancelled :never-told) *hook-told*)
      :pass)))

(miao:define-hook :hook-wait-short (:phases (:before-tool-call) :timeout 200)
  (:intercept (phase request)
    (let ((semaphore (bt:make-semaphore)))
      (miao:on-cancel (getf request :cancel) (lambda () (bt:signal-semaphore semaphore)))
      (push (if (bt:wait-on-semaphore semaphore :timeout 4) :cancelled :never-told) *hook-told*)
      :pass)))

(miao:define-hook :hook-check-token (:phases (:before-tool-call))
  (:intercept (phase request)
    (push (miao:cancelled-p (getf request :cancel)) *hook-told*)
    :pass))

(defun waiting-hook-run (hooks &rest extra)
  "Mount an echo agent running HOOKS, start a run and call :BODY with the agent
once its first tool call is waiting on a hook."
  (setf *hook-told* nil)
  (let ((body (getf extra :body)))
    (call-with-agent (echo-answer) (append (list 'tool-echo) +test-hooks+)
                     (lambda (*ctx*)
                       (let ((agent (apply #'m:mount *ctx* 'miao:agent :name :hooked
                                           :model :test-keyed :tools '(:tool-echo)
                                           :hooks hooks
                                           (and (getf extra :deadline)
                                                (list :deadline (getf extra :deadline))))))
                         (m:cast agent (list :run :messages '((:role :user :content "go"))))
                         (is-true (eventually (lambda ()
                                                (getf (getf (m:call agent '(:snapshot)) :in-flight)
                                                      :tool-calls))))
                         (funcall body agent))))))

(test a-hook-is-told-when-its-run-is-cancelled
  (waiting-hook-run '(:hook-wait)
                    :body (lambda (agent)
                            (m:cast agent '(:cancel))
                            (is-true (eventually (lambda () (equal '(:cancelled) *hook-told*)))))))

(test a-hook-is-told-when-the-deadline-passes
  (waiting-hook-run '(:hook-wait) :deadline 300
                    :body (lambda (agent)
                            (declare (ignore agent))
                            (is-true (eventually (lambda () (equal '(:cancelled) *hook-told*)) 3)))))

(test a-hook-is-told-when-an-interrupting-steer-abandons-its-call
  (waiting-hook-run '(:hook-wait)
                    :body (lambda (agent)
                            (m:cast agent (list :steer :content "change of plan" :interrupt t))
                            (is-true (eventually (lambda () (equal '(:cancelled) *hook-told*)))))))

(test a-hook-is-told-when-the-agent-gives-up-on-its-timeout
  (waiting-hook-run '(:hook-wait-short)
                    :body (lambda (agent)
                            (declare (ignore agent))
                            (is-true (eventually (lambda () (equal '(:cancelled) *hook-told*)) 3)))))

(test a-hook-is-told-when-its-agent-is-restored
  (waiting-hook-run '(:hook-wait)
                    :body (lambda (agent)
                            (m:call agent (list :restore (list :messages '((:role :user :content "go")) :turns 1)))
                            (is-true (eventually (lambda () (equal '(:cancelled) *hook-told*)))))))

(test a-hook-that-answers-in-time-is-not-cancelled-while-it-runs
  (setf *hook-told* nil)
  (hooked-run '(:hook-check-token))
  (is (equal '(nil) *hook-told*)))

(test a-hook-called-with-a-cancelled-token-does-not-run
  (setf *hook-told* nil)
  (call-with-agent (echo-answer) '(hook-check-token)
                   (lambda (*ctx*)
                     (let ((token (miao:make-cancel-token)))
                       (miao:cancel token)
                       (is (equal '(:error :cancelled)
                                  (m:call (m:lookup :hook-check-token)
                                          (list :intercept :phase :before-tool-call :cancel token
                                                :name :tool-echo :arguments nil)))))))
  (is (null *hook-told*)))

;;; --- resumed calls and late answers ---------------------------------------------------------

(test a-resumed-call-goes-through-the-before-tool-call-hooks
  (with-vault-path (path)
    (seed-call path "x-0" :call-id "c9")
    (with-agent ((scripted (final-reply "moving on") (final-reply "got it"))
                 'tool-again 'hook-deny-all)
      (m:with-process (runner)
        (let* ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                  :tools '(:tool-again) :journal path :hooks '(:hook-deny-all)))
               (answer (call-child child (list :run :continue t :resume '("x-0")
                                                    :messages '((:role :user :content "go"))))))
          (is (equal '("x-0") (mapcar #'car (getf (second answer) :resumed))))
          (is-true (nth-value 1 (m:receive :timeout 8)))
          (is (search "tool call c9 (tool-again) finished" (request-body 2)))
          (is (search "no calls" (request-body 2)))
          (is (not (search "again\":true" (request-body 2)))))))))

(defvar *resumed-text* nil)
(defvar *suffix-saw* nil)

(m:defservice tool-again-echo () () (:name :tool-again-echo))

(defmethod m:metadata ((service tool-again-echo))
  (list :kind :tool :name :tool-again-echo :trust :agent :resumable t
        :summary "Echo TEXT back, safe to run twice"
        :params '((:text string :required t :doc "text to echo"))))

(miao::define-tool-handler tool-again-echo (service args)
  (setf *resumed-text* (getf args :text))
  (miao::ok :text *resumed-text*))

(miao:define-hook :hook-suffix (:phases (:before-tool-call))
  (:intercept (phase request)
    (push (getf request :resumed) *suffix-saw*)
    (list :rewrite (list :text (concatenate 'string (getf (getf request :arguments) :text) "!")))))

(test a-resumed-call-runs-with-the-logged-arguments-and-the-hooks-only-decide
  (dolist (log-raw '(nil t))
    (with-vault-path (path)
      (seed-call path "x-0" :name :tool-again-echo :arguments "{\"text\":\"hi!\"}" :call-id "c9")
      (setf *resumed-text* nil
            *suffix-saw* nil)
      (with-agent ((scripted (final-reply "moving on") (final-reply "got it"))
                   'tool-again-echo 'hook-suffix)
        (m:with-process (runner)
          (let* ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                    :tools '(:tool-again-echo) :journal path
                                    :hooks '(:hook-suffix) :log-raw log-raw))
                 (answer (call-child child (list :run :continue t :resume '("x-0")
                                                      :messages '((:role :user :content "go"))))))
            (is (equal '("x-0") (mapcar #'car (getf (second answer) :resumed))))
            (is-true (nth-value 1 (m:receive :timeout 8)))
            (is (equal "hi!" *resumed-text*))
            (is (equal '(t) *suffix-saw*))
            (let ((resumed (find "x-0" (miao:call-entries path)
                                 :key (lambda (e) (getf e :resumes)) :test #'equal)))
              (is (equal "{\"text\":\"hi!\"}" (getf resumed :arguments))))))))))

(miao:define-hook :hook-suffix-skipped (:phases (:before-tool-call) :on-resume :skip)
  (:intercept (phase request)
    (push :ran *suffix-saw*)
    :pass))

(defun resume-through (hooks)
  "Resume a logged call through HOOKS and wait for its result to land."
  (with-vault-path (path)
    (seed-call path "x-0" :name :tool-again-echo :arguments "{\"text\":\"hi!\"}" :call-id "c9")
    (setf *resumed-text* nil
          *suffix-saw* nil)
    (with-agent ((scripted (final-reply "moving on") (final-reply "got it"))
                 'tool-again-echo 'hook-suffix 'hook-suffix-skipped 'hook-deny-all)
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :test-keyed
                                 :tools '(:tool-again-echo) :journal path :hooks hooks)))
          (call-child child (list :run :continue t :resume '("x-0")
                                       :messages '((:role :user :content "go"))))
          (is-true (nth-value 1 (m:receive :timeout 8))))))))

(test an-on-resume-skip-hook-is-left-out-of-a-resumed-calls-chain
  (resume-through '(:hook-suffix-skipped))
  (is (null *suffix-saw*))
  (is (equal "hi!" *resumed-text*)))

(test an-on-resume-skip-can-be-set-on-the-hooks-entry
  (resume-through '((:hook-suffix :on-resume :skip)))
  (is (null *suffix-saw*)))

(test a-skipped-hook-does-not-skip-the-hooks-that-deny
  (resume-through '(:hook-suffix-skipped :hook-deny-all))
  (is (null *resumed-text*)))

(test a-bad-on-resume-refuses-the-run
  (is (eq :error (first (hooked-run '((:hook-record :on-resume :never)))))))

(test an-answer-that-comes-after-an-interrupt-is-dropped
  (call-with-agent (echo-answer) (list* 'tool-echo +test-hooks+)
                   (lambda (*ctx*)
                     (let ((agent (m:mount *ctx* 'miao:agent :name :hooked
                                                             :model :test-keyed
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
                     (m:mount *ctx* 'miao:agent :name :hooked :model :test-keyed
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

;;; --- told a run ended (~takeiteasy/miao#212) ---------------------------------------------------------

(defun sub-agent-answer ()
  (let ((n 0))
    (lambda (&rest request)
      (declare (ignore request))
      (case (incf n)
        (1 (tool-call-reply "c1" "agent-task" "{\"task\":\"help\"}"))
        (2 (tool-call-reply "c2" "tool-echo" "{\"text\":\"hi\"}"))
        (t (final-reply "done"))))))

(test a-hook-is-told-once-when-the-root-run-ends-sub-agents-included
  (setf *hook-seen* nil *hook-ended* nil)
  (call-with-agent (sub-agent-answer) (list* 'tool-echo +test-hooks+)
                   (lambda (*ctx*)
                     (agent-turn :messages '((:role :user :content "go"))
                                 :tools '(:tool-echo) :sub-agents t :hooks '(:hook-ends))
                     (is-true (eventually (lambda () *hook-ended*) 3))
                     (sleep 0.1)
                     (is (= 1 (length *hook-ended*)))
                     (let ((notice (first *hook-ended*))
                           (seen (remove :intercept *hook-seen* :key #'first :test-not #'eq)))
                       (is (eq :stop (getf notice :reason)))
                       (is (integerp (getf notice :run)))
                       (is-true seen)
                       (is-true (every (lambda (call) (eq (getf notice :handle) (second call))) seen))
                       (is-true (every (lambda (call) (eql (getf notice :run) (third call))) seen))))))

(test a-hook-with-no-run-done-clause-and-one-that-fails-in-it-leave-the-run-alone
  (setf *hook-ended* nil)
  (let ((result (hooked-run '(:hook-record :hook-ends-boom :hook-ends))))
    (is (eq :stop (getf (second result) :stop-reason)))
    (is-true (eventually (lambda () *hook-ended*) 3))))

(test a-function-hook-is-not-sent-the-notice
  (is (eq :stop (getf (second (hooked-run (list (lambda (phase request)
                                                  (declare (ignore phase request))
                                                  :pass))))
                      :stop-reason))))

(test define-hook-refuses-a-clause-after-intercept-that-is-not-run-done
  (signals error (macroexpand-1 '(miao:define-hook :hook-bad ()
                                  (:intercept (phase request) :pass)
                                  (:other (request) nil))))
  (signals error (macroexpand-1 '(miao:define-hook :hook-bad ()
                                  (:intercept (phase request) :pass)
                                  (:run-done (request) nil)
                                  (:run-done (request) nil)))))
