(in-package #:miao/tests)
(in-suite :miao)

;;; The agent loop against the fake HTTP backend and the provider harness
;;; PROVIDER.LISP set up: allow-list resolution, the tool round trip,
;;; budgets, steering, cancellation, sub-agents and the event stream.

;;; Two :TRUST :AGENT tools -- nothing shipped is that trust level, so the
;;; default allow-list needs at least one to be non-empty in a test.

(m:defservice tool-echo () () (:name :tool-echo))

(defmethod m:metadata ((service tool-echo))
  (list :kind :tool :name :tool-echo :trust :agent
        :summary "Echo TEXT back"
        :params '((:text string :required t :doc "text to echo"))))

(miao::define-tool-handler tool-echo (service args)
  (miao::ok :text (getf args :text)))

(m:defservice tool-boom () () (:name :tool-boom))

(defmethod m:metadata ((service tool-boom))
  (list :kind :tool :name :tool-boom :trust :agent
        :summary "Always fails" :params nil))

(miao::define-tool-handler tool-boom (service args)
  args
  (miao::fail (list :error "boom")))

;;; Holds a call in flight long enough to cancel or snapshot around it.

(m:defservice tool-hold () () (:name :tool-hold))

(defmethod m:metadata ((service tool-hold))
  (list :kind :tool :name :tool-hold :trust :agent
        :summary "Hold the call open, then answer" :params nil))

(miao::define-tool-handler tool-hold (service args)
  args
  (sleep 0.5)
  (miao::ok :slept t))

;;; Holds a call until it is cancelled, or 3s pass, and notes the cancel.

(defvar *tool-wait-cancelled* nil)

(m:defservice tool-wait () () (:name :tool-wait))

(defmethod m:metadata ((service tool-wait))
  (list :kind :tool :name :tool-wait :trust :agent
        :summary "Wait until cancelled" :params nil))

(miao::define-tool-handler tool-wait (service args cancel)
  args
  (let ((woken (bt:make-semaphore)))
    (miao::on-cancel cancel (lambda () (bt:signal-semaphore woken)))
    (if (bt:wait-on-semaphore woken :timeout 3)
        (progn (setf *tool-wait-cancelled* t) (miao::fail :cancelled))
        (miao::ok :waited t))))

;;; Slower than a turn's round trip, faster than TOOL-WAIT.

(m:defservice tool-slow () () (:name :tool-slow))

(defmethod m:metadata ((service tool-slow))
  (list :kind :tool :name :tool-slow :trust :agent
        :summary "Answer after a second" :params nil))

(miao::define-tool-handler tool-slow (service args)
  args
  (sleep 1)
  (miao::ok :slow t))

;;; Two services sharing one count of the calls running at once, and the
;;; most there ever were: one service answers its calls one at a time, so
;;; overlap needs two.

(defvar *gate-lock* (bt:make-lock))
(defvar *gate-running* 0)
(defvar *gate-peak* 0)

(defun reset-gate ()
  (bt:with-lock-held (*gate-lock*) (setf *gate-running* 0 *gate-peak* 0)))

(defun pass-gate ()
  (bt:with-lock-held (*gate-lock*)
    (setf *gate-peak* (max *gate-peak* (incf *gate-running*))))
  (sleep 0.2)
  (bt:with-lock-held (*gate-lock*) (decf *gate-running*))
  (miao::ok :gated t))

(defmacro define-gate (name)
  `(progn
     (m:defservice ,name () () (:name ,(intern (symbol-name name) :keyword)))
     (defmethod m:metadata ((service ,name))
       (list :kind :tool :name ,(intern (symbol-name name) :keyword) :trust :agent
             :summary "Hold the call briefly, counting overlap" :params nil))
     (miao::define-tool-handler ,name (service args)
       args
       (pass-gate))))

(define-gate tool-gate)
(define-gate tool-gate-b)

;;; --- the harness --------------------------------------------------------

(defun call-with-agent (answer tool-classes body)
  "Like WITH-PROVIDERS, plus the tools TOOL-CLASSES names mounted alongside
the keyed provider, ready for RUN-AGENT."
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :agents)
                                   :registry registry))
         (server (start-fake-http
                  (lambda (&rest request)
                    (if (functionp answer) (apply answer request) answer)))))
    (setf *backend* server)
    (unwind-protect
         (progn
           (m:mount context 'miao:protocol-openai)
           (m:mount context 'miao:protocol-ollama)
           (let ((mount (keyed)))
             (apply #'m:mount context (first mount)
                    :base-url (fake-http-url server) (rest mount)))
           (dolist (class tool-classes) (m:mount context class))
           (funcall body context))
      (m:stop context)
      (stop-fake-http server))))

(defmacro with-agent ((answer &rest tool-classes) &body body)
  `(call-with-agent ,answer (list ,@tool-classes) (lambda (*ctx*) ,@body)))

(defvar *ctx* nil "The running context, bound by WITH-AGENT.")

(defun agent-turn (&rest extra)
  (apply #'miao:run-agent *ctx* :model :provider-test-keyed extra))

(defun requests () (fake-http-requests *backend*))

;;; A function sink is called from the agent's emitter, which may still be
;;; delivering after the run has answered, so events are read back through
;;; RECORDED-EVENTS once it has exited.

(defstruct (recorder (:constructor make-recorder ()))
  (lock (bt:make-lock)) events (active 0) overlapped)

(defun recorder-sink (recorder)
  (lambda (event)
    (bt:with-lock-held ((recorder-lock recorder))
      (when (plusp (recorder-active recorder))
        (setf (recorder-overlapped recorder) t))
      (incf (recorder-active recorder))
      (push event (recorder-events recorder)))
    (sleep 0.001)
    (bt:with-lock-held ((recorder-lock recorder))
      (decf (recorder-active recorder)))))

(defun recorder-has (recorder type)
  (bt:with-lock-held ((recorder-lock recorder))
    (find type (recorder-events recorder) :key (lambda (e) (getf e :type)))))

(defun recorded-events (recorder)
  "RECORDER's events, oldest first, once every agent's emitter has finished."
  (is-true (eventually #'sinks-idle-p 5))
  (bt:with-lock-held ((recorder-lock recorder))
    (reverse (recorder-events recorder))))

(defun request-tool-names (n)
  "The `name`s in request N's (1-based) `tools` array."
  (let ((body (com.inuoe.jzon:parse (getf (nth (1- n) (requests)) :body))))
    (map 'list (lambda (tool) (gethash "name" (gethash "function" tool)))
         (gethash "tools" body))))

(defun tool-call-reply (id name arguments-json)
  "ARGUMENTS-JSON is the call's arguments as already-encoded JSON text,
matching the wire's own doubly-encoded shape (a JSON string holding a JSON
object)."
  (json-response
   (format nil "{\"choices\":[{\"message\":{\"role\":\"assistant\",
     \"tool_calls\":[{\"id\":\"~a\",\"type\":\"function\",
       \"function\":{\"name\":\"~a\",\"arguments\":~s}}]},
     \"finish_reason\":\"tool_calls\"}]}"
           id name arguments-json)))

(defun final-reply (text)
  (json-response
   (format nil "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":~a},
     \"finish_reason\":\"stop\"}]}"
           (com.inuoe.jzon:stringify text))))

;;; --- the happy path -----------------------------------------------------

(test a-turn-with-no-tool-calls-finishes-stop
  (with-agent ((final-reply "hi there"))
    (let ((result (agent-turn :messages '((:role :user :content "hello")))))
      (is (eq :ok (first result)))
      (is (eq :stop (getf (second result) :stop-reason)))
      (is (= 1 (getf (second result) :turns)))
      (is (equal "hi there" (miao:content-text (getf (second result) :content))))
      (is (= 1 (length (requests)))))))

(test a-tool-call-round-trips-and-the-run-continues
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (if (= 0 (incf n))
                       nil
                       (if (= n 1)
                           (tool-call-reply "c1" "tool-echo" "{\"text\":\"hi\"}")
                           (final-reply "done"))))
                'tool-echo)
      (let ((result (agent-turn :messages '((:role :user :content "go")))))
        (is (eq :ok (first result)))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (= 2 (length (requests))))
        (let ((tool-message (find :tool (getf (second result) :messages)
                                  :key (lambda (m) (getf m :role)))))
          (is (equal "c1" (getf tool-message :tool-call-id)))
          (is (search "hi" (miao:content-text (getf tool-message :content)))))))))

(test a-disallowed-tool-comes-back-as-a-tool-message
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (incf n)
                   (if (= n 1)
                       (tool-call-reply "c1" "tool-boom" "{}")
                       (final-reply "recovered")))
                'tool-echo)
      ;; tool-boom is mounted but not in the allow-list, so it fails as
      ;; "not allowed" without ever reaching the tool.
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :tools '(:tool-echo))))
        (is (eq :ok (first result)))
        (is (eq :stop (getf (second result) :stop-reason)))
        (let ((tool-message (find :tool (getf (second result) :messages)
                                  :key (lambda (m) (getf m :role)))))
          (is (search "error" (miao:content-text (getf tool-message :content)))))))))

(test an-erroring-tool-comes-back-as-a-tool-message-and-the-run-continues
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (incf n)
                   (if (= n 1)
                       (tool-call-reply "c1" "tool-boom" "{}")
                       (final-reply "recovered")))
                'tool-boom)
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :tools '(:tool-boom))))
        (is (eq :ok (first result)))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (= 2 (length (requests))))
        (let ((tool-message (find :tool (getf (second result) :messages)
                                  :key (lambda (m) (getf m :role)))))
          (is (search "boom" (miao:content-text (getf tool-message :content)))))))))

;;; --- the allow-list -----------------------------------------------------

(test the-allow-list-is-what-reaches-the-wire
  (with-agent ((final-reply "hi") 'tool-echo 'tool-boom)
    (agent-turn :messages '((:role :user :content "hi")) :tools '(:tool-echo))
    (is (equal '("tool-echo") (request-tool-names 1)))))

(test the-default-allow-list-is-the-trust-agent-tools
  (with-agent ((final-reply "hi") 'tool-echo 'tool-boom)
    (agent-turn :messages '((:role :user :content "hi")))
    (is (equal '("tool-boom" "tool-echo") (sort (request-tool-names 1) #'string<)))))

;;; --- budgets --------------------------------------------------------

(test max-turns-stops-a-model-that-keeps-calling-tools
  (with-agent ((lambda (&rest request)
                 (declare (ignore request))
                 (tool-call-reply "c1" "tool-echo" "{\"text\":\"again\"}"))
              'tool-echo)
    (let ((result (agent-turn :messages '((:role :user :content "go"))
                              :tools '(:tool-echo) :max-turns 2)))
      (is (eq :ok (first result)))
      (is (eq :max-turns (getf (second result) :stop-reason)))
      (is (= 2 (getf (second result) :turns))))))

(test deadline-stops-a-slow-model
  (with-agent ((lambda (&rest request)
                 (declare (ignore request))
                 (sleep 0.3)
                 (final-reply "too late")))
    (let ((result (agent-turn :messages '((:role :user :content "go"))
                              :deadline 100 :timeout 5)))
      (is (eq :ok (first result)))
      (is (eq :timeout (getf (second result) :stop-reason))))))

;;; --- cancel and steer ---------------------------------------------------

(test cancel-mid-run-finishes-cancelled
  (with-agent ((lambda (&rest request)
                 (declare (ignore request))
                 (sleep 0.3)
                 (final-reply "too late")))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed)))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (m:cast child '(:cancel))
        (multiple-value-bind (message received) (m:receive :timeout 5)
          (is-true received)
          (is (eq :agent-done (first message)))
          (is (eq :cancelled (getf (second (fourth message)) :stop-reason))))))))

(test cancel-stops-the-completion-in-flight
  ;; The backend never finishes its answer, so the turn's thread can only
  ;; end because :cancel closed its connection.
  (with-agent ((stalled-stream "application/json" "{\"choices\":["))
    (with-hold
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :turn-timeout 30000)))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually #'completion-running-p))
          (m:cast child '(:cancel))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :cancelled (getf (second (fourth message)) :stop-reason))))
          (is-true (eventually (lambda () (not (completion-running-p))))))))))

(test steer-reaches-the-next-request
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (incf n)
                   (if (= n 1)
                       (tool-call-reply "c1" "tool-echo" "{\"text\":\"hi\"}")
                       (final-reply "done")))
                'tool-echo)
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :tools '(:tool-echo))))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (m:cast child (list :steer :content "also do this"))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :agent-done (first message)))
            (is (find "also do this" (getf (second (fourth message)) :messages)
                     :key (lambda (m) (miao:content-text (getf m :content)))
                     :test #'equal))))))))

;;; --- interrupting steers ---------------------------------------------

(defun delta-chunk (text)
  (format nil "{\"choices\":[{\"delta\":{\"content\":~a}}]}"
          (com.inuoe.jzon:stringify text)))

(defun streamed-reply (text)
  (sse-response (delta-chunk text)
                "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"
                "[DONE]"))

(defun interruptible-backend (release)
  "A backend whose first turn streams \"partial \" and then stalls until
RELEASE's car is set, and whose later turns answer at once. The fake server
serves one connection at a time, so the next request waits on the release."
  (let ((n 0))
    (lambda (&rest request)
      (declare (ignore request))
      (if (= (incf n) 1)
          (list :stall
                (format nil "HTTP/1.1 200 OK~c~cContent-Type: text/event-stream~c~cConnection: close~c~c~c~c~a"
                        #\Return #\Newline #\Return #\Newline #\Return #\Newline
                        #\Return #\Newline (sse-body (delta-chunk "partial ")))
                (lambda () (not (car release))))
          (streamed-reply "done")))))

(defun event-types (events)
  (mapcar (lambda (e) (getf e :type)) events))

(defun interrupt-mid-stream (&rest delegate-args)
  "Run an agent against INTERRUPTIBLE-BACKEND, send an interrupting steer
once its first turn has streamed, and return the :agent-done result and the
sink's events, oldest first."
  (let ((release (list nil))
        (recorder (make-recorder)))
    (with-agent ((interruptible-backend release))
      (unwind-protect
           (m:with-process (runner)
             (let ((child (apply #'m:delegate *ctx* 'miao:agent
                                 :model :provider-test-keyed
                                 :sink (recorder-sink recorder)
                                 delegate-args)))
               (m:cast child (list :run :messages '((:role :user :content "go"))))
               (is-true (eventually (lambda () (recorder-has recorder :text-delta))))
               (m:cast child (list :steer :content "change of plan" :interrupt t))
               (setf (car release) t)
               (multiple-value-bind (message received) (m:receive :timeout 5)
                 (is-true received)
                 (values (fourth message)
                         (recorded-events recorder)
                         (requests)))))
        (setf (car release) t)))))

(test an-interrupting-steer-keeps-the-streamed-text-and-folds-at-once
  (multiple-value-bind (result events requests) (interrupt-mid-stream)
    (is (eq :stop (getf (second result) :stop-reason)))
    (is (eql 2 (getf (second result) :turns)))
    (is (equal '((:assistant . "partial ") (:user . "change of plan") (:assistant . "done"))
               (mapcar (lambda (m) (cons (getf m :role) (miao:content-text (getf m :content))))
                       (last (getf (second result) :messages) 3))))
    (let ((body (getf (second requests) :body)))
      (is (< (search "partial " body) (search "change of plan" body))))
    (is (equal '(:run-start :turn :text-delta :turn-interrupted :steer :turn :text-delta :done :run-done)
               (event-types events)))
    (is (eql 1 (getf (find :turn-interrupted events :key (lambda (e) (getf e :type)))
                     :turn)))
    (let ((steer (find :steer events :key (lambda (e) (getf e :type)))))
      (is (equal "change of plan" (getf steer :content)))
      (is (eq t (getf steer :interrupt))))))

(test an-interrupt-without-a-sink-keeps-nothing
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (if (= (incf n) 1)
                       (progn (sleep 0.5) (final-reply "too late"))
                       (final-reply "done"))))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed)))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually (lambda () (requests))))
          (m:cast child (list :steer :content "change of plan" :interrupt t))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (equal '(:user :user :assistant)
                       (mapcar (lambda (m) (getf m :role))
                               (getf (second (fourth message)) :messages))))
            (is (equal "done" (miao:content-text
                               (getf (second (fourth message)) :content))))))))))

(test an-interrupt-before-the-first-turn-issues-one-turn
  ;; The :STEP that :RUN queues has not run yet, so there is no turn to
  ;; interrupt: the steer folds into the first turn, and only one is issued.
  (with-agent ((final-reply "done"))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed)))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (m:cast child (list :steer :content "early" :interrupt t))
        (multiple-value-bind (message received) (m:receive :timeout 5)
          (is-true received)
          (is (eql 1 (getf (second (fourth message)) :turns)))
          (is (eql 1 (length (requests))))
          (is (search "early" (getf (first (requests)) :body))))))))

(defun tool-calls-reply (&rest calls)
  "A reply calling each of CALLS, (id name arguments-json) lists."
  (json-response
   (format nil "{\"choices\":[{\"message\":{\"role\":\"assistant\",
     \"tool_calls\":[~{~a~^,~}]},\"finish_reason\":\"tool_calls\"}]}"
           (mapcar (lambda (call)
                     (destructuring-bind (id name arguments) call
                       (format nil "{\"id\":\"~a\",\"type\":\"function\",
                         \"function\":{\"name\":\"~a\",\"arguments\":~s}}"
                               id name arguments)))
                   calls))))

(defun in-tool-phase-p (snapshot)
  (getf (getf snapshot :in-flight) :tool-calls))

(defun interrupt-tool-phase (answer tool-classes delegate-args &key (ready #'in-tool-phase-p))
  "Run an agent against ANSWER with TOOL-CLASSES mounted, send an interrupting
steer once READY is true of its snapshot, and return the :agent-done result
and the seconds from the steer to it."
  (call-with-agent
   answer tool-classes
   (lambda (*ctx*)
     (m:with-process (runner)
       (let ((child (apply #'m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                           delegate-args)))
         (m:cast child (list :run :messages '((:role :user :content "go"))))
         (is-true (eventually (lambda () (funcall ready (m:call child '(:snapshot))))))
         (let ((start (get-internal-real-time)))
           (m:cast child (list :steer :content "change of plan" :interrupt t))
           (multiple-value-bind (message received) (m:receive :timeout 5)
             (is-true received)
             (values (fourth message)
                     (/ (- (get-internal-real-time) start)
                        internal-time-units-per-second)))))))))

(defun message-texts (result)
  (mapcar (lambda (m) (cons (getf m :role) (miao:content-text (getf m :content))))
          (getf (second result) :messages)))

(test an-interrupt-during-the-tool-phase-closes-the-calls
  (setf *tool-wait-cancelled* nil)
  (let ((n 0))
    (multiple-value-bind (result seconds)
        (interrupt-tool-phase (lambda (&rest request)
                                (declare (ignore request))
                                (if (= (incf n) 1)
                                    (tool-call-reply "c1" "tool-wait" "{}")
                                    (final-reply "done")))
                              '(tool-wait)
                              '(:tools (:tool-wait)))
      (is (< seconds 2))
      (is (eq :stop (getf (second result) :stop-reason)))
      (is (equal '((:tool . "{\"error\":\"interrupted\"}")
                   (:user . "change of plan")
                   (:assistant . "done"))
                 (last (message-texts result) 3)))
      (is-true (eventually (lambda () *tool-wait-cancelled*))))))

(test an-interrupt-keeps-the-tool-results-already-in
  (let ((n 0))
    (multiple-value-bind (result seconds)
        (interrupt-tool-phase (lambda (&rest request)
                                (declare (ignore request))
                                (if (= (incf n) 1)
                                    (tool-calls-reply '("c1" "tool-echo" "{\"text\":\"kept\"}")
                                                      '("c2" "tool-wait" "{}"))
                                    (final-reply "done")))
                              '(tool-echo tool-wait)
                              '(:tools (:tool-echo :tool-wait))
                              :ready (lambda (snapshot)
                                       (and (in-tool-phase-p snapshot)
                                            (search "kept" (format nil "~s" (getf snapshot :messages))))))
      (is (< seconds 2))
      (let ((tools (remove :tool (getf (second result) :messages)
                           :key (lambda (m) (getf m :role)) :test-not #'eq)))
        (is (equal '("c1" "c2") (mapcar (lambda (m) (getf m :tool-call-id)) tools)))
        (is (search "kept" (miao:content-text (getf (first tools) :content))))
        (is (search "interrupted" (miao:content-text (getf (second tools) :content))))))))

(test an-interrupt-in-the-last-turns-tool-phase-finishes-the-run
  (multiple-value-bind (result seconds)
      (interrupt-tool-phase (tool-call-reply "c1" "tool-wait" "{}")
                            '(tool-wait)
                            '(:tools (:tool-wait) :max-turns 1))
    (is (< seconds 2))
    (is (eq :max-turns (getf (second result) :stop-reason)))
    (is (not (find "change of plan" (message-texts result) :key #'cdr :test #'equal)))))

(test a-busy-tool-does-not-hold-up-the-turn-after-an-interrupt
  ;; TOOL-SLOW ignores its cancel; the next turn must not wait on it.
  (let ((n 0))
    (multiple-value-bind (result seconds)
        (interrupt-tool-phase (lambda (&rest request)
                                (declare (ignore request))
                                (if (= (incf n) 1)
                                    (tool-call-reply "c1" "tool-slow" "{}")
                                    (final-reply "done")))
                              '(tool-slow)
                              '(:tools (:tool-slow)))
      (is (eq :stop (getf (second result) :stop-reason)))
      (is (< seconds 0.8)))))

(test a-late-reply-from-an-interrupted-call-does-not-answer-a-reused-id
  ;; Turn 2 reuses c1, as Ollama's call_N ids do; turn 1's TOOL-SLOW reply
  ;; lands while turn 2's TOOL-WAIT c1 is still outstanding.
  (let ((n 0))
    (let ((result (interrupt-tool-phase (lambda (&rest request)
                                          (declare (ignore request))
                                          (case (incf n)
                                            (1 (tool-call-reply "c1" "tool-slow" "{}"))
                                            (2 (tool-call-reply "c1" "tool-wait" "{}"))
                                            (t (final-reply "done"))))
                                        '(tool-slow tool-wait)
                                        '(:tools (:tool-slow :tool-wait)))))
      (is (eq :stop (getf (second result) :stop-reason)))
      (let ((answer (find :tool (reverse (getf (second result) :messages))
                          :key (lambda (m) (getf m :role)))))
        (is (search "waited" (miao:content-text (getf answer :content))))))))

(test an-interrupt-cancels-a-sub-agent-in-flight
  (let ((n 0))
    (call-with-agent
     (lambda (&rest request)
       (declare (ignore request))
       (case (incf n)
         (1 (tool-call-reply "c1" "agent-task" "{\"task\":\"help\"}"))
         (2 (sleep 1) (final-reply "sub-answer"))
         (t (final-reply "done"))))
     '()
     (lambda (*ctx*)
       (m:with-process (runner)
         (let ((parent (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                   :sub-agents t)))
           (m:cast parent (list :run :messages '((:role :user :content "go"))))
           (let ((sub (other-agent-child *ctx* parent)))
             (is-true sub)
             (m:cast parent (list :steer :content "change of plan" :interrupt t))
             (is-true (eventually (lambda () (not (m:process-alive-p sub)))))
             (multiple-value-bind (message received) (m:receive :timeout 5)
               (is-true received)
               (let ((texts (message-texts (fourth message))))
                 (is (not (find "sub-answer" texts :key #'cdr :test #'search)))
                 (is (equal '(:tool . "{\"error\":\"interrupted\"}")
                            (find :tool texts :key #'car))))))))))))

(test cancel-stops-a-dispatched-tool-call
  (setf *tool-wait-cancelled* nil)
  (with-agent ((tool-call-reply "c1" "tool-wait" "{}") 'tool-wait)
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                               :tools '(:tool-wait))))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (is-true (eventually
                  (lambda ()
                    (getf (getf (m:call child '(:snapshot)) :in-flight) :tool-calls))))
        (m:cast child '(:cancel))
        (is-true (nth-value 1 (m:receive :timeout 5)))
        (is-true (eventually (lambda () *tool-wait-cancelled*)))))))

(test cancel-mid-tool-call-closes-the-call
  ;; A sink makes the request stream, so the backend answers in SSE.
  (with-agent ((sse-response
                "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"c1\",\"function\":{\"name\":\"tool-hold\",\"arguments\":\"{}\"}}]}}]}"
                "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
                "[DONE]")
               'tool-hold)
    (m:with-process (runner)
      (let* ((recorder (make-recorder))
             (child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                :tools '(:tool-hold)
                                :sink (recorder-sink recorder))))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (loop repeat 100
              until (getf (getf (m:call child '(:snapshot)) :in-flight) :tool-calls)
              do (sleep 0.02))
        (m:cast child '(:cancel))
        (multiple-value-bind (message received) (m:receive :timeout 5)
          (is-true received)
          (let* ((messages (getf (second (fourth message)) :messages))
                 (last-message (car (last messages))))
            (is (eq :tool (getf last-message :role)))
            (is (equal "c1" (getf last-message :tool-call-id)))
            (is (search "interrupted" (miao:content-text (getf last-message :content))))
            (let ((event (find :tool-result (recorded-events recorder)
                               :key (lambda (e) (getf e :type)))))
              (is (equal "c1" (getf event :id)))
              (is (equal '(:error :interrupted) (getf event :result))))))))))

;;; --- continuing a conversation ---------------------------------------

(defun restored-conversation ()
  '((:role :system :content "be brief")
    (:role :user :content "first")
    (:role :assistant :content "earlier answer")))

(defun run-on-restored (&rest run-args)
  "Restore a three-message conversation onto a fresh agent, :RUN it with
RUN-ARGS and return the run's result."
  (m:with-process (runner)
    (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                             :system "be brief")))
      (m:call child (list :restore (list :messages (restored-conversation) :turns 3)))
      (m:cast child (list* :run run-args))
      (multiple-value-bind (message received) (m:receive :timeout 5)
        (is-true received)
        (fourth message)))))

(test continue-runs-on-the-restored-conversation
  (with-agent ((final-reply "ok"))
    (let* ((result (run-on-restored :continue t
                                    :messages '((:role :user :content "more"))))
           (messages (getf (second result) :messages)))
      (is (eq :stop (getf (second result) :stop-reason)))
      (is (= 1 (getf (second result) :turns)))
      (is (equal '(:system :user :assistant :user :assistant)
                 (mapcar (lambda (m) (getf m :role)) messages)))
      (is (search "earlier answer" (getf (first (requests)) :body)))
      (is (search "more" (getf (first (requests)) :body))))))

(test a-snapshot-keeps-its-messages-when-the-conversation-grows
  (with-agent ((final-reply "ok"))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed)))
        (m:call child (list :restore (list :messages (restored-conversation) :turns 3)))
        (let* ((snapshot (getf (m:call child (list :snapshot)) :messages))
               (before (copy-tree snapshot)))
          (is (equal (restored-conversation) snapshot))
          (m:cast child (list :run :continue t :messages '((:role :user :content "more"))))
          (is-true (nth-value 1 (m:receive :timeout 5)))
          (is (equal before snapshot)))))))

(test run-without-continue-replaces-the-conversation
  (with-agent ((final-reply "ok"))
    (run-on-restored :messages '((:role :user :content "fresh")))
    (is (not (search "earlier answer" (getf (first (requests)) :body))))
    (is (search "fresh" (getf (first (requests)) :body)))))

;;; --- forking (~takeiteasy/miao#74) ------------------------------------

(defun tooled-conversation ()
  "system, user, an assistant turn with a call and its result, user, assistant."
  (list (list :role :system :content "be brief")
        (list :role :user :content "first")
        (list :role :assistant :content nil
              :tool-calls (list (list :id "c1" :name :tool-echo :arguments nil)))
        (list :role :tool :tool-call-id "c1" :content "{}")
        (list :role :user :content "second")
        (list :role :assistant :content "answer")))

(defun roles-of (messages) (mapcar (lambda (m) (getf m :role)) messages))

(test fork-conversation-keeps-a-prefix-at-a-message-index
  (let ((messages (tooled-conversation)))
    (multiple-value-bind (prefix turns) (miao:fork-conversation messages :at 2)
      (is (equal '(:system :user) (roles-of prefix)))
      (is (= 0 turns)))
    (is (equal '(:system :user :assistant :tool) (roles-of (miao:fork-conversation messages :at 4))))
    (is (= 6 (length (miao:fork-conversation messages))))
    (is (null (miao:fork-conversation messages :at 0)))))

(test fork-conversation-refuses-a-cut-inside-a-tool-call-turn
  (signals error (miao:fork-conversation (tooled-conversation) :at 3))
  (signals error (miao:fork-conversation (tooled-conversation) :at 7))
  (signals error (miao:fork-conversation (tooled-conversation) :at -1)))

(test fork-conversation-cuts-at-a-turn
  (let ((messages (tooled-conversation)))
    (multiple-value-bind (prefix turns) (miao:fork-conversation messages :turn 1)
      (is (equal '(:system :user :assistant :tool) (roles-of prefix)))
      (is (= 1 turns)))
    (is (equal '(:system :user) (roles-of (miao:fork-conversation messages :turn 0))))
    (is (= 6 (length (miao:fork-conversation messages :turn 2))))
    (signals error (miao:fork-conversation messages :turn 3))))

(test fork-conversation-takes-one-cut-and-leaves-its-input-alone
  (let* ((messages (tooled-conversation))
         (before (copy-tree messages)))
    (signals error (miao:fork-conversation messages :at 1 :turn 1))
    (miao:fork-conversation messages :at 2)
    (is (equal before messages))))

(defun mount-source-agent (&rest initargs)
  (apply #'m:mount *ctx* 'miao:agent :name :source :model :provider-test-keyed initargs)
  (m:call (m:lookup :source)
          (list :restore (list :messages (tooled-conversation) :turns 2))))

(test a-fork-continues-from-the-cut-and-leaves-the-source-alone
  (with-agent ((final-reply "branch") 'tool-echo)
    (mount-source-agent)
    (let ((before (copy-tree (m:call (m:lookup :source) '(:snapshot)))))
      (is (eq :branch (miao:fork-agent *ctx* :source :turn 1 :as :branch)))
      (m:cast (m:lookup :branch)
              (list :run :continue t :messages '((:role :user :content "instead"))))
      (is-true (eventually (lambda () (plusp (length (getf (first (requests)) :body))))))
      (let ((sent (getf (first (requests)) :body)))
        (is (search "first" sent))
        (is (search "instead" sent))
        (is (not (search "answer" sent))))
      (is (equal before (m:call (m:lookup :source) '(:snapshot)))))))

(test two-forks-from-one-point-carry-only-the-prefix
  (with-agent ((final-reply "branch") 'tool-echo)
    (mount-source-agent)
    (miao:fork-agent *ctx* :source :at 2 :as :one)
    (miao:fork-agent *ctx* :source :at 2 :as :two)
    (m:cast (m:lookup :one) (list :run :continue t :messages '((:role :user :content "left"))))
    (m:cast (m:lookup :two) (list :run :continue t :messages '((:role :user :content "right"))))
    (is-true (eventually (lambda () (= 2 (length (requests))))))
    (let ((bodies (mapcar (lambda (r) (getf r :body)) (requests))))
      (is (= 1 (count-if (lambda (b) (search "left" b)) bodies)))
      (is (= 1 (count-if (lambda (b) (search "right" b)) bodies)))
      (is (every (lambda (b) (and (search "first" b) (not (search "answer" b)))) bodies)))
    (is (= 6 (length (getf (m:call (m:lookup :source) '(:snapshot)) :messages))))))

(test forking-a-running-agent-closes-its-unanswered-calls
  (with-agent ((tool-call-reply "c1" "tool-hold" "{}") 'tool-hold)
    (m:mount *ctx* 'miao:agent :name :source :model :provider-test-keyed
                               :tools '(:tool-hold))
    (m:cast (m:lookup :source) (list :run :messages '((:role :user :content "go"))))
    (is-true (eventually
              (lambda ()
                (getf (getf (m:call (m:lookup :source) '(:snapshot)) :in-flight) :tool-calls))))
    (miao:fork-agent *ctx* :source :as :branch)
    (let ((messages (getf (m:call (m:lookup :branch) '(:snapshot)) :messages)))
      (is (eq :tool (getf (car (last messages)) :role)))
      (is (search "interrupted" (miao:content-text (getf (car (last messages)) :content)))))
    (is-true (m:process-alive-p (m:lookup :source)))))

(test fork-agent-refuses-a-bad-name
  (with-agent ((final-reply "ok") 'tool-echo)
    (mount-source-agent)
    (signals error (miao:fork-agent *ctx* :nobody :as :branch))
    (signals error (miao:fork-agent *ctx* :tool-echo :as :branch))
    (signals error (miao:fork-agent *ctx* :source :as :source))
    (signals error (miao:fork-agent *ctx* :source))))

(test a-system-prompt-is-not-sent-twice
  (with-agent ((final-reply "ok"))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                                 :system "be brief")))
        (m:cast child (list :run :messages (restored-conversation)))
        (multiple-value-bind (message received) (m:receive :timeout 5)
          (is-true received)
          (is (= 1 (count :system (roles-of (getf (second (fourth message)) :messages))))))))))

;;; --- events ---------------------------------------------------------

(test stream-events-reach-the-sink-in-order
  (with-agent ((sse-response
                "{\"choices\":[{\"delta\":{\"content\":\"hi \"}}]}"
                "{\"choices\":[{\"delta\":{\"content\":\"there\"}}]}"
                "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"
                "[DONE]"))
    (let ((recorder (make-recorder)))
      (agent-turn :messages '((:role :user :content "hi"))
                  :sink (recorder-sink recorder))
      ;; :TEXT-DELTA/:DONE are the protocol's own, passed straight through
      ;; because :STREAM is handed down in the request; :TURN and :RUN-DONE
      ;; are the loop's.
      (is (equal '(:run-start :turn :text-delta :text-delta :done :run-done)
                 (event-types (recorded-events recorder)))))))

;;; A failed turn ends the sink's turn with a failed :DONE, ahead of the loop's
;;; own :RUN-DONE.
(test a-failed-streamed-turn-emits-a-failed-done-before-run-done
  (with-agent ('(500 ("Content-Type" "application/json") "{\"error\":\"boom\"}"))
    (let* ((recorder (make-recorder))
           (result (agent-turn :messages '((:role :user :content "hi"))
                               :sink (recorder-sink recorder)))
           (events (recorded-events recorder)))
      (is (eq :backend-error (first (miao:tool-error result))))
      (is (equal '(:run-start :turn :done :run-done)
                 (mapcar (lambda (e) (getf e :type)) events)))
      (is (miao:tool-error-p (getf (third events) :reason))))))

;;; A function sink is called from one emitter per agent, a sub-agent's
;;; events included, one event at a time, so a sink that blocks never holds up
;;; the loop.

(defun sse-tool-call (id name arguments-json)
  (sse-response
   (format nil "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"~a\",~
     \"function\":{\"name\":\"~a\",\"arguments\":~s}}]}}]}"
           id name arguments-json)
   "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
   "[DONE]"))

(test every-event-reaches-a-function-sink-one-at-a-time
  (let ((n 0)
        (recorder (make-recorder)))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (case (incf n)
                     (1 (sse-tool-call "c1" "agent-task" "{\"task\":\"help\"}"))
                     (2 (streamed-reply "sub-answer"))
                     (t (streamed-reply "done")))))
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :sub-agents t
                                :sink (recorder-sink recorder))))
        (is (eq :stop (getf (second result) :stop-reason)))
        (let ((events (recorded-events recorder)))
          (is (= 2 (count :run-done events :key (lambda (e) (getf e :type)))))
          (is (eq :run-done (getf (car (last events)) :type)))
          (is (not (consp (getf (car (last events)) :ref))))
          (is-false (recorder-overlapped recorder)))))))

(test a-blocking-sink-holds-up-neither-the-run-nor-cancel
  (let ((stuck (bt:make-semaphore))
        (grace miao::*sink-grace*))
    (setf miao::*sink-grace* 0.3)
    (unwind-protect
         (with-agent ((sse-tool-call "c1" "tool-wait" "{}") 'tool-wait)
           (m:with-process (runner)
             (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                      :tools '(:tool-wait)
                                      :sink (lambda (event)
                                              (declare (ignore event))
                                              (bt:wait-on-semaphore stuck :timeout 60)))))
               (m:cast child (list :run :messages '((:role :user :content "go"))))
               (is-true (eventually
                         (lambda ()
                           (getf (getf (m:call child '(:snapshot)) :in-flight) :tool-calls))))
               (m:cast child '(:cancel))
               (multiple-value-bind (message received) (m:receive :timeout 5)
                 (is-true received)
                 (is (eq :cancelled (getf (second (fourth message)) :stop-reason))))
               (is-true (eventually #'sinks-idle-p 5)))))
      (setf miao::*sink-grace* grace)
      (bt:signal-semaphore stuck :count 100))))

(test a-cancelled-turn-streams-nothing-after-run-done
  (let ((release (list nil))
        (recorder (make-recorder)))
    (with-agent ((interruptible-backend release))
      (unwind-protect
           (m:with-process (runner)
             (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                      :sink (recorder-sink recorder))))
               (m:cast child (list :run :messages '((:role :user :content "go"))))
               (is-true (eventually (lambda () (recorder-has recorder :text-delta))))
               (m:cast child '(:cancel))
               (is-true (nth-value 1 (m:receive :timeout 5)))
               (is (equal '(:run-start :turn :text-delta :run-done)
                          (event-types (recorded-events recorder))))))
        (setf (car release) t)))))

(test a-restore-mid-turn-streams-nothing-more-from-that-turn
  (let ((release (list nil))
        (recorder (make-recorder)))
    (with-agent ((interruptible-backend release))
      (unwind-protect
           (m:with-process (runner)
             (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                      :sink (recorder-sink recorder))))
               (m:cast child (list :run :messages '((:role :user :content "go"))))
               (is-true (eventually (lambda () (recorder-has recorder :text-delta))))
               (m:call child (list :restore (list :messages '((:role :user :content "go"))
                                                  :turns 0)))
               (setf (car release) t)
               (m:cast child (list :run :continue t))
               (is-true (nth-value 1 (m:receive :timeout 5)))
               (is (equal '(:run-start :turn :text-delta :run-start :turn :text-delta :done :run-done)
                          (event-types (recorded-events recorder))))))
        (setf (car release) t)))))

;;; --- sub-agents ----------------------------------------------------

(test a-sub-agent-result-reaches-the-parent-as-a-tool-message
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (incf n)
                   (cond
                     ((= n 1) (tool-call-reply "c1" "agent-task" "{\"task\":\"help\"}"))
                     ((= n 2) (final-reply "sub-answer"))
                     (t (final-reply "done")))))
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :sub-agents t)))
        (is (eq :ok (first result)))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (= 3 (length (requests))))
        (let ((tool-message (find :tool (getf (second result) :messages)
                                  :key (lambda (m) (getf m :role)))))
          (is (search "sub-answer" (miao:content-text (getf tool-message :content)))))))))

(defun other-agent-child (context exclude)
  "Poll CONTEXT's children for a mounted AGENT that is not EXCLUDE, up to
2s -- delegation happens on the parent's own process, asynchronously to the
test's."
  (loop repeat 100
        for found = (find-if (lambda (c)
                                (and (eq (getf c :class) 'miao:agent)
                                     (not (eq (getf c :process) exclude))))
                              (m:children context))
        when found return (getf found :process)
        do (sleep 0.02)))

(test a-crashed-sub-agent-comes-back-as-an-error-tool-message
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (incf n)
                   (if (= n 1)
                       (tool-call-reply "c1" "agent-task" "{\"task\":\"help\"}")
                       ;; Slow, so there is a window to unmount the child
                       ;; before its own turn would otherwise finish it.
                       (progn (sleep 0.5) (final-reply "recovered")))))
      (m:with-process (runner)
        (let ((parent (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                  :sub-agents t)))
          (m:cast parent (list :run :messages '((:role :user :content "go"))))
          (let ((sub (other-agent-child *ctx* parent)))
            (is-true sub)
            ;; UNMOUNT stops the sub-agent for :shutdown, which is the
            ;; :agent-down path -- the one raw shape ~takeiteasy/meow#59's
            ;; fallback exists for.
            (m:unmount *ctx* sub)
            (multiple-value-bind (message received) (m:receive :timeout 5)
              (is-true received)
              (is (eq :agent-done (first message)))
              (let ((tool-message (find :tool (getf (second (fourth message)) :messages)
                                        :key (lambda (m) (getf m :role)))))
                (is (search "sub_agent_down"
                           (miao:content-text (getf tool-message :content))))))))))))

;;; --- discovery -----------------------------------------------------

(test a-mounted-agent-is-discoverable-and-describes-itself
  (with-agent ((final-reply "hi"))
    (m:mount *ctx* 'miao:agent :name :agent-under-test :model :provider-test-keyed)
    (is (member :agent-under-test (miao:agents) :test #'equal))
    (let ((metadata (miao:describe-agent :agent-under-test)))
      (is (eq :agent (getf metadata :kind)))
      (is (eq :provider-test-keyed (getf metadata :model))))))
;;; --- the tool cap ------------------------------------------------------

(defun tool-message-texts (result)
  (mapcar #'cdr (remove :tool (message-texts result) :key #'car :test-not #'eq)))

(test queued-tool-calls-dispatch-as-slots-free
  (reset-gate)
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (if (= (incf n) 1)
                       (tool-calls-reply '("c1" "tool-gate" "{}") '("c2" "tool-gate-b" "{}")
                                         '("c3" "tool-gate" "{}"))
                       (final-reply "done")))
                 'tool-gate 'tool-gate-b)
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :tools '(:tool-gate :tool-gate-b) :max-parallel-tools 1)))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (= 1 *gate-peak*))
        (is (= 3 (count-if (lambda (text) (search "gated" text))
                           (tool-message-texts result))))))))

(test without-a-cap-tool-calls-run-together
  (reset-gate)
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (if (= (incf n) 1)
                       (tool-calls-reply '("c1" "tool-gate" "{}") '("c2" "tool-gate-b" "{}"))
                       (final-reply "done")))
                 'tool-gate 'tool-gate-b)
      (agent-turn :messages '((:role :user :content "go")) :tools '(:tool-gate :tool-gate-b))
      (is (= 2 *gate-peak*)))))

(defun wait-then-echo (&rest request)
  (declare (ignore request))
  (tool-calls-reply '("c1" "tool-wait" "{}") '("c2" "tool-echo" "{\"text\":\"ran\"}")))

(test an-interrupt-closes-queued-calls-too
  (let ((n 0))
    (multiple-value-bind (result seconds)
        (interrupt-tool-phase (lambda (&rest request)
                                (if (= (incf n) 1)
                                    (apply #'wait-then-echo request)
                                    (final-reply "done")))
                              '(tool-wait tool-echo)
                              '(:tools (:tool-wait :tool-echo) :max-parallel-tools 1))
      (is (< seconds 2))
      (is (eq :stop (getf (second result) :stop-reason)))
      (is (equal '("{\"error\":\"interrupted\"}" "{\"error\":\"interrupted\"}")
                 (tool-message-texts result))))))

(test cancel-closes-queued-calls
  (with-agent (#'wait-then-echo 'tool-wait 'tool-echo)
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                               :tools '(:tool-wait :tool-echo) :max-parallel-tools 1)))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (is-true (eventually (lambda () (in-tool-phase-p (m:call child '(:snapshot))))))
        (m:cast child '(:cancel))
        (multiple-value-bind (message received) (m:receive :timeout 5)
          (is-true received)
          (let ((result (fourth message)))
            (is (eq :cancelled (getf (second result) :stop-reason)))
            (is (equal '("{\"error\":\"interrupted\"}" "{\"error\":\"interrupted\"}")
                       (tool-message-texts result)))))))))

(test a-snapshot-closes-queued-calls-as-interrupted
  (with-agent (#'wait-then-echo 'tool-wait 'tool-echo)
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                               :tools '(:tool-wait :tool-echo) :max-parallel-tools 1)))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (is-true (eventually (lambda () (in-tool-phase-p (m:call child '(:snapshot))))))
        (let ((snapshot (m:call child '(:snapshot))))
          (is (equal '("c1" "c2") (in-tool-phase-p snapshot)))
          (is (= 2 (count :tool (getf snapshot :messages)
                          :key (lambda (m) (getf m :role))))))
        (m:cast child '(:cancel))
        (is-true (nth-value 1 (m:receive :timeout 5)))))))

(test a-disallowed-call-takes-no-slot
  (reset-gate)
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (if (= (incf n) 1)
                       (tool-calls-reply '("c1" "tool-boom" "{}") '("c2" "tool-gate" "{}"))
                       (final-reply "done")))
                 'tool-gate)
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :tools '(:tool-gate) :max-parallel-tools 1)))
        (is (eq :stop (getf (second result) :stop-reason)))
        (destructuring-bind (refused gated) (tool-message-texts result)
          (is (search "allow-list" refused))
          (is (search "gated" gated)))))))

(test a-sub-agent-holds-a-slot-for-its-run
  (reset-gate)
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (case (incf n)
                     (1 (tool-calls-reply '("c1" "agent-task" "{\"task\":\"help\"}")
                                          '("c2" "tool-gate" "{}")))
                     ;; The gate would be running by now, were it not queued.
                     (2 (sleep 0.3)
                        (if (zerop *gate-peak*)
                            (final-reply "gate still queued")
                            (final-reply "gate ran alongside")))
                     (t (final-reply "done"))))
                 'tool-gate)
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :tools '(:tool-gate) :sub-agents t :max-parallel-tools 1)))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (search "gate still queued" (first (tool-message-texts result))))
        (is (search "gated" (second (tool-message-texts result))))))))

(test max-parallel-tools-must-be-a-positive-integer
  (with-agent ((final-reply "hi"))
    (signals error (agent-turn :messages '((:role :user :content "go"))
                               :max-parallel-tools 0))))

;;; --- replies as messages --------------------------------------------------

(test an-unregistered-model-fails-the-run-at-once
  (with-agent ((final-reply "unused"))
    (let* ((start (get-internal-real-time))
           (result (miao:run-agent *ctx* :model :no-such-model :deadline 20000
                                         :messages '((:role :user :content "go")))))
      (is (eq :error (first result)))
      (is (< (elapsed-since start) 5)))))

(test a-call-its-schema-refuses-comes-back-as-a-tool-message
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (if (= (incf n) 1)
                       (tool-call-reply "c1" "tool-echo" "{}")
                       (final-reply "recovered")))
                 'tool-echo)
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :tools '(:tool-echo))))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (search "error" (first (tool-message-texts result))))))))

;;; A tool that runs an agent of its own, which calls a tool in turn.

(m:defservice tool-nested () () (:name :tool-nested))

(defmethod m:metadata ((service tool-nested))
  (list :kind :tool :name :tool-nested :trust :agent
        :summary "Run an agent of its own" :params nil))

(miao::define-tool-handler tool-nested (service args)
  args
  (let ((result (miao:run-agent (m:service-process (m:service-context service))
                                :model :provider-test-keyed :tools '(:tool-echo)
                                :messages '((:role :user :content "inner")))))
    (if (miao::tool-error-p result)
        result
        (miao::ok :answer (miao:content-text (getf (second result) :content))))))

(test a-tool-that-runs-an-agent-completes
  (let ((n 0))
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (case (incf n)
                     (1 (tool-call-reply "outer" "tool-nested" "{}"))
                     (2 (tool-call-reply "inner" "tool-echo" "{\"text\":\"hi\"}"))
                     (3 (final-reply "inner done"))
                     (t (final-reply "outer done"))))
                 'tool-nested 'tool-echo)
      (let* ((start (get-internal-real-time))
             (result (agent-turn :messages '((:role :user :content "go"))
                                 :tools '(:tool-nested))))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (search "inner done" (first (tool-message-texts result))))
        (is (< (elapsed-since start) 10))))))

;;; --- retrying a turn (~takeiteasy/miao#42) --------------------------------

(defun error-reply (status)
  (list status '("Content-Type" "application/json") "{\"error\":\"nope\"}"))

(defun fails-then (n-failures status success)
  "A backend answering STATUS to its first N-FAILURES requests, SUCCESS after."
  (let ((n 0))
    (lambda (&rest request)
      (declare (ignore request))
      (if (<= (incf n) n-failures) (error-reply status) success))))

(test a-transient-failure-is-retried-and-the-run-carries-on
  (with-agent ((fails-then 1 500 (final-reply "recovered")))
    (let ((result (agent-turn :messages '((:role :user :content "go"))
                              :turn-retries 2 :retry-backoff 5)))
      (is (eq :stop (getf (second result) :stop-reason)))
      (is (= 1 (getf (second result) :turns)))
      (is (= 2 (length (requests))))
      (is (equal "recovered" (miao:content-text (getf (second result) :content)))))))

(test a-failure-past-the-retries-ends-the-run-with-that-error
  (with-agent ((fails-then 10 503 nil))
    (let ((result (agent-turn :messages '((:role :user :content "go"))
                              :turn-retries 2 :retry-backoff 5)))
      (is (eq :backend-error (first (miao:tool-error result))))
      (is (= 503 (second (miao:tool-error result))))
      (is (= 3 (length (requests)))))))

(test a-failure-a-retry-cannot-fix-is-not-retried
  (with-agent ((fails-then 10 400 nil))
    (let ((result (agent-turn :messages '((:role :user :content "go"))
                              :turn-retries 2 :retry-backoff 5)))
      (is (eq :backend-error (first (miao:tool-error result))))
      (is (= 1 (length (requests)))))))

(test a-turn-is-not-retried-by-default
  (with-agent ((fails-then 10 500 nil))
    (let ((result (agent-turn :messages '((:role :user :content "go")))))
      (is (eq :backend-error (first (miao:tool-error result))))
      (is (= 1 (length (requests)))))))

(test retryable-p-takes-the-transient-shapes-only
  (flet ((retryable (reason) (miao::retryable-p (miao::fail reason))))
    (is-true (retryable :unavailable))
    (is-true (retryable '(:backend-error 408 "")))
    (is-true (retryable '(:backend-error 429 "")))
    (is-true (retryable '(:backend-error 503 "")))
    (is-true (retryable '(:backend-error 200 "the stream ended before the turn did")))
    (is-false (retryable '(:backend-error 400 "")))
    (is-false (retryable '(:backend-error 401 "")))
    (is-false (retryable :timeout))
    (is-false (retryable :cancelled))
    (is-false (retryable '(:bad-request "no")))
    (is-false (retryable '(:error "No model registered")))))

(test retry-delay-waits-for-the-longer-of-backoff-and-retry-after
  (flet ((delay (backoff attempt retry-after)
           (miao::retry-delay backoff attempt retry-after)))
    (dotimes (i 20)
      (is (<= 1000 (delay 1000 1 nil) 1250))
      (is (<= 4000 (delay 1000 3 nil) 5000))
      (is (<= 5000 (delay 1000 1 5000) 6250))
      (is (<= 4000 (delay 1000 3 1000) 5000)))))

(test retry-after-reads-a-backend-error-reason
  (is (= 2000 (miao::retry-after '(:backend-error 429 "" :retry-after 2000))))
  (is (null (miao::retry-after '(:backend-error 429 ""))))
  (is (null (miao::retry-after :unavailable))))

(test a-retry-is-announced-and-is-not-a-new-turn
  (let ((recorder (make-recorder)))
    (with-agent ((fails-then 1 500 (streamed-reply "ok")))
      (let ((result (agent-turn :messages '((:role :user :content "go"))
                                :turn-retries 2 :retry-backoff 5
                                :sink (recorder-sink recorder))))
        (is (eq :stop (getf (second result) :stop-reason)))
        (let* ((events (recorded-events recorder))
               (retry (find :turn-retry events :key (lambda (e) (getf e :type)))))
          (is (equal '(:run-start :turn :done :turn-retry) (subseq (event-types events) 0 4)))
          (is (= 1 (count :turn events :key (lambda (e) (getf e :type)))))
          (is (eq :run-done (getf (car (last events)) :type)))
          (is (= 1 (getf retry :attempt)))
          (is (= 1 (getf retry :turn)))
          (is (eq :backend-error (first (getf retry :reason)))))))))

(test a-cancel-during-the-backoff-sends-nothing-more
  (let ((recorder (make-recorder)))
    (with-agent ((fails-then 10 500 nil))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :turn-retries 2 :retry-backoff 300
                                 :sink (recorder-sink recorder))))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually (lambda () (recorder-has recorder :turn-retry))))
          (m:cast child '(:cancel))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :cancelled (getf (second (fourth message)) :stop-reason))))
          (sleep 0.6)
          (is (= 1 (length (requests)))))))))

(test a-steer-during-the-backoff-folds-into-the-retry
  (let ((recorder (make-recorder)))
    (with-agent ((fails-then 1 500 (streamed-reply "ok")))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :turn-retries 2 :retry-backoff 300
                                 :sink (recorder-sink recorder))))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually (lambda () (recorder-has recorder :turn-retry))))
          (m:cast child (list :steer :content "change of plan"))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :stop (getf (second (fourth message)) :stop-reason)))
            (is (= 1 (getf (second (fourth message)) :turns))))
          (is (= 2 (length (requests))))
          (is-false (search "change of plan" (getf (first (requests)) :body)))
          (is (search "change of plan" (getf (second (requests)) :body))))))))

(test an-interrupting-steer-during-the-backoff-retries-at-once
  (let ((recorder (make-recorder)))
    (with-agent ((fails-then 1 500 (streamed-reply "ok")))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :turn-retries 2 :retry-backoff 5000
                                 :sink (recorder-sink recorder))))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually (lambda () (recorder-has recorder :turn-retry))))
          (m:cast child (list :steer :content "change of plan" :interrupt t))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :stop (getf (second (fourth message)) :stop-reason)))
            (is (= 1 (getf (second (fourth message)) :turns))))
          (is (search "change of plan" (getf (second (requests)) :body))))))))

;;; --- capping a tool result (~takeiteasy/miao#40) --------------------------

(defun long-text-backend ()
  (let ((n 0))
    (lambda (&rest request)
      (declare (ignore request))
      (if (= (incf n) 1)
          (sse-tool-call "c1" "tool-echo"
                         (format nil "{\"text\":\"~a\"}" (make-string 200 :initial-element #\x)))
          (streamed-reply "done")))))

;;; The cut is made per request, so what the conversation holds and what the
;;; sink sees stay whole. Request N's (1-based) `messages` are read back from
;;; the wire.

(defun request-message-contents (n)
  (let ((body (com.inuoe.jzon:parse (getf (nth (1- n) (requests)) :body))))
    (map 'list (lambda (message) (gethash "content" message))
         (gethash "messages" body))))

(test a-capped-tool-result-is-cut-in-the-request-with-a-note
  (let ((recorder (make-recorder)))
    (with-agent ((long-text-backend) 'tool-echo)
      (let* ((result (agent-turn :messages '((:role :user :content "go"))
                                 :max-tool-result 50
                                 :sink (recorder-sink recorder)))
             (sent (car (last (request-message-contents 2))))
             (whole (first (tool-message-texts result))))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (= 50 (search "... [truncated: " sent)))
        (is (search "first 50 kept]" sent))
        (is (not (search "truncated" whole)) "the conversation keeps the whole result")
        (is (> (length whole) 200))
        (let ((event (find :tool-result (recorded-events recorder)
                           :key (lambda (e) (getf e :type)))))
          (is (= 200 (length (getf (second (getf event :result)) :text)))))
        (let ((trimmed (find :context-trimmed (recorded-events recorder)
                             :key (lambda (e) (getf e :type)))))
          (is (equal `((2 :from ,(length whole) :to 50)) (getf trimmed :truncated)))
          (is (null (getf trimmed :omitted))))))))

(test a-tool-result-is-not-cut-by-default
  (let ((recorder (make-recorder)))
    (with-agent ((long-text-backend) 'tool-echo)
      (let ((text (first (tool-message-texts
                          (agent-turn :messages '((:role :user :content "go"))
                                      :sink (recorder-sink recorder))))))
        (is (not (search "truncated" text)))
        (is (> (length text) 200))
        (is (not (recorder-has recorder :context-trimmed)))))))

(test a-result-within-the-cap-is-untouched
  (let ((text (miao::render-tool-result (miao::ok :a 1))))
    (is (equal "{\"a\":1}" (miao::%cut-text text 100)))
    (is (equal "{\"a\":1}" (miao::%cut-text text 7)))))

;;; --- fitting the conversation (~takeiteasy/miao#39) ----------------------

(defun msg (role text &rest more) (list* :role role :content text more))

(defun roles (messages) (mapcar (lambda (m) (getf m :role)) messages))

(defun long-conversation ()
  "A system prompt, an old exchange with a tool call, and a recent one."
  (list (msg :system "be brief")
        (msg :user (make-string 100 :initial-element #\a))
        (msg :assistant "" :tool-calls '((:id "c1" :name :tool-echo :arguments (:text "x"))))
        (msg :tool (make-string 100 :initial-element #\b) :tool-call-id "c1")
        (msg :assistant (make-string 100 :initial-element #\c))
        (msg :user "and now?")))

(test a-conversation-within-budget-is-sent-as-is
  (let ((messages (long-conversation)))
    (multiple-value-bind (view record) (miao::fit-conversation messages :max-context 10000)
      (is (equal messages view))
      (is (null record)))
    (is (null (nth-value 1 (miao::fit-conversation messages))))))

(test the-oldest-turns-are-left-out-with-a-note
  (multiple-value-bind (view record)
      (miao::fit-conversation (long-conversation) :max-context 250)
    (is (equal '(:system :user :assistant :user) (roles view)))
    (is (equal "be brief" (getf (first view) :content)) "a system message is kept")
    (is (search "earlier messages omitted" (getf (second view) :content)))
    (is (equal "and now?" (getf (car (last view)) :content)) "the newest turn is kept")
    (is (equal '(1 2 3) (getf record :omitted)))
    (is-false (getf record :over-budget))
    (is (<= (getf record :size) 250))))

(test a-tool-call-and-its-results-are-left-out-together
  (dolist (budget '(120 150 200 250))
    (let* ((view (miao::fit-conversation (long-conversation) :max-context budget))
           (calls (count-if (lambda (m) (getf m :tool-calls)) view))
           (results (count :tool (roles view))))
      (is (= calls results) "budget ~d: a call is never left without its result" budget))))

(test a-conversation-that-cannot-fit-is-sent-and-said-to-be-over
  (multiple-value-bind (view record)
      (miao::fit-conversation (long-conversation) :max-context 5)
    (is (equal '(:system :user :user) (roles view)) "only the note and the newest turn are left")
    (is-true (getf record :over-budget))
    (is (equal "and now?" (getf (car (last view)) :content)))))

(test fitting-a-conversation-changes-nothing-it-was-given
  (let* ((messages (long-conversation)) (copy (copy-tree messages)))
    (miao::fit-conversation messages :max-context 100 :max-tool-result 10)
    (is (equal copy messages))))

(test the-budget-is-in-tokens-at-the-ratio-with-a-margin
  (let ((messages (long-conversation)))
    (multiple-value-bind (view record)
        (miao::fit-conversation messages :max-context 250 :chars-per-token 4)
      (is (equal messages view) "250 tokens at 4 characters each holds 1000")
      (is (null record)))
    (multiple-value-bind (view record)
        (miao::fit-conversation messages :max-context 250 :margin 9/10)
      (is (equal '(:system :user :assistant :user) (roles view)))
      (is (<= (getf record :size) 225) "a tenth of the budget is held back")
      (is (= 1 (getf record :ratio))))))

(test reserved-characters-count-against-the-budget
  (let ((messages (long-conversation)))
    (is (null (nth-value 1 (miao::fit-conversation messages :max-context 400))))
    (is (equal '(1 2 3)
               (getf (nth-value 1 (miao::fit-conversation messages :max-context 400
                                                          :reserved 200))
                     :omitted)))))

(test fitting-answers-the-characters-the-request-measures
  (let ((messages (long-conversation)))
    (is (= (+ 8 100 (miao::%printed-size (getf (third messages) :tool-calls)) 100 100 8 100)
           (nth-value 2 (miao::fit-conversation messages :reserved 100))))))

(defun usage-reply (reply tokens)
  "REPLY, a whole reply from JSON-RESPONSE, with a usage object of TOKENS."
  (let ((json (third reply)))
    (json-response
     (format nil "~a,\"usage\":{\"prompt_tokens\":~d}}"
             (subseq json 0 (1- (length json))) tokens))))

(defun calibration-backend (tokens &key (usage t))
  "A tool call and then a final answer, each reporting TOKENS prompt tokens
when USAGE."
  (let ((n 0))
    (lambda (&rest request)
      (declare (ignore request))
      (let ((reply (if (= (incf n) 1)
                       (tool-call-reply "c1" "tool-echo" "{\"text\":\"x\"}")
                       (final-reply "done"))))
        (if usage (usage-reply reply tokens) reply)))))

(defun calibration-run (backend)
  "Two turns over a 600-character history, budgeted at 500 tokens, and the
first message of each request as the backend saw it."
  (with-agent (backend 'tool-echo)
    (let ((result (agent-turn
                   :messages (loop for c in '(#\a #\b #\c)
                                   collect (msg :user (make-string 200 :initial-element c)))
                   :max-context 500)))
      (is (equal '(:ok :stop 2) (list (first result) (getf (second result) :stop-reason)
                                      (getf (second result) :turns))))
      (list (first (request-message-contents 1)) (first (request-message-contents 2))))))

(defun noted-p (content)
  (search "earlier messages omitted" content))

(test the-ratio-is-calibrated-from-the-last-replys-prompt-tokens
  (destructuring-bind (first second) (calibration-run (calibration-backend 600))
    (is (not (noted-p first)) "the first turn is measured at the default ratio")
    (is (noted-p second) "then at the characters per token the backend's count implies")))

(test a-reply-with-no-usage-leaves-the-ratio
  (destructuring-bind (first second) (calibration-run (calibration-backend 0 :usage nil))
    (is (not (noted-p first)))
    (is (not (noted-p second)))))

(test an-implausible-prompt-count-is-ignored
  (destructuring-bind (first second) (calibration-run (calibration-backend 1))
    (is (not (noted-p first)))
    (is (not (noted-p second)))))

(defun calibrated (chars tokens)
  (let ((service (make-instance 'miao:agent)))
    (setf (miao::%last-request-chars service) chars)
    (miao::calibrate service (list :meta (list :usage (list :prompt-tokens tokens))))
    (miao::%chars-per-token service)))

(test calibration-sets-characters-over-tokens-within-bounds
  (is (= 4 (calibrated 400 100)))
  (is (= 3 (calibrated 400 1)) "over 8 characters per token")
  (is (= 3 (calibrated 40 100)) "under 1 character per token")
  (is (= 3 (calibrated nil 100)) "no request measured"))

(test the-ratio-is-in-the-metadata
  (with-agent ((chatty-backend))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                               :chars-per-token 9/2)))
        (is (= 9/2 (getf (m:call child '(:describe)) :chars-per-token)))))))

(defun chatty-backend ()
  (lambda (&rest request)
    (declare (ignore request))
    (streamed-reply (make-string 100 :initial-element #\y))))

(test a-request-past-the-budget-is-trimmed-and-the-sink-is-told
  (let ((recorder (make-recorder))
        (history (list (msg :user (make-string 100 :initial-element #\a))
                       (msg :assistant (make-string 100 :initial-element #\b))
                       (msg :user "again?"))))
    (with-agent ((chatty-backend))
      (let* ((result (agent-turn :messages history :max-context 100 :chars-per-token 1
                                 :sink (recorder-sink recorder)))
             (sent (request-message-contents 1))
             (event (find :context-trimmed (recorded-events recorder)
                          :key (lambda (e) (getf e :type)))))
        (is (eq :stop (getf (second result) :stop-reason)))
        (is (= 2 (length sent)))
        (is (search "earlier messages omitted" (first sent)))
        (is (equal "again?" (second sent)))
        (is (equal '(0 1) (getf event :omitted)))
        (is (= 1 (getf event :turn)))
        (is (= 100 (getf event :budget)))
        (is (= 1 (getf event :ratio)))
        (is (= 4 (length (getf (second result) :messages)))
            "the result keeps the whole conversation, plus the reply")))))

(test a-request-within-budget-emits-no-trim-event
  (let ((recorder (make-recorder)))
    (with-agent ((chatty-backend))
      (agent-turn :messages '((:role :user :content "hi")) :max-context 5000
                  :sink (recorder-sink recorder))
      (is (not (recorder-has recorder :context-trimmed))))))

(test the-context-budget-is-in-the-metadata
  (with-agent ((chatty-backend))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                               :max-context 1234)))
        (is (= 1234 (getf (m:call child '(:describe)) :max-context)))))))

(test a-tool-calls-schema-does-not-count-toward-the-context-estimate
  (let ((call (list :id "c1" :name :tool-http :arguments '(:url "x")))
        (schema '((:url string :required t :doc "where to fetch"))))
    (is (= (miao::%message-size (list :role :assistant :tool-calls (list call)))
           (miao::%message-size (list :role :assistant
                                      :tool-calls (list (list* :schema schema call))))))))
