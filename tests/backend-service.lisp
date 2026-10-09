(in-package #:miao/tests)
(in-suite :miao)

;;; BACKEND-SERVICE against the fake HTTP backend: how a client protocol or
;;; provider is mounted, what a mount may override, and that a turn survives
;;; the pool job, the sink and the deadline between the caller and the wire.
;;; The wire itself, the provider declaration and the schema are the client's,
;;; and tested there.

(defvar *backend* nil "The fake HTTP server the running test answers from.")

(defun json-response (json)
  (list 200 '("Content-Type" "application/json") json))

(defparameter +hello-reply+
  "{\"id\":\"cmpl-1\",
    \"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"hi there\"},
                  \"finish_reason\":\"stop\"}],
    \"usage\":{\"prompt_tokens\":7,\"completion_tokens\":2}}")

(defparameter +stalled-delta+
  "{\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}")

(defun completion-running-p ()
  "True while a pooled thread is running a completion."
  (plusp (getf (miao:pool-stats 0) :running)))

;;; Every declaration pins a base URL nothing listens on, because the fake
;;; backend's port is only known at run time: each mount overrides it, which
;;; is the same override a remote Ollama host or a proxy uses.

(miao:define-provider :test-keyed
  :protocol :protocol-openai
  :base-url "http://127.0.0.1:1"
  :auth '(:bearer :env "MIAO_TEST_KEY_NEVER_SET")
  :models '("test-model" "test-model-large")
  :summary "A bearer-keyed provider for tests")

(miao:define-provider :test-keyless
  :protocol :protocol-openai
  :base-url "http://127.0.0.1:1"
  :auth :none)

;;; PATH rather than a variable the test sets: no implementation miao runs on
;;; offers a portable SETENV, and PATH is the one variable guaranteed to be
;;; there. What is under test is that the key is read from the variable the
;;; declaration names, not which variable that is.

(miao:define-provider :test-env-keyed
  :protocol :protocol-openai
  :base-url "http://127.0.0.1:1"
  :auth '(:bearer :env "PATH"))

(miao:define-provider :test-echo
  :protocol :protocol-echo
  :base-url "http://127.0.0.1:1")

(miao:define-provider :test-rewriting
  :protocol :protocol-echo
  :base-url "http://127.0.0.1:1"
  :rewrite-response (lambda (result)
                      (if (eq :ok (first result))
                          (list :ok (list* :rewritten t (second result)))
                          result)))

(miao:define-provider :test-stacked
  :protocol :test-echo
  :base-url "http://127.0.0.1:1")

(miao:define-provider :test-orphan
  :protocol :protocol-nobody-registered
  :base-url "http://127.0.0.1:1")

;;; --- the harness ------------------------------------------------------

(defun call-with-backends (answer mounts body)
  "Run BODY with MOUNTS up against a fake backend. Each mount is (name .
initargs); a provider's :base-url is filled in."
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :backends)
                                   :registry registry))
         (server (start-fake-http
                  (lambda (&rest request)
                    (if (functionp answer) (apply answer request) answer)))))
    (setf *backend* server)
    (unwind-protect
         (progn
           (dolist (mount mounts)
             (destructuring-bind (name . initargs) mount
               (if (typep (ci:find-backend name) 'ci:provider)
                   (apply #'mount-backend context name
                          :base-url (fake-http-url server) initargs)
                   (apply #'mount-backend context name initargs))))
           (funcall body context))
      (m:stop context)
      (stop-fake-http server))))

(defmacro with-backends ((answer &rest mounts) &body body)
  "BODY, with CONTEXT bound to the context holding MOUNTS."
  `(call-with-backends ,answer (list ,@mounts) (lambda (context)
                                                (declare (ignorable context))
                                                ,@body)))

(defun keyed (&rest initargs)
  "The bearer-keyed mount. INITARGS come first, since the leftmost initarg is
the one MAKE-INSTANCE takes."
  (list* :test-keyed (append initargs '(:model "test-model" :api-key "sk-secret"))))

(defun turn (name &rest extra)
  (apply #'miao:complete name :messages '((:role :user :content "hello")) extra))

(defun ask (&rest extra)
  "One turn against the fake backend through the OpenAI protocol."
  (apply #'miao:complete :protocol-openai
         :base-url (fake-http-url *backend*)
         :model "test-model"
         :messages '((:role :user :content "hello"))
         extra))

(defun sent-body ()
  "The JSON body of the one request the backend received."
  (com.inuoe.jzon:parse (getf (first (fake-http-requests *backend*)) :body)))

(defun sent-header (name)
  (getf-string (getf (first (fake-http-requests *backend*)) :headers) name))

(defun event-types (events)
  (mapcar (lambda (event) (getf event :type)) events))

;;; --- mounting ---------------------------------------------------------

(test a-protocol-is-discoverable-and-describes-itself
  (with-backends ((json-response +hello-reply+) '(:protocol-openai))
    (is (equal '(:protocol-openai) (miao:protocols)))
    (let ((metadata (miao:describe-protocol :protocol-openai)))
      (is (eq :protocol (getf metadata :kind)))
      (is (eq :protocol-openai (getf metadata :name)))
      (is (stringp (getf metadata :summary))))))

(test a-provider-is-discoverable-and-describes-itself
  (with-backends ((json-response +hello-reply+) (keyed))
    (is (equal '(:test-keyed) (miao:providers)))
    (let ((metadata (miao:describe-provider :test-keyed)))
      (is (eq :provider (getf metadata :kind)))
      (is (eq :test-keyed (getf metadata :name)))
      (is (eq :protocol-openai (getf metadata :protocol)))
      (is (eq :ready (getf metadata :status)))
      (is (equal "MIAO_TEST_KEY_NEVER_SET" (getf (getf metadata :auth) :env))))))

(test metadata-carries-no-key-material
  (with-backends ((json-response +hello-reply+) (keyed))
    (is (null (search "sk-secret"
                      (princ-to-string (miao:describe-provider :test-keyed)))))))

(test a-mount-overrides-the-declaration
  ;; The declaration pins a dead port; only the override makes the turn land.
  (with-backends ((json-response +hello-reply+) (keyed :model "override-model"))
    (let ((metadata (miao:describe-provider :test-keyed)))
      (is (equal (fake-http-url *backend*) (getf metadata :base-url)))
      (is (equal "override-model" (getf metadata :model))))
    (is (eq :ok (first (turn :test-keyed))))
    (is (equal "override-model" (gethash "model" (sent-body))))))

(test a-protocol-mount-supplies-the-base-url-and-model-under-the-request
  (with-backends ((json-response +hello-reply+)
                  (list :protocol-openai :model "mounted-model"))
    (is (eq :ok (first (turn :protocol-openai :base-url (fake-http-url *backend*)))))
    (is (equal "mounted-model" (gethash "model" (sent-body))))))

(test a-mount-of-an-unknown-backend-fails
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :backends)
                                   :registry registry)))
    (unwind-protect
         (signals error (mount-backend context :nobody-registered))
      (m:stop context))))

(test only-a-provider-takes-an-api-key
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :backends)
                                   :registry registry)))
    (unwind-protect
         (signals error (mount-backend context :protocol-openai :api-key "sk-x"))
      (m:stop context))))

;;; --- credentials ------------------------------------------------------

(test bearer-auth-reaches-the-wire
  (with-backends ((json-response +hello-reply+) (keyed))
    (is (eq :ok (first (turn :test-keyed))))
    (is (equal "Bearer sk-secret" (sent-header "authorization")))))

(test a-key-comes-from-the-environment-variable-the-declaration-names
  (with-backends ((json-response +hello-reply+)
                  (list :test-env-keyed :model "test-model"))
    (is (eq :ok (first (turn :test-env-keyed))))
    (is (equal (format nil "Bearer ~a" (uiop:getenv "PATH"))
               (sent-header "authorization")))))

(test a-provider-with-no-key-mounts-unavailable-and-stays-off-the-wire
  ;; It mounts, so discovery lists it and the reason is legible without a
  ;; call; the call itself is a bad request rather than an outage.
  (with-backends ((json-response +hello-reply+)
                  (list :test-keyed :model "test-model"))
    (is (eq :unavailable (getf (miao:describe-provider :test-keyed) :status)))
    (let ((reason (miao:result-error (turn :test-keyed))))
      (is (eq :bad-request (first reason)))
      (is (search "MIAO_TEST_KEY_NEVER_SET" (second reason))))
    (is (null (fake-http-requests *backend*)))))

;;; --- the turn crosses the service -----------------------------------------

(test a-turn-crosses-the-service-unchanged
  (with-backends ((json-response +hello-reply+) (keyed))
    (let ((reply (second (turn :test-keyed))))
      (is (equal "hi there" (miao:content-text (getf reply :content))))
      (is (eq :stop (getf (getf reply :meta) :finish-reason)))
      (is (= 7 (getf (getf (getf reply :meta) :usage) :prompt-tokens))))))

(test a-streamed-turn-ends-in-one-done
  (with-backends ((sse-response
                   "{\"choices\":[{\"delta\":{\"content\":\"hi \"}}]}"
                   "{\"choices\":[{\"delta\":{\"content\":\"there\"}}]}"
                   "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"
                   "[DONE]")
                  (keyed))
    (let* ((events '())
           (result (turn :test-keyed :ref :r1
                         :stream (lambda (event) (push event events)))))
      (is (equal '(:text-delta :text-delta :done) (event-types (reverse events))))
      (is (every (lambda (event) (eq :r1 (getf event :ref))) events))
      (is (eq :stop (getf (first events) :reason)))
      (is (equal "hi there" (miao:content-text (getf (second result) :content)))))))

(test a-backend-error-crosses-the-service-intact
  (with-backends ('(429 ("Content-Type" "application/json")
                    "{\"error\":{\"message\":\"rate limited\"}}")
                  (keyed))
    (let ((reason (miao:result-error (turn :test-keyed))))
      (is (eq :backend-error (first reason)))
      (is (= 429 (second reason)))
      (is (search "rate limited" (third reason))))))

(test a-malformed-request-never-leaves-the-service
  (with-backends ((json-response +hello-reply+) (keyed))
    (is (eq :bad-request
            (first (miao:result-error (miao:complete :test-keyed
                                                     :messages '((:role :wizard)))))))
    (is (null (fake-http-requests *backend*)))))

(test a-provider-whose-protocol-is-not-registered-is-unavailable
  (with-backends ((json-response +hello-reply+)
                  (list :test-orphan :model "test-model"))
    (is (eq :unavailable (miao:result-error (turn :test-orphan))))))

;;; --- the deadline, the sink and the pool ---------------------------------

(defun expect-one-failed-done (events)
  "EVENTS end in exactly one :done, carrying a failed result."
  (is (= 1 (count :done (event-types events))))
  (is (eq :done (getf (car (last events)) :type)))
  (is (miao:result-error-p (getf (car (last events)) :reason))))

(test a-stream-failing-before-any-delta-ends-in-one-failed-done
  (with-backends ('(429 ("Content-Type" "application/json") "{\"error\":\"slow down\"}")
                  '(:protocol-openai))
    (let* ((events '())
           (result (ask :ref :r1 :stream (lambda (event) (push event events)))))
      (is (= 429 (second (miao:result-error result))))
      (expect-one-failed-done (reverse events))
      (is (equal '(:done) (event-types events))))))

(test a-stalled-stream-ends-in-one-timeout-done-and-frees-its-threads
  ;; The backend sends one delta and goes quiet. The deadline answers
  ;; :timeout, ends the sink's turn once, and closes the connection so the
  ;; reader thread does not outlive it.
  (with-backends ((stalled-stream "text/event-stream" (sse-body +stalled-delta+))
                  '(:protocol-openai))
    (with-hold
      (let* ((events '())
             (lock (bt:make-lock))
             (result (ask :ref :r1 :timeout 400
                          :stream (lambda (event)
                                    (bt:with-lock-held (lock) (push event events))))))
        (is (eq :timeout (miao:result-error result)))
        (is-true (eventually (lambda () (= 2 (length (bt:with-lock-held (lock) events))))))
        (is (equal '(:text-delta :done) (event-types (reverse events))))
        (is (equal '(:error :timeout) (getf (first events) :reason)))
        (is-true (eventually (lambda () (not (completion-running-p)))))
        (sleep 0.2)
        (is (= 2 (length events)))))))

(test a-blocking-sink-does-not-delay-the-timeout-reply
  ;; The sink parks on its first event; the deadline still answers on time
  ;; and the turn's :done queues behind it.
  (with-backends ((stalled-stream "text/event-stream" (sse-body +stalled-delta+))
                  '(:protocol-openai))
    (with-hold
      (let* ((release (bt:make-semaphore))
             (events '())
             (lock (bt:make-lock))
             (started (get-internal-real-time))
             (result (ask :ref :r1 :timeout 400
                          :stream (lambda (event)
                                    (bt:wait-on-semaphore release :timeout 10)
                                    (bt:with-lock-held (lock) (push event events))))))
        (is (eq :timeout (miao:result-error result)))
        (is (< (elapsed-since started) 2))
        (bt:signal-semaphore release :count 2)
        (is-true (eventually (lambda () (= 2 (length (bt:with-lock-held (lock) events))))))
        (is (equal '(:text-delta :done) (event-types (reverse events))))))))

(test a-sink-that-never-returns-loses-its-emitter-after-the-grace
  (with-backends ((stalled-stream "text/event-stream" (sse-body +stalled-delta+))
                  '(:protocol-openai))
    (with-hold
      (let ((grace miao::*emitter-grace*)
            (stuck (bt:make-semaphore)))
        (setf miao::*emitter-grace* 0.3)
        (unwind-protect
             (let ((result (ask :ref :r1 :timeout 400
                                :stream (lambda (event)
                                          (declare (ignore event))
                                          (bt:wait-on-semaphore stuck :timeout 60)))))
               (is (eq :timeout (miao:result-error result)))
               (is-true (eventually #'sinks-idle-p 5)))
          (setf miao::*emitter-grace* grace)
          (bt:signal-semaphore stuck :count 3))))))

(test cancelling-a-stalled-stream-ends-in-one-cancelled-done-and-frees-its-threads
  (with-backends ((stalled-stream "text/event-stream" (sse-body +stalled-delta+))
                  '(:protocol-openai))
    (with-hold
      (let* ((events '())
             (lock (bt:make-lock))
             (token (miao:make-cancel-token))
             (started (get-internal-real-time))
             (canceller (bt:make-thread (lambda () (sleep 0.3) (miao:cancel token))))
             (result (ask :ref :r1 :timeout 30000 :cancel token
                          :stream (lambda (event)
                                    (bt:with-lock-held (lock) (push event events))))))
        (bt:join-thread canceller)
        (is (eq :cancelled (miao:result-error result)))
        (is (< (elapsed-since started) 5))
        (is-true (eventually (lambda () (= 2 (length (bt:with-lock-held (lock) events))))))
        (is (equal '(:text-delta :done) (event-types (reverse events))))
        (is (equal '(:error :cancelled) (getf (first events) :reason)))
        (is-true (eventually (lambda () (not (completion-running-p)))))))))

(test stopping-the-service-mid-stream-ends-the-turn-and-frees-its-threads
  (with-backends ((stalled-stream "text/event-stream" (sse-body +stalled-delta+))
                  '(:protocol-openai))
    (with-hold
      (let* ((events '())
             (lock (bt:make-lock))
             (thread (in-thread
                      (lambda ()
                        (ask :ref :r1 :timeout 30000
                             :stream (lambda (event)
                                       (bt:with-lock-held (lock) (push event events)))))))
             (started (get-internal-real-time)))
        (sleep 0.3)
        (m:stop-and-wait (m:lookup :protocol-openai))
        (is (eq :cancelled (miao:result-error (bt:join-thread thread))))
        (is (< (elapsed-since started) 5))
        (is-true (eventually (lambda () (= 2 (length (bt:with-lock-held (lock) events))))))
        (is (equal '(:text-delta :done) (event-types (reverse events))))
        (is-true (eventually (lambda () (not (completion-running-p)))))))))

(test a-request-cancelled-beforehand-never-reaches-the-backend
  (with-backends ((json-response +hello-reply+) '(:protocol-openai))
    (let ((token (miao:make-cancel-token))
          (lock (bt:make-lock))
          (events '()))
      (miao:cancel token)
      (let ((result (ask :ref :r1 :cancel token
                         :stream (lambda (event)
                                   (bt:with-lock-held (lock) (push event events))))))
        (is (eq :cancelled (miao:result-error result)))
        (is (null (fake-http-requests *backend*)))
        (is-true (eventually (lambda () (bt:with-lock-held (lock) events))))
        (is (equal '(:done) (event-types events)))))))

(test cancelling-after-the-reply-changes-nothing
  (with-backends ((sse-response
                   "{\"choices\":[{\"delta\":{\"content\":\"hi\"},\"finish_reason\":\"stop\"}]}")
                  '(:protocol-openai))
    (let* ((token (miao:make-cancel-token))
           (events '())
           (result (ask :ref :r1 :cancel token
                        :stream (lambda (event) (push event events)))))
      (is (eq :ok (first result)))
      (miao:cancel token)
      (sleep 0.1)
      (is (= 2 (length events))))))

;;; --- concurrency ------------------------------------------------------

(defun call-with-echo-provider (body &rest mount-args)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :providers)
                                   :registry registry)))
    (unwind-protect
         (progn
           (apply #'mount-backend context :test-echo mount-args)
           (funcall body context))
      (m:stop context))))

(test a-provider-runs-completions-concurrently
  (call-with-echo-provider
   (lambda (context)
     (declare (ignore context))
     (let* ((start (get-internal-real-time))
            (results (concurrently
                      3 (lambda () (turn :test-echo :delay 0.5)))))
       (is (every (lambda (result) (eq :ok (first result))) results))
       (is (< (elapsed-since start) 1.2))))))

(test a-provider-max-in-flight-queues
  (call-with-echo-provider
   (lambda (context)
     (declare (ignore context))
     (let* ((start (get-internal-real-time))
            (results (concurrently
                      2 (lambda () (turn :test-echo :delay 0.4)))))
       (is (every (lambda (result) (eq :ok (first result))) results))
       (is (>= (elapsed-since start) 0.75))))
   :max-in-flight 1))

(test a-provider-passes-down-the-time-left
  (call-with-echo-provider
   (lambda (context)
     (declare (ignore context))
     (let ((busy (in-thread (lambda () (turn :test-echo :delay 0.3)))))
       (sleep 0.05)
       (let ((result (turn :test-echo :timeout 2000)))
         (is (eq :ok (first result)))
         (is (< (getf (getf (second result) :meta) :timeout) 1800)))
       (bt:join-thread busy)))
   :max-in-flight 1))

(test a-provider-over-a-provider-completes
  (call-with-echo-provider
   (lambda (context)
     (mount-backend context :test-stacked)
     (is (eq :ok (first (turn :test-stacked)))))))

(test a-provider-that-rewrites-the-response-still-does
  (call-with-echo-provider
   (lambda (context)
     (mount-backend context :test-rewriting)
     (let ((result (turn :test-rewriting)))
       (is (eq :ok (first result)))
       (is-true (getf (second result) :rewritten))))))

;;; --- live -------------------------------------------------------------

(defun model-missing-p (result)
  "A backend that answers but does not know the model, so the tag named by
CL_INFERENCE_OLLAMA_MODEL has not been pulled."
  (let ((reason (miao:result-error result)))
    (and (consp reason) (eq :backend-error (first reason)) (= 404 (second reason)))))

(test ollama-live-completion-through-the-service
  ;; Off by default: CI must not depend on a model being installed.
  (let ((base-url (uiop:getenv "CL_INFERENCE_OLLAMA_NATIVE_URL"))
        (model (or (uiop:getenv "CL_INFERENCE_OLLAMA_MODEL") "llama3.2")))
    (if (null base-url)
        (skip "set CL_INFERENCE_OLLAMA_NATIVE_URL to run live Ollama tests")
        (let* ((registry (make-instance 'm:registry))
               (m:*registry* registry)
               (context (m:start-service (make-instance 'm:context :name :live)
                                         :registry registry)))
          (unwind-protect
               (progn
                 (mount-backend context :ollama :base-url base-url :model model)
                 (let ((result (miao:complete
                                :ollama :timeout 120000
                                :messages '((:role :user
                                             :content "Reply with the word ok.")))))
                   (if (model-missing-p result)
                       (skip "~a has no model ~a; set CL_INFERENCE_OLLAMA_MODEL" base-url model)
                       (progn
                         (is (eq :ok (first result)))
                         (is (plusp (length (miao:content-text
                                             (getf (second result) :content)))))))))
            (m:stop context))))))
