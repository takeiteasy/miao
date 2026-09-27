(in-package #:miao/tests)
(in-suite :miao)

;;; The provider layer against the fake HTTP backend: what a declaration
;;; publishes, what a mount may override, where a credential comes from and
;;; what it looks like on the wire, and that a turn crosses the extra hop
;;; without losing anything.

;;; Every declaration pins a base URL nothing listens on, because the fake
;;; backend's port is only known at run time: each mount overrides it, which
;;; is the same override a remote Ollama host or a proxy uses.

(miao:define-provider :test-keyed
  :protocol :protocol-openai
  :base-url "http://127.0.0.1:1"
  :auth '(:bearer :env "MIAO_TEST_KEY_NEVER_SET")
  :models '("test-model" "test-model-large")
  :defaults '(:temperature 0.25)
  :headers '("x-quirk" "on")
  :summary "A bearer-keyed provider for tests")

(miao:define-provider :test-header-keyed
  :protocol :protocol-openai
  :base-url "http://127.0.0.1:1"
  :auth '(:header "x-api-key" :env "MIAO_TEST_KEY_NEVER_SET"))

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
  :protocol :provider-test-echo
  :base-url "http://127.0.0.1:1")

(miao:define-provider :test-orphan
  :protocol :protocol-nobody-mounted
  :base-url "http://127.0.0.1:1")

;;; --- the harness ------------------------------------------------------

(defun call-with-providers (answer mounts body)
  "Run BODY with both protocols and MOUNTS up against a fake backend. Each
mount is (class . initargs), with :base-url filled in."
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :providers)
                                   :registry registry))
         (server (start-fake-http
                  (lambda (&rest request)
                    (if (functionp answer) (apply answer request) answer)))))
    (setf *backend* server)
    (unwind-protect
         (progn
           (m:mount context 'miao:protocol-openai)
           (m:mount context 'miao:protocol-ollama)
           (dolist (mount mounts)
             (apply #'m:mount context (first mount)
                    :base-url (fake-http-url server) (rest mount)))
           (funcall body))
      (m:stop context)
      (stop-fake-http server))))

(defmacro with-providers ((answer &rest mounts) &body body)
  `(call-with-providers ,answer (list ,@mounts) (lambda () ,@body)))

(defun keyed (&rest initargs)
  "The bearer-keyed mount. INITARGS come first, since the leftmost initarg
is the one MAKE-INSTANCE takes."
  (append (list* 'provider-test-keyed initargs)
          '(:model "test-model" :api-key "sk-secret")))

(defun turn (name &rest extra)
  (apply #'miao:complete name :messages '((:role :user :content "hello")) extra))

(defun sent-header (name)
  (getf-string (getf (first (fake-http-requests *backend*)) :headers) name))

;;; --- the declaration --------------------------------------------------

(test a-provider-is-discoverable-and-describes-itself
  (with-providers ((json-response +hello-reply+) (keyed))
    (is (equal '(:provider-test-keyed) (miao:providers)))
    (let ((metadata (miao:describe-provider :provider-test-keyed)))
      (is (eq :provider (getf metadata :kind)))
      (is (eq :protocol-openai (getf metadata :protocol)))
      (is (stringp (getf metadata :summary)))
      (is (equal '("test-model" "test-model-large") (getf metadata :models)))
      (is (equal '(:temperature 0.25) (getf metadata :defaults)))
      (is (eq :ready (getf metadata :status)))
      ;; The auth form names its kind and where the key comes from, never
      ;; the key.
      (is (eq :bearer (getf (getf metadata :auth) :kind)))
      (is (equal "MIAO_TEST_KEY_NEVER_SET" (getf (getf metadata :auth) :env))))))

(test metadata-carries-no-key-material
  (with-providers ((json-response +hello-reply+) (keyed))
    (is (null (search "sk-secret"
                      (princ-to-string
                       (miao:describe-provider :provider-test-keyed)))))))

(test a-mount-overrides-the-declaration
  ;; The declaration pins a dead port; only the override makes the turn land.
  (with-providers ((json-response +hello-reply+) (keyed :model "override-model"))
    (let ((metadata (miao:describe-provider :provider-test-keyed)))
      (is (equal (fake-http-url *backend*) (getf metadata :base-url)))
      (is (equal "override-model" (getf metadata :model))))
    (is (eq :ok (first (turn :provider-test-keyed))))
    (is (equal "override-model" (gethash "model" (sent-body))))))

(test a-declaration-outside-the-vocabulary-is-a-definition-error
  (flet ((declaration (&rest plist)
           (signals error
             (miao::check-provider-declaration
              :provider-broken
              (append plist '(:protocol :protocol-openai
                              :base-url "http://127.0.0.1:1"))))))
    (declaration :auth :oauth)                     ; not one of the three kinds
    (declaration :auth '(:bearer))                 ; a bearer with no :env
    (declaration :auth '(:header :env "VAR"))      ; a header with no name
    (declaration :defaults '(:temperature))        ; not a plist
    (declaration :quirk t))                        ; not a key at all
  ;; And the two the declaration cannot do without.
  (signals error (miao::check-provider-declaration :b '(:base-url "http://x")))
  (signals error (miao::check-provider-declaration :b '(:protocol :protocol-openai)))
  (signals error (miao::check-provider-declaration
                  :b '(:protocol :protocol-openai :base-url "127.0.0.1:11434"))))

;;; --- credentials ------------------------------------------------------

(test bearer-auth-reaches-the-wire
  (with-providers ((json-response +hello-reply+) (keyed))
    (is (eq :ok (first (turn :provider-test-keyed))))
    (is (equal "Bearer sk-secret" (sent-header "authorization")))))

(test header-auth-reaches-the-wire-under-its-own-name
  (with-providers ((json-response +hello-reply+)
                   (list 'provider-test-header-keyed
                         :model "test-model" :api-key "sk-secret"))
    (is (eq :ok (first (turn :provider-test-header-keyed))))
    (is (equal "sk-secret" (sent-header "x-api-key")))
    (is (null (sent-header "authorization")))))

(test a-keyless-provider-authenticates-with-nothing
  (with-providers ((json-response +hello-reply+)
                   (list 'provider-test-keyless :model "test-model"))
    (is (eq :ok (first (turn :provider-test-keyless))))
    (is (null (sent-header "authorization")))
    (is (eq :none (getf (getf (miao:describe-provider :provider-test-keyless)
                              :auth)
                        :kind)))))

(test a-key-comes-from-the-environment-variable-the-declaration-names
  (with-providers ((json-response +hello-reply+)
                   (list 'provider-test-env-keyed :model "test-model"))
    (is (eq :ok (first (turn :provider-test-env-keyed))))
    (is (equal (format nil "Bearer ~a" (uiop:getenv "PATH"))
               (sent-header "authorization")))))

(test a-provider-with-no-key-mounts-unavailable-and-stays-off-the-wire
  ;; It mounts, so discovery lists it and the reason is legible without a
  ;; call; the call itself is a bad request rather than an outage.
  (with-providers ((json-response +hello-reply+)
                   (list 'provider-test-keyed :model "test-model"))
    (is (eq :unavailable (getf (miao:describe-provider :provider-test-keyed)
                               :status)))
    (let ((reason (miao:tool-error (turn :provider-test-keyed))))
      (is (eq :bad-request (first reason)))
      (is (search "MIAO_TEST_KEY_NEVER_SET" (second reason))))
    (is (null (fake-http-requests *backend*)))))

;;; --- layering ---------------------------------------------------------

(test defaults-layer-under-the-request
  (with-providers ((json-response +hello-reply+) (keyed))
    (turn :provider-test-keyed)
    (is (= 0.25d0 (gethash "temperature" (sent-body)))))
  (with-providers ((json-response +hello-reply+) (keyed))
    (turn :provider-test-keyed :temperature 0.9)
    (is (= 0.9d0 (gethash "temperature" (sent-body))))))

(test quirk-headers-travel-and-the-caller-outranks-them
  (with-providers ((json-response +hello-reply+) (keyed))
    (turn :provider-test-keyed)
    (is (equal "on" (sent-header "x-quirk"))))
  (with-providers ((json-response +hello-reply+) (keyed))
    ;; Matched without case, as HTTP names are.
    (turn :provider-test-keyed :headers '("X-Quirk" "off"))
    (is (equal "off" (sent-header "x-quirk")))))

(test a-mount-that-binds-no-model-leaves-the-requirement-to-the-caller
  (with-providers ((json-response +hello-reply+)
                   (list 'provider-test-keyless))
    (is (eq :bad-request (first (miao:tool-error (turn :provider-test-keyless)))))
    (is (eq :ok (first (turn :provider-test-keyless :model "asked-for"))))
    (is (equal "asked-for" (gethash "model" (sent-body))))))

;;; --- the turn crosses the hop -----------------------------------------

(test a-turn-crosses-the-provider-unchanged
  (with-providers ((json-response +hello-reply+) (keyed))
    (let ((reply (second (turn :provider-test-keyed))))
      (is (equal "hi there" (miao:content-text (getf reply :content))))
      (is (eq :stop (getf (getf reply :meta) :finish-reason)))
      (is (= 7 (getf (getf (getf reply :meta) :usage) :prompt-tokens))))))

(test a-streamed-turn-crosses-the-provider
  (with-providers ((sse-response
                    "{\"choices\":[{\"delta\":{\"content\":\"hi \"}}]}"
                    "{\"choices\":[{\"delta\":{\"content\":\"there\"}}]}"
                    "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"
                    "[DONE]")
                   (keyed))
    (let* ((events '())
           (result (turn :provider-test-keyed :ref :r1
                         :stream (lambda (event) (push event events)))))
      (is (equal '(:text-delta :text-delta :done)
                 (mapcar (lambda (event) (getf event :type)) (nreverse events))))
      (is (equal "hi there" (miao:content-text (getf (second result) :content)))))))

(test a-cancel-crosses-the-provider
  (with-providers ((stalled-stream "text/event-stream" (sse-body "{\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}"))
                   (keyed))
    (with-hold
      (let* ((token (miao:make-cancel-token))
             (canceller (bt:make-thread (lambda () (sleep 0.3) (miao:cancel token))))
             (result (turn :provider-test-keyed :cancel token :timeout 30000
                           :stream (lambda (event) event))))
        (bt:join-thread canceller)
        (is (eq :cancelled (miao:tool-error result)))))))

(test a-tool-call-crosses-the-provider-ready-to-invoke
  (with-providers ((json-response
                     "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,
                        \"tool_calls\":[{\"id\":\"c9\",\"type\":\"function\",
                          \"function\":{\"name\":\"tool-shell\",
                            \"arguments\":\"{\\\"cmd\\\":\\\"ls\\\"}\"}}]},
                        \"finish_reason\":\"tool_calls\"}]}")
                   (keyed))
    (let ((call (first (getf (second (turn :provider-test-keyed)) :tool-calls))))
      (is (eq :tool-shell (getf call :name)))
      (is (equal "ls" (getf (getf call :arguments) :cmd))))))

;;; --- errors -----------------------------------------------------------

(test a-backend-error-crosses-the-provider-intact
  (with-providers ('(429 ("Content-Type" "application/json")
                     "{\"error\":{\"message\":\"rate limited\"}}")
                   (keyed))
    (let ((reason (miao:tool-error (turn :provider-test-keyed))))
      (is (eq :backend-error (first reason)))
      (is (= 429 (second reason)))
      (is (search "rate limited" (third reason))))))

(test an-unreachable-backend-crosses-the-provider-as-unavailable
  (with-providers (:close (keyed))
    (is (eq :unavailable (miao:tool-error (turn :provider-test-keyed))))))

(test a-malformed-request-never-leaves-the-provider
  (with-providers ((json-response +hello-reply+) (keyed))
    (is (eq :bad-request
            (first (miao:tool-error (miao:complete :provider-test-keyed
                                                   :messages '((:role :wizard)))))))
    (is (null (fake-http-requests *backend*)))))

(test a-provider-whose-protocol-is-not-mounted-is-unavailable
  (with-providers ((json-response +hello-reply+)
                   (list 'provider-test-orphan :model "test-model"))
    (is (eq :unavailable (miao:tool-error (turn :provider-test-orphan))))))

;;; --- the ollama provider ----------------------------------------------

(test ollama-declares-a-keyless-native-backend
  (with-providers ((json-response
                     "{\"message\":{\"role\":\"assistant\",\"content\":\"hi\"},
                       \"done\":true,\"done_reason\":\"stop\"}")
                   (list 'miao:provider-ollama :model "llama3.2"))
    (let ((metadata (miao:describe-provider :provider-ollama)))
      (is (eq :provider (getf metadata :kind)))
      (is (eq :protocol-ollama (getf metadata :protocol)))
      (is (eq :none (getf (getf metadata :auth) :kind)))
      (is (eq :ready (getf metadata :status)))
      (is (member "llama3.2" (getf metadata :models) :test #'equal)))
    (is (eq :ok (first (turn :provider-ollama))))
    (is (equal "llama3.2" (gethash "model" (sent-body))))
    (is (null (sent-header "authorization")))))

(test ollama-pins-the-local-endpoint-by-default
  ;; Read from the declaration rather than a mount, so the default cannot
  ;; drift without this failing. The OpenAI-compatible /v1 route stays
  ;; reachable with no provider of its own -- :protocol-openai takes
  ;; :base-url per request.
  (is (equal "http://127.0.0.1:11434"
             (getf (miao::provider-declaration
                    (make-instance 'miao:provider-ollama))
                   :base-url))))

;;; --- live -------------------------------------------------------------

(test ollama-live-completion-through-the-provider
  ;; Off by default: CI must not depend on a model being installed. A
  ;; separate variable from MIAO_OLLAMA_URL, which the OpenAI protocol's live
  ;; tests point at the /v1 route.
  (let ((base-url (uiop:getenv "MIAO_OLLAMA_NATIVE_URL"))
        (model (or (uiop:getenv "MIAO_OLLAMA_MODEL") "llama3.2")))
    (if (null base-url)
        (skip "set MIAO_OLLAMA_NATIVE_URL to run live Ollama tests")
        (let* ((registry (make-instance 'm:registry))
               (m:*registry* registry)
               (context (m:start-service (make-instance 'm:context :name :live)
                                         :registry registry)))
          (unwind-protect
               (progn
                 (m:mount context 'miao:protocol-ollama)
                 (m:mount context 'miao:provider-ollama
                          :base-url base-url :model model)
                 (let ((result (miao:complete
                                :provider-ollama :timeout 120000
                                :messages '((:role :user
                                             :content "Reply with the word ok.")))))
                   (if (model-missing-p result)
                       (skip "~a has no model ~a; set MIAO_OLLAMA_MODEL" base-url model)
                       (progn
                         (is (eq :ok (first result)))
                         (is (plusp (length (miao:content-text
                                             (getf (second result) :content)))))))))
            (m:stop context))))))

;;; --- concurrency ------------------------------------------------------

(test a-provider-runs-completions-concurrently
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :providers)
                                   :registry registry)))
    (unwind-protect
         (progn
           (m:mount context 'protocol-echo)
           (m:mount context 'provider-test-echo)
           (let* ((start (get-internal-real-time))
                  (results (concurrently
                            3 (lambda () (turn :provider-test-echo :delay 0.5)))))
             (is (every (lambda (result) (eq :ok (first result))) results))
             (is (< (elapsed-since start) 1.2))))
      (m:stop context))))

(defun call-with-echo-provider (body &rest provider-args)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :providers)
                                   :registry registry)))
    (unwind-protect
         (progn
           (m:mount context 'protocol-echo)
           (apply #'m:mount context 'provider-test-echo provider-args)
           (funcall body context))
      (m:stop context))))

(test a-provider-max-in-flight-queues
  (call-with-echo-provider
   (lambda (context)
     (declare (ignore context))
     (let* ((start (get-internal-real-time))
            (results (concurrently
                      2 (lambda () (turn :provider-test-echo :delay 0.4)))))
       (is (every (lambda (result) (eq :ok (first result))) results))
       (is (>= (elapsed-since start) 0.75))))
   :max-in-flight 1))

(test a-provider-passes-down-the-time-left
  (call-with-echo-provider
   (lambda (context)
     (declare (ignore context))
     (let ((busy (in-thread (lambda () (turn :provider-test-echo :delay 0.3)))))
       (sleep 0.05)
       (let ((result (turn :provider-test-echo :timeout 2000)))
         (is (eq :ok (first result)))
         (is (< (getf (getf (second result) :meta) :timeout) 1800)))
       (bt:join-thread busy)))
   :max-in-flight 1))

(test a-provider-over-a-provider-completes
  (call-with-echo-provider
   (lambda (context)
     (m:mount context 'provider-test-stacked)
     (is (eq :ok (first (turn :provider-test-stacked)))))))

(test a-provider-forwards-without-holding-a-pooled-thread
  (call-with-echo-provider
   (lambda (context)
     (declare (ignore context))
     (let* ((spawned (getf (miao:pool-stats 0) :spawned))
            (start (get-internal-real-time))
            (results (concurrently 3 (lambda () (turn :provider-test-echo :delay 0.4)))))
       (is (every (lambda (result) (eq :ok (first result))) results))
       (is (< (elapsed-since start) 1))
       (is (= spawned (getf (miao:pool-stats 0) :spawned)))
       (is (zerop (getf (miao:pool-stats 0) :running)))))))

(test a-provider-that-rewrites-the-response-still-does
  (call-with-echo-provider
   (lambda (context)
     (m:mount context 'provider-test-rewriting)
     (let ((result (turn :provider-test-rewriting)))
       (is (eq :ok (first result)))
       (is-true (getf (second result) :rewritten))))))
