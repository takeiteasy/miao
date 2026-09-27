(in-package #:miao/tests)
(in-suite :miao)

;;; Ollama's native protocol against the fake HTTP backend: the /api/chat
;;; request shape, the flat reply and its counters, NDJSON streaming, and the
;;; one ordering trap -- the counters arrive only on the final done:true line.

(defun call-with-ollama (answer body)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :protocols)
                                   :registry registry))
         (server (start-fake-http
                  (lambda (&rest request)
                    (if (functionp answer) (apply answer request) answer)))))
    (setf *backend* server)
    (unwind-protect
         (progn (m:mount context 'miao:protocol-ollama)
                (funcall body))
      (m:stop context)
      (stop-fake-http server))))

(defmacro with-ollama ((answer) &body body)
  `(call-with-ollama ,answer (lambda () ,@body)))

(defun ask-ollama (&rest extra)
  (apply #'miao:complete :protocol-ollama
         :base-url (fake-http-url *backend*)
         :model "test-model"
         :messages '((:role :user :content "hello"))
         extra))

(defparameter +hello-chat-reply+
  "{\"model\":\"llama3.2\",
    \"message\":{\"role\":\"assistant\",\"content\":\"hi there\"},
    \"done\":true,\"done_reason\":\"stop\",
    \"prompt_eval_count\":26,\"eval_count\":298,
    \"total_duration\":4883583458,\"load_duration\":1073398416,
    \"prompt_eval_duration\":132000000,\"eval_duration\":3678000000}")

;;; --- the convention -----------------------------------------------------

(test ollama-is-discoverable-and-describes-itself
  (with-ollama ((json-response +hello-chat-reply+))
    (is (equal '(:protocol-ollama) (miao:protocols)))
    (let ((metadata (miao:describe-protocol :protocol-ollama)))
      (is (eq :protocol (getf metadata :kind)))
      (is (stringp (getf metadata :summary))))))

;;; --- the request ----------------------------------------------------------

(test the-request-goes-to-api-chat
  (with-ollama ((json-response +hello-chat-reply+))
    (is (eq :ok (first (ask-ollama))))
    (let ((request (first (fake-http-requests *backend*))))
      (is (equal "POST" (getf request :method)))
      (is (equal "/api/chat" (getf request :path))))))

(test sampling-parameters-nest-under-options
  (with-ollama ((json-response +hello-chat-reply+))
    (ask-ollama :temperature 0.2 :num-ctx 8192)
    (let* ((body (sent-body))
           (options (gethash "options" body)))
      (is (= 0.2d0 (gethash "temperature" options)))
      (is (= 8192 (gethash "num_ctx" options)))
      ;; Not top-level: the sampling knobs are grouped.
      (is (null (nth-value 1 (gethash "temperature" body)))))))

(test format-and-keep-alive-stay-top-level
  (with-ollama ((json-response +hello-chat-reply+))
    (ask-ollama :format "json" :keep-alive "5m")
    (let ((body (sent-body)))
      (is (equal "json" (gethash "format" body)))
      (is (equal "5m" (gethash "keep_alive" body)))
      (is (null (nth-value 1 (gethash "options" body)))))))

(test an-absent-param-is-absent-from-the-body
  (with-ollama ((json-response +hello-chat-reply+))
    (ask-ollama)
    (is (null (nth-value 1 (gethash "options" (sent-body)))))))

(test tools-render-the-same-as-the-openai-protocol
  (with-ollama ((json-response +hello-chat-reply+))
    (ask-ollama :tools (list (list :name :tool-demo
                                   :summary "A demo tool"
                                   :params '((:cmd string :required t :doc "the command")))))
    (let* ((tool (aref (gethash "tools" (sent-body)) 0))
           (function (gethash "function" tool)))
      (is (equal "function" (gethash "type" tool)))
      (is (equal "tool-demo" (gethash "name" function)))
      (is (equal "A demo tool" (gethash "description" function))))))

(test a-sent-tool-call-carries-its-arguments-as-an-object
  (with-ollama ((json-response +hello-chat-reply+))
    (miao:complete
     :protocol-ollama
     :base-url (fake-http-url *backend*)
     :model "test-model"
     :messages '((:role :user :content "go")
                 (:role :assistant :content nil
                  :tool-calls ((:id "c1" :name :tool-shell :arguments (:cmd "ls"))))))
    (let* ((messages (gethash "messages" (sent-body)))
           (call (aref (gethash "tool_calls" (aref messages 1)) 0))
           (arguments (gethash "arguments" (gethash "function" call))))
      ;; An object, not a stringified one: JZON parses it straight to a hash
      ;; table, unlike the OpenAI protocol's "{\"cmd\":\"ls\"}".
      (is (hash-table-p arguments))
      (is (equal "ls" (gethash "cmd" arguments))))))

;;; --- the reply --------------------------------------------------------

(test a-reply-becomes-the-neutral-shape-with-native-counters
  (with-ollama ((json-response +hello-chat-reply+))
    (let ((reply (second (ask-ollama))))
      (is (eq :assistant (getf reply :role)))
      (is (eq t (getf reply :done)))
      (is (equal "hi there" (miao:content-text (getf reply :content))))
      (is (eq :stop (getf (getf reply :meta) :finish-reason)))
      (let ((usage (getf (getf reply :meta) :usage)))
        (is (= 26 (getf usage :prompt-eval-count)))
        (is (= 26 (getf usage :prompt-tokens)))
        (is (= 298 (getf usage :eval-count)))
        (is (= 4883583458 (getf usage :total-duration)))
        (is (= 1073398416 (getf usage :load-duration)))))))

(test a-tool-call-comes-back-with-a-synthesised-id
  (with-ollama ((json-response
                  "{\"message\":{\"role\":\"assistant\",\"content\":\"\",
                     \"tool_calls\":[{\"function\":{\"name\":\"tool-shell\",
                       \"arguments\":{\"cmd\":\"ls -l\"}}}]},
                    \"done\":true,\"done_reason\":\"tool_calls\"}"))
    (let* ((reply (second (ask-ollama)))
           (call (first (getf reply :tool-calls))))
      (is (eq :tool-calls (getf (getf reply :meta) :finish-reason)))
      (is (equal "call_0" (getf call :id)))
      (is (eq :tool-shell (getf call :name)))
      (is (equal "ls -l" (getf (getf call :arguments) :cmd))))))

(test a-tool-call-round-trips-back-through-complete
  ;; Parse a native reply with two calls, then feed the reply's own
  ;; tool-calls and matching tool-role messages straight back in: the
  ;; synthesised ids must correlate, not merely exist, and CHECK-REQUEST's
  ;; own :id/:name requirement must be satisfied by them.
  (with-ollama ((json-response
                  "{\"message\":{\"role\":\"assistant\",\"content\":\"\",
                     \"tool_calls\":[{\"function\":{\"name\":\"tool-shell\",\"arguments\":{\"cmd\":\"ls\"}}},
                                     {\"function\":{\"name\":\"tool-shell\",\"arguments\":{\"cmd\":\"pwd\"}}}]},
                    \"done\":true,\"done_reason\":\"tool_calls\"}"))
    (let* ((reply (second (ask-ollama)))
           (calls (getf reply :tool-calls)))
      (is (= 2 (length calls)))
      (is (equal '("call_0" "call_1") (mapcar (lambda (c) (getf c :id)) calls)))
      (let ((tool-messages (mapcar (lambda (c)
                                     (list :role :tool
                                           :tool-call-id (getf c :id)
                                           :content "ok"))
                                   calls))
            (assistant-turn (list :role :assistant :content nil :tool-calls calls)))
        (let ((followup-request
                (list :messages
                      (list* '(:role :user :content "list, then pwd")
                             assistant-turn tool-messages))))
          (is (null (miao:check-request followup-request)))
          (is (eq :ok (first (apply #'miao:complete :protocol-ollama
                                    :base-url (fake-http-url *backend*)
                                    :model "test-model"
                                    followup-request)))))))))

;;; --- the call carries its schema ----------------------------------------

(test an-ollama-replayed-call-renders-by-its-own-schema-when-its-tool-is-no-longer-offered
  (with-ollama ((json-response
                  "{\"message\":{\"role\":\"assistant\",\"content\":\"\",
                     \"tool_calls\":[{\"function\":{\"name\":\"tool-demo\",
                       \"arguments\":{\"headers\":{\"Accept\":\"text/plain\"}}}}]},
                    \"done\":true,\"done_reason\":\"tool_calls\"}"))
    (let* ((tool '(:name :tool-demo :params ((:headers (map-of string)))))
           (turn (second (ask-ollama :tools (list tool)))))
      (is (equal (getf tool :params) (getf (first (getf turn :tool-calls)) :schema)))
      (miao:complete :protocol-ollama
                     :base-url (fake-http-url *backend*)
                     :model "test-model"
                     :messages (list '(:role :user :content "go") turn
                                     '(:role :tool :tool-call-id "call_0" :content "ok"))))
    (let* ((body (com.inuoe.jzon:parse
                  (getf (second (fake-http-requests *backend*)) :body)))
           (call (aref (gethash "tool_calls" (aref (gethash "messages" body) 1)) 0))
           (arguments (gethash "arguments" (gethash "function" call))))
      (is (equal "text/plain" (gethash "Accept" (gethash "headers" arguments)))))))

;;; --- streaming --------------------------------------------------------

(defun collect-ollama-stream (&rest extra)
  (let* ((events '())
         (result (apply #'ask-ollama :ref :r1
                        :stream (lambda (event) (push event events))
                        extra)))
    (values (nreverse events) result)))

(test text-deltas-reach-the-sink-and-the-reply
  (with-ollama ((ndjson-response
                 "{\"message\":{\"role\":\"assistant\",\"content\":\"hi \"},\"done\":false}"
                 "{\"message\":{\"role\":\"assistant\",\"content\":\"there\"},\"done\":false}"
                 "{\"message\":{\"role\":\"assistant\",\"content\":\"\"},\"done\":true,\"done_reason\":\"stop\",\"prompt_eval_count\":3,\"eval_count\":2}"))
    (multiple-value-bind (events result) (collect-ollama-stream)
      (is (equal '(:text-delta :text-delta :done)
                 (mapcar (lambda (event) (getf event :type)) events)))
      (is (equal "hi " (getf (first events) :text)))
      (is (eq :stop (getf (third events) :reason)))
      (is (equal "hi there" (miao:content-text (getf (second result) :content))))
      ;; The counters arrive only on the final line, and must still land.
      (is (= 3 (getf (getf (getf (second result) :meta) :usage) :prompt-eval-count)))
      (is (= 3 (miao::reply-prompt-tokens (second result))))
      (is (eq t (gethash "stream" (sent-body)))))))

(test a-stream-ending-at-eof-without-done-is-a-backend-error
  (with-ollama ((ndjson-response
                 "{\"message\":{\"role\":\"assistant\",\"content\":\"partial\"},\"done\":false}"))
    (multiple-value-bind (events result) (collect-ollama-stream)
      (is (eq :backend-error (first (miao:tool-error result))))
      (expect-one-failed-done events))))

(test ollama-a-stalled-stream-ends-in-one-timeout-done-and-frees-its-threads
  ;; The backend sends one delta and goes quiet. The deadline answers
  ;; :timeout, ends the sink's turn once, and closes the connection so the
  ;; reader thread does not outlive it.
  (with-ollama ((stalled-stream "application/x-ndjson" (ndjson-body "{\"message\":{\"role\":\"assistant\",\"content\":\"hi\"},\"done\":false}")))
    (with-hold
      (let* ((events '())
             (lock (bt:make-lock))
             (result (ask-ollama :ref :r1 :timeout 400
                            :stream (lambda (event)
                                      (bt:with-lock-held (lock) (push event events))))))
        (is (eq :timeout (miao:tool-error result)))
        (is (equal '(:text-delta :done)
                   (mapcar (lambda (event) (getf event :type)) (reverse events))))
        (is (equal '(:error :timeout) (getf (first events) :reason)))
        ;; The hold is still on, so the threads are gone only because the
        ;; deadline closed the connection.
        (is-true (eventually (lambda () (not (completion-running-p)))))
        (sleep 0.2)
        (is (= 2 (length events)))))))

(test ollama-a-streamed-non-ok-status-ends-in-one-failed-done
  (with-ollama ('(429 ("Content-Type" "application/json") "{\"error\":\"slow down\"}"))
    (multiple-value-bind (events result) (collect-ollama-stream)
      (is (= 429 (second (miao:tool-error result))))
      (expect-one-failed-done events))))

(test ollama-a-non-ascii-request-body-reaches-the-backend
  (with-ollama ((json-response +hello-chat-reply+))
    (let ((result (miao:complete :protocol-ollama
                                 :base-url (fake-http-url *backend*)
                                 :model "test-model"
                                 :messages (list (list :role :user :content +non-ascii-text+)))))
      (is (eq :ok (first result)))
      (is (equal +non-ascii-text+
                 (gethash "content" (aref (gethash "messages" (sent-body)) 0)))))))

(test ollama-a-non-ascii-delta-survives-the-stream
  (with-ollama ((ndjson-response
                 (format nil "{\"message\":{\"role\":\"assistant\",\"content\":\"~a\"},\"done\":true,\"done_reason\":\"stop\"}"
                         +non-ascii-text+)))
    (multiple-value-bind (events result) (collect-ollama-stream)
      (is (equal +non-ascii-text+ (getf (first events) :text)))
      (is (equal +non-ascii-text+
                 (miao:content-text (getf (second result) :content)))))))

(test a-non-ok-status-is-a-backend-error
  (with-ollama ('(429 ("Content-Type" "application/json")
                  "{\"error\":\"rate limited\"}")
                )
    (let ((reason (miao:tool-error (ask-ollama))))
      (is (eq :backend-error (first reason)))
      (is (= 429 (second reason))))))

(test ollama-a-retry-after-header-reaches-the-error
  (with-ollama ('(429 ("Content-Type" "application/json" "Retry-After" "2")
                  "{\"error\":\"rate limited\"}"))
    (is (equal '(:retry-after 2000) (cdddr (miao:tool-error (ask-ollama)))))))

(test a-connection-closed-before-a-response-is-unavailable
  (with-ollama (:close)
    (is (eq :unavailable (miao:tool-error (ask-ollama))))))

(test an-unreachable-backend-is-unavailable
  (with-ollama ((json-response +hello-chat-reply+))
    (is (eq :unavailable
            (miao:tool-error
             (miao:complete :protocol-ollama
                            :base-url "http://127.0.0.1:1"
                            :model "test-model"
                            :timeout 5000
                            :messages '((:role :user :content "hello"))))))))

;;; --- live -------------------------------------------------------------

;;; A separate variable from MIAO_OLLAMA_URL, which the OpenAI protocol's live
;;; tests point at the /v1 route.

(test ollama-live-completion
  (let ((base-url (uiop:getenv "MIAO_OLLAMA_NATIVE_URL"))
        (model (or (uiop:getenv "MIAO_OLLAMA_MODEL") "llama3.2")))
    (if (null base-url)
        (skip "set MIAO_OLLAMA_NATIVE_URL to run live native Ollama tests")
        (let* ((registry (make-instance 'm:registry))
               (m:*registry* registry)
               (context (m:start-service (make-instance 'm:context :name :live)
                                         :registry registry)))
          (unwind-protect
               (progn
                 (m:mount context 'miao:protocol-ollama)
                 (let ((result (miao:complete
                                :protocol-ollama :base-url base-url :model model
                                :timeout 120000 :num-ctx 2048
                                :messages '((:role :user
                                             :content "Reply with the word ok.")))))
                   (if (model-missing-p result)
                       (skip "~a has no model ~a; set MIAO_OLLAMA_MODEL" base-url model)
                       (progn
                         (is (eq :ok (first result)))
                         (is (plusp (length (miao:content-text
                                             (getf (second result) :content)))))
                         (is (getf (getf (second result) :meta) :usage))))))
            (m:stop context))))))
