(in-package #:miao/tests)
(in-suite :miao)

;;; The OpenAI protocol against the fake HTTP backend: what it puts on the
;;; wire, what it makes of what comes back, the SSE vocabulary, and the line
;;; between a transport failure and a backend that misbehaved.

(defvar *backend* nil "The fake HTTP server the running test answers from.")

(defun call-with-openai (answer body)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :protocols)
                                   :registry registry))
         (server (start-fake-http
                  (lambda (&rest request)
                    (if (functionp answer) (apply answer request) answer)))))
    (setf *backend* server)
    (unwind-protect
         (progn (m:mount context 'miao:protocol-openai)
                (funcall body))
      (m:stop context)
      (stop-fake-http server))))

(defmacro with-openai ((answer) &body body)
  `(call-with-openai ,answer (lambda () ,@body)))

(defun ask (&rest extra)
  "One turn against the fake backend, with the provider data the protocol
needs and a user message."
  (apply #'miao:complete :protocol-openai
         :base-url (fake-http-url *backend*)
         :model "test-model"
         :messages '((:role :user :content "hello"))
         extra))

(defun sent-body ()
  "The JSON body of the one request the backend received."
  (com.inuoe.jzon:parse (getf (first (fake-http-requests *backend*)) :body)))

(defun json-response (json)
  (list 200 '("Content-Type" "application/json") json))

(defparameter +hello-reply+
  "{\"id\":\"cmpl-1\",
    \"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"hi there\"},
                  \"finish_reason\":\"stop\"}],
    \"usage\":{\"prompt_tokens\":7,\"completion_tokens\":2}}")

;;; --- the convention ---------------------------------------------------

(test openai-is-discoverable-and-describes-itself
  (with-openai ((json-response +hello-reply+))
    (is (equal '(:protocol-openai) (miao:protocols)))
    (let ((metadata (miao:describe-protocol :protocol-openai)))
      (is (eq :protocol (getf metadata :kind)))
      (is (stringp (getf metadata :summary)))
      ;; :params advertises the sampling knobs, not the provider data.
      (is (equal '(:temperature :top-p :max-tokens :stop :seed)
                 (mapcar #'first (getf metadata :params))))
      (is (null (find :base-url (getf metadata :params) :key #'first))))))

;;; --- provider data ----------------------------------------------------

(test provider-data-is-required-before-the-wire
  (with-openai ((json-response +hello-reply+))
    (flet ((problem (result)
             (let ((reason (miao:tool-error result)))
               (and (consp reason) (eq :bad-request (first reason))))))
      (is (problem (miao:complete :protocol-openai
                                  :model "m"
                                  :messages '((:role :user :content "x")))))
      (is (problem (miao:complete :protocol-openai
                                  :base-url (fake-http-url *backend*)
                                  :messages '((:role :user :content "x")))))
      ;; A URL with no scheme cannot fail as a transport failure later.
      (is (problem (miao:complete :protocol-openai
                                  :base-url "127.0.0.1:11434/v1" :model "m"
                                  :messages '((:role :user :content "x"))))))
    ;; Pre-flight, so neither reached the backend.
    (is (null (fake-http-requests *backend*)))))

(test the-request-goes-to-chat-completions-with-the-headers
  (with-openai ((json-response +hello-reply+))
    (is (eq :ok (first (ask :headers '("authorization" "Bearer sk-test")))))
    (let ((request (first (fake-http-requests *backend*))))
      (is (equal "POST" (getf request :method)))
      (is (equal "/chat/completions" (getf request :path)))
      (is (equal "Bearer sk-test"
                 (getf-string (getf request :headers) "authorization"))))))

(test a-base-url-with-a-trailing-slash-is-the-same-url
  (with-openai ((json-response +hello-reply+))
    (is (eq :ok (first (miao:complete
                        :protocol-openai
                        :base-url (concatenate 'string (fake-http-url *backend*) "/")
                        :model "test-model"
                        :messages '((:role :user :content "hello"))))))
    (is (equal "/chat/completions"
               (getf (first (fake-http-requests *backend*)) :path)))))

;;; --- the request body -------------------------------------------------

(test the-body-carries-the-model-and-the-messages
  (with-openai ((json-response +hello-reply+))
    (ask)
    (let* ((body (sent-body))
           (message (aref (gethash "messages" body) 0)))
      (is (equal "test-model" (gethash "model" body)))
      (is (eq nil (gethash "stream" body)))
      (is (equal "user" (gethash "role" message)))
      (is (equal "hello" (gethash "content" message))))))

(test the-body-writes-its-keys-in-a-fixed-order
  (with-openai ((json-response +hello-reply+))
    (ask :temperature 0.2 :max-tokens 64)
    (let ((body (getf (first (fake-http-requests *backend*)) :body)))
      (is (apply #'< (mapcar (lambda (key) (search (format nil "\"~a\":" key) body))
                             '("model" "messages" "stream" "temperature" "max_tokens")))))))

(test sampling-parameters-travel-under-their-wire-names
  (with-openai ((json-response +hello-reply+))
    (ask :temperature 0.2 :max-tokens 64 :stop '("STOP") :top-k 40)
    (let ((body (sent-body)))
      (is (= 0.2d0 (gethash "temperature" body)))
      (is (= 64 (gethash "max_tokens" body)))
      (is (equalp #("STOP") (gethash "stop" body)))
      ;; A key the protocol does not advertise is ignored, not forwarded.
      (is (null (nth-value 1 (gethash "top_k" body)))))))

(test every-role-renders-including-tool-results
  (with-openai ((json-response +hello-reply+))
    (miao:complete
     :protocol-openai
     :base-url (fake-http-url *backend*)
     :model "test-model"
     :messages '((:role :system :content "be terse")
                 (:role :user :content ((:type :text :text "ls")))
                 (:role :assistant :content nil
                  :tool-calls ((:id "c1" :name :tool-shell
                                :arguments (:cmd "ls"))))
                 (:role :tool :tool-call-id "c1" :content "a.lisp")))
    (let ((messages (gethash "messages" (sent-body))))
      (is (= 4 (length messages)))
      (is (equal "system" (gethash "role" (aref messages 0))))
      ;; A block list and a flat string reach the wire the same way.
      (is (equal "ls" (gethash "content" (aref messages 1))))
      (let* ((assistant (aref messages 2))
             (call (aref (gethash "tool_calls" assistant) 0)))
        ;; No content at all, rather than an empty string.
        (is (eq 'cl:null (gethash "content" assistant)))
        (is (equal "c1" (gethash "id" call)))
        (is (equal "function" (gethash "type" call)))
        (is (equal "tool-shell" (gethash "name" (gethash "function" call))))
        (is (equal "{\"cmd\":\"ls\"}"
                   (gethash "arguments" (gethash "function" call)))))
      (let ((tool (aref messages 3)))
        (is (equal "tool" (gethash "role" tool)))
        (is (equal "c1" (gethash "tool_call_id" tool)))
        (is (equal "a.lisp" (gethash "content" tool)))))))

(test a-replayed-call-renders-its-arguments-by-the-tools-schema
  ;; The outbound counterpart: a map stays an object rather than becoming
  ;; the array a bare plist would suggest.
  (with-openai ((json-response +hello-reply+))
    (miao:complete
     :protocol-openai
     :base-url (fake-http-url *backend*)
     :model "test-model"
     :tools (list (list :name :tool-demo :params '((:headers (map-of string)))))
     :messages '((:role :user :content "go")
                 (:role :assistant :content nil
                  :tool-calls ((:id "c1" :name :tool-demo
                                :arguments (:headers ("Accept" "text/plain")))))))
    (let* ((messages (gethash "messages" (sent-body)))
           (call (aref (gethash "tool_calls" (aref messages 1)) 0))
           (arguments (com.inuoe.jzon:parse
                       (gethash "arguments" (gethash "function" call)))))
      (is (equal "text/plain"
                 (gethash "Accept" (gethash "headers" arguments)))))))

(test tools-render-from-their-schemas
  (with-openai ((json-response +hello-reply+))
    (ask :tools (list (list :name :tool-demo
                            :summary "A demo tool"
                            :params '((:cmd string :required t :doc "the command")
                                      (:timeout (integer 1) :default 30)))))
    (let* ((tool (aref (gethash "tools" (sent-body)) 0))
           (function (gethash "function" tool))
           (parameters (gethash "parameters" function)))
      (is (equal "function" (gethash "type" tool)))
      (is (equal "tool-demo" (gethash "name" function)))
      (is (equal "A demo tool" (gethash "description" function)))
      (is (equal "object" (gethash "type" parameters)))
      (is (equalp #("cmd") (gethash "required" parameters)))
      (is (equal "string"
                 (gethash "type" (gethash "cmd" (gethash "properties" parameters))))))))

;;; --- the reply --------------------------------------------------------

(test a-reply-becomes-the-neutral-shape
  (with-openai ((json-response +hello-reply+))
    (let ((reply (second (ask))))
      (is (eq :assistant (getf reply :role)))
      (is (eq t (getf reply :done)))
      (is (equal "hi there" (miao:content-text (getf reply :content))))
      (is (null (getf reply :tool-calls)))
      (is (eq :stop (getf (getf reply :meta) :finish-reason)))
      (is (equal "cmpl-1" (getf (getf reply :meta) :id)))
      (is (= 7 (getf (getf (getf reply :meta) :usage) :prompt-tokens))))))

(test a-tool-call-comes-back-ready-to-invoke
  (with-openai ((json-response
                  "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,
                     \"tool_calls\":[{\"id\":\"c9\",\"type\":\"function\",
                       \"function\":{\"name\":\"tool-shell\",
                         \"arguments\":\"{\\\"cmd\\\":\\\"ls -l\\\",\\\"timeout\\\":5000}\"}}]},
                     \"finish_reason\":\"tool_calls\"}]}"))
    (let* ((reply (second (ask)))
           (call (first (getf reply :tool-calls))))
      (is (null (getf reply :content)))
      (is (eq :tool-calls (getf (getf reply :meta) :finish-reason)))
      (is (equal "c9" (getf call :id)))
      ;; A keyword name and a plist of keyword arguments: what INVOKE-TOOL
      ;; and COERCE-ARGS take.
      (is (eq :tool-shell (getf call :name)))
      (is (equal "ls -l" (getf (getf call :arguments) :cmd)))
      (is (= 5000 (getf (getf call :arguments) :timeout))))))

(test an-arguments-object-reads-by-the-tools-own-schema
  ;; A map's keys stay strings and an array stays a list, which a plist
  ;; alone cannot tell apart.
  (with-openai ((json-response
                  "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,
                     \"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\",
                       \"function\":{\"name\":\"tool-demo\",
                         \"arguments\":\"{\\\"headers\\\":{\\\"Accept\\\":\\\"text/plain\\\"},\\\"names\\\":[\\\"a\\\",\\\"b\\\"]}\"}}]},
                     \"finish_reason\":\"tool_calls\"}]}"))
    (let* ((reply (second (ask :tools (list (list :name :tool-demo
                                                  :params '((:headers (map-of string))
                                                            (:names (array-of string))))))))
           (arguments (getf (first (getf reply :tool-calls)) :arguments)))
      (is (equal '("Accept" "text/plain") (getf arguments :headers)))
      (is (equal '("a" "b") (getf arguments :names))))))

(defparameter +map-tool+
  '(:name :tool-demo :params ((:headers (map-of string)))))

(test a-parsed-call-carries-its-tools-schema
  (with-openai ((json-response
                  "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,
                     \"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\",
                       \"function\":{\"name\":\"tool-demo\",
                         \"arguments\":\"{\\\"headers\\\":{\\\"Accept\\\":\\\"text/plain\\\"}}\"}}]},
                     \"finish_reason\":\"tool_calls\"}]}"))
    (is (equal (getf +map-tool+ :params)
               (getf (first (getf (second (ask :tools (list +map-tool+))) :tool-calls))
                     :schema)))
    (is (null (getf (first (getf (second (ask)) :tool-calls)) :schema)))))

(test an-openai-replayed-call-renders-by-its-own-schema-when-its-tool-is-no-longer-offered
  (with-openai ((json-response
                  "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,
                     \"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\",
                       \"function\":{\"name\":\"tool-demo\",
                         \"arguments\":\"{\\\"headers\\\":{\\\"Accept\\\":\\\"text/plain\\\"}}\"}}]},
                     \"finish_reason\":\"tool_calls\"}]}"))
    (let ((turn (second (ask :tools (list +map-tool+)))))
      (miao:complete :protocol-openai
                     :base-url (fake-http-url *backend*)
                     :model "test-model"
                     :messages (list '(:role :user :content "go") turn
                                     '(:role :tool :tool-call-id "c1" :content "ok"))))
    (let* ((body (com.inuoe.jzon:parse
                  (getf (second (fake-http-requests *backend*)) :body)))
           (call (aref (gethash "tool_calls" (aref (gethash "messages" body) 1)) 0))
           (arguments (com.inuoe.jzon:parse
                       (gethash "arguments" (gethash "function" call)))))
      (is (equal "text/plain" (gethash "Accept" (gethash "headers" arguments)))))))

;;; --- streaming --------------------------------------------------------

(defun collect-stream (&rest extra)
  "One streamed turn. Answers the events in order and the reply."
  (let* ((events '())
         (result (apply #'ask :ref :r1
                        :stream (lambda (event) (push event events))
                        extra)))
    (values (nreverse events) result)))

(test openai-text-deltas-reach-the-sink-and-the-reply
  (with-openai ((sse-response
                 "{\"choices\":[{\"delta\":{\"content\":\"hi \"}}]}"
                 "{\"choices\":[{\"delta\":{\"content\":\"there\"}}]}"
                 "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"
                 "[DONE]"))
    (multiple-value-bind (events result) (collect-stream)
      (is (equal '(:text-delta :text-delta :done)
                 (mapcar (lambda (event) (getf event :type)) events)))
      (is (every (lambda (event) (eq :r1 (getf event :ref))) events))
      (is (equal "hi " (getf (first events) :text)))
      (is (eq :stop (getf (third events) :reason)))
      ;; The reply carries the whole turn regardless of the sink.
      (is (equal "hi there" (miao:content-text (getf (second result) :content))))
      (is (eq t (gethash "stream" (sent-body)))))))

(test tool-call-deltas-carry-identity-on-every-fragment
  (with-openai ((sse-response
                 "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"c1\",\"function\":{\"name\":\"tool-shell\",\"arguments\":\"{\\\"cmd\\\"\"}}]}}]}"
                 "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\":\\\"ls\\\"}\"}}]}}]}"
                 "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
                 "[DONE]"))
    (multiple-value-bind (events result) (collect-stream)
      (let ((deltas (remove :tool-call-delta events
                            :key (lambda (event) (getf event :type))
                            :test-not #'eq)))
        (is (= 2 (length deltas)))
        ;; The wire names the call once; every delta repeats it, so a sink
        ;; alone reassembles without tracking arrival order.
        (is (every (lambda (event) (equal "c1" (getf event :id))) deltas))
        (is (every (lambda (event) (eq :tool-shell (getf event :name))) deltas))
        (is (equal "{\"cmd\"" (getf (first deltas) :arguments)))
        (is (equal ":\"ls\"}" (getf (second deltas) :arguments))))
      ;; The fragments reassemble into one call.
      (let ((call (first (getf (second result) :tool-calls))))
        (is (equal "c1" (getf call :id)))
        (is (eq :tool-shell (getf call :name)))
        (is (equal "ls" (getf (getf call :arguments) :cmd)))))))

(test a-stream-ending-at-eof-without-done-still-finishes
  ;; Not every backend sends [DONE]; a finish_reason ends the turn too.
  (with-openai ((sse-response
                 "{\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}"
                 "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"))
    (multiple-value-bind (events result) (collect-stream)
      (is (eq :done (getf (car (last events)) :type)))
      (is (equal "ok" (miao:content-text (getf (second result) :content)))))))

(defun done-events (events)
  (remove :done events :key (lambda (event) (getf event :type)) :test-not #'eq))

(defun completion-running-p ()
  "True while a pooled thread is running a completion."
  (plusp (getf (miao:pool-stats 0) :running)))

(defun expect-one-failed-done (events)
  "EVENTS end in exactly one :done, carrying a failed result."
  (is (= 1 (length (done-events events))))
  (is (eq :done (getf (car (last events)) :type)))
  (is (miao:tool-error-p (getf (car (last events)) :reason))))

(defun cut-stream-bytes ()
  (format nil "HTTP/1.1 200 OK~c~cContent-Type: text/event-stream~c~c~
               Connection: close~c~c~c~c~
               data: {\"choices\":[{\"delta\":{\"content\":\"partial\"}}]}~c~c~c~c~
               data: {\"choices\":[{\"delta\":{\"cont"
          #\Return #\Newline #\Return #\Newline #\Return #\Newline
          #\Return #\Newline #\Return #\Newline #\Return #\Newline))

(test a-stream-cut-short-is-a-backend-error
  ;; Verbatim bytes and a hard close: the turn stops mid-event, with neither
  ;; a finish_reason nor a [DONE].
  (with-openai ((list :raw (cut-stream-bytes)))
    (multiple-value-bind (events result) (collect-stream)
      (is (eq :backend-error (first (miao:tool-error result))))
      (expect-one-failed-done events)
      (is (eq :backend-error (first (miao:tool-error (getf (car (last events)) :reason))))))))

(test a-stalled-stream-ends-in-one-timeout-done-and-frees-its-threads
  ;; The backend sends one delta and goes quiet. The deadline answers
  ;; :timeout, ends the sink's turn once, and closes the connection so the
  ;; reader thread does not outlive it.
  (with-openai ((stalled-stream "text/event-stream" (sse-body "{\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}")))
    (with-hold
      (let* ((events '())
             (lock (bt:make-lock))
             (result (ask :ref :r1 :timeout 400
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

(test a-blocking-sink-does-not-delay-the-timeout-reply
  ;; The sink parks on its first event; the deadline still answers on time
  ;; and the turn's :done queues behind it.
  (with-openai ((stalled-stream "text/event-stream" (sse-body "{\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}")))
    (with-hold
      (let* ((release (bt:make-semaphore))
             (events '())
             (lock (bt:make-lock))
             (started (get-internal-real-time))
             (result (ask :ref :r1 :timeout 400
                            :stream (lambda (event)
                                      (bt:wait-on-semaphore release :timeout 10)
                                      (bt:with-lock-held (lock) (push event events)))))
             (elapsed (/ (- (get-internal-real-time) started)
                         internal-time-units-per-second)))
        (is (eq :timeout (miao:tool-error result)))
        (is (< elapsed 2))
        (bt:signal-semaphore release :count 2)
        (is-true (eventually (lambda () (= 2 (length (bt:with-lock-held (lock) events))))))
        (is (equal '(:text-delta :done)
                   (mapcar (lambda (event) (getf event :type)) (reverse events))))))))

(test a-sink-that-never-returns-loses-its-emitter-after-the-grace
  (with-openai ((stalled-stream "text/event-stream" (sse-body "{\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}")))
    (with-hold
      (let ((grace miao::*sink-grace*)
            (stuck (bt:make-semaphore)))
        (setf miao::*sink-grace* 0.3)
        (unwind-protect
             (let ((result (ask :ref :r1 :timeout 400
                                :stream (lambda (event)
                                          (declare (ignore event))
                                          (bt:wait-on-semaphore stuck :timeout 60)))))
               (is (eq :timeout (miao:tool-error result)))
               (is-true (eventually #'sinks-idle-p 5)))
          (setf miao::*sink-grace* grace)
          (bt:signal-semaphore stuck :count 3))))))

(defun cancel-after (token seconds)
  (bt:make-thread (lambda () (sleep seconds) (miao:cancel token))
                  :name "test-canceller"))

(test cancelling-a-stalled-stream-ends-in-one-cancelled-done-and-frees-its-threads
  (with-openai ((stalled-stream "text/event-stream" (sse-body "{\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}")))
    (with-hold
      (let* ((events '())
             (lock (bt:make-lock))
             (token (miao:make-cancel-token))
             (started (get-internal-real-time))
             (canceller (cancel-after token 0.3))
             (result (ask :ref :r1 :timeout 30000 :cancel token
                            :stream (lambda (event)
                                      (bt:with-lock-held (lock) (push event events)))))
             (elapsed (/ (- (get-internal-real-time) started)
                         internal-time-units-per-second)))
        (bt:join-thread canceller)
        (is (eq :cancelled (miao:tool-error result)))
        (is (< elapsed 5))
        (is-true (eventually (lambda () (= 2 (length (bt:with-lock-held (lock) events))))))
        (is (equal '(:text-delta :done)
                   (mapcar (lambda (event) (getf event :type)) (reverse events))))
        (is (equal '(:error :cancelled) (getf (first events) :reason)))
        (is-true (eventually (lambda () (not (completion-running-p)))))))))

(test stopping-the-protocol-mid-stream-ends-the-turn-and-frees-its-threads
  (with-openai ((stalled-stream "text/event-stream" (sse-body "{\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}")))
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
        (is (eq :cancelled (miao:tool-error (bt:join-thread thread))))
        (is (< (elapsed-since started) 5))
        (is-true (eventually (lambda () (= 2 (length (bt:with-lock-held (lock) events))))))
        (is (equal '(:text-delta :done)
                   (mapcar (lambda (event) (getf event :type)) (reverse events))))
        (is-true (eventually (lambda () (not (completion-running-p)))))))))

(test a-request-cancelled-beforehand-never-reaches-the-backend
  (with-openai ((json-response +hello-reply+))
    (let ((token (miao:make-cancel-token))
          (lock (bt:make-lock))
          (events '()))
      (miao:cancel token)
      (let ((result (ask :ref :r1 :cancel token
                           :stream (lambda (event)
                                     (bt:with-lock-held (lock) (push event events))))))
        (is (eq :cancelled (miao:tool-error result)))
        (is (null (fake-http-requests *backend*)))
        ;; A cancelled reply does not wait on the sink.
        (is-true (eventually (lambda () (bt:with-lock-held (lock) events))))
        (is (equal '(:done) (mapcar (lambda (event) (getf event :type)) events)))))))

(test cancelling-after-the-reply-changes-nothing
  (with-openai ((sse-response
                 "{\"choices\":[{\"delta\":{\"content\":\"hi\"},\"finish_reason\":\"stop\"}]}"))
    (let* ((token (miao:make-cancel-token))
           (events '())
           (result (ask :ref :r1 :cancel token
                          :stream (lambda (event) (push event events)))))
      (is (eq :ok (first result)))
      (miao:cancel token)
      (sleep 0.1)
      (is (= 2 (length events))))))

(test a-streamed-non-ok-status-ends-in-one-failed-done
  (with-openai ('(429 ("Content-Type" "application/json") "{\"error\":\"slow down\"}"))
    (multiple-value-bind (events result) (collect-stream)
      (is (= 429 (second (miao:tool-error result))))
      (is (equal '(:done) (mapcar (lambda (event) (getf event :type)) events)))
      (expect-one-failed-done events))))

(test a-streamed-unreachable-backend-ends-in-one-failed-done
  (with-openai ((json-response +hello-reply+))
    (let* ((events '())
           (result (miao:complete :protocol-openai
                                  :base-url "http://127.0.0.1:1" :model "m"
                                  :messages '((:role :user :content "x"))
                                  :ref :r1 :stream (lambda (event) (push event events)))))
      (is (eq :unavailable (miao:tool-error result)))
      (is (equal '(:error :unavailable) (getf (first events) :reason)))
      (is (= 1 (length events))))))

(defparameter +non-ascii-text+ (format nil "h~cllo ~c" (code-char #xe9) (code-char #x2713)))

(test a-non-ascii-request-body-reaches-the-backend
  (with-openai ((json-response +hello-reply+))
    (let ((result (miao:complete :protocol-openai
                                 :base-url (fake-http-url *backend*)
                                 :model "test-model"
                                 :messages (list (list :role :user :content +non-ascii-text+)))))
      (is (eq :ok (first result)))
      (is (equal +non-ascii-text+
                 (gethash "content" (aref (gethash "messages" (sent-body)) 0)))))))

(test a-non-ascii-delta-survives-the-stream
  (with-openai ((sse-response
                 (format nil "{\"choices\":[{\"delta\":{\"content\":\"~a\"},\"finish_reason\":\"stop\"}]}"
                         +non-ascii-text+)))
    (multiple-value-bind (events result) (collect-stream)
      (is (equal +non-ascii-text+ (getf (first events) :text)))
      (is (equal +non-ascii-text+
                 (miao:content-text (getf (second result) :content)))))))

;;; --- errors -----------------------------------------------------------

(test openai-a-non-ok-status-is-a-backend-error
  (with-openai ('(429 ("Content-Type" "application/json")
                  "{\"error\":{\"message\":\"rate limited\"}}"))
    (let ((reason (miao:tool-error (ask))))
      (is (eq :backend-error (first reason)))
      (is (= 429 (second reason)))
      (is (search "rate limited" (third reason))))))

(test openai-a-retry-after-header-reaches-the-error
  (with-openai ('(429 ("Content-Type" "application/json" "Retry-After" "2")
                  "{\"error\":\"rate limited\"}"))
    (is (equal '(:retry-after 2000) (cdddr (miao:tool-error (ask)))))))

(test a-malformed-payload-is-a-backend-error
  (with-openai ((json-response "{\"choices\":"))
    (is (eq :backend-error (first (miao:tool-error (ask)))))))

(test a-reply-with-no-choices-is-a-backend-error
  (with-openai ((json-response "{\"choices\":[]}"))
    (is (eq :backend-error (first (miao:tool-error (ask)))))))

(test openai-a-connection-closed-before-a-response-is-unavailable
  (with-openai (:close)
    (is (eq :unavailable (miao:tool-error (ask))))))

(test openai-an-unreachable-backend-is-unavailable
  (with-openai ((json-response +hello-reply+))
    ;; Port 1 on the loopback: nothing listens, so nothing is ever read.
    (is (eq :unavailable
            (miao:tool-error
             (miao:complete :protocol-openai
                            :base-url "http://127.0.0.1:1/v1"
                            :model "test-model"
                            :timeout 5000
                            :messages '((:role :user :content "hello"))))))))

;;; --- live -------------------------------------------------------------

(defun live-turn (base-url model &rest extra)
  (apply #'miao:complete :protocol-openai
         :base-url base-url :model model :timeout 120000 extra))

(defun model-missing-p (result)
  "A backend that answers but does not know the model, so the tag named by
MIAO_OLLAMA_MODEL has not been pulled."
  (let ((reason (miao:tool-error result)))
    (and (consp reason) (eq :backend-error (first reason)) (= 404 (second reason)))))

(test openai-live-completion
  ;; Off by default: CI must not depend on a model being installed. Ollama
  ;; serves this API at http://127.0.0.1:11434/v1 with no key.
  (let ((base-url (uiop:getenv "MIAO_OLLAMA_URL"))
        (model (or (uiop:getenv "MIAO_OLLAMA_MODEL") "llama3.2")))
    (if (null base-url)
        (skip "set MIAO_OLLAMA_URL to run live Ollama tests")
        (let* ((registry (make-instance 'm:registry))
               (m:*registry* registry)
               (context (m:start-service (make-instance 'm:context :name :live)
                                         :registry registry)))
          (unwind-protect
               (progn
                 (m:mount context 'miao:protocol-openai)
                 (let ((result (live-turn base-url model
                                          :messages '((:role :user
                                                       :content "Reply with the word ok.")))))
                   (if (model-missing-p result)
                       (skip "~a has no model ~a; set MIAO_OLLAMA_MODEL" base-url model)
                       (progn
                         (is (eq :ok (first result)))
                         (is (plusp (length (miao:content-text
                                             (getf (second result) :content)))))
                         ;; And once more over SSE, where the deltas are
                         ;; genuinely progressive rather than a body read at
                         ;; once.
                         (let* ((events '())
                                (streamed
                                  (live-turn base-url model
                                             :ref :live
                                             :stream (lambda (event) (push event events))
                                             :messages '((:role :user
                                                          :content "Count to three.")))))
                           (is (eq :ok (first streamed)))
                           (is (plusp (count :text-delta events
                                             :key (lambda (event) (getf event :type)))))
                           (is (eq :done (getf (first events) :type))))))))
            (m:stop context))))))
