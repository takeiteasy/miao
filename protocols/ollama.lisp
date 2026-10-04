(in-package #:miao)

;;; Ollama's native chat endpoint: POST <base-url>/api/chat, with optional
;;; NDJSON streaming. Distinct from :PROTOCOL-OPENAI because the wire shape
;;; differs, not just the base URL -- this is what carries the usage counters
;;; and options the OpenAI-compatible /v1 route drops. See docs/protocols.md.

(defparameter +ollama-params+
  '((:temperature number :doc "sampling temperature" :nest "options")
    (:top-p number :doc "nucleus sampling cutoff" :nest "options")
    (:top-k (integer 1) :doc "top-k sampling cutoff" :nest "options")
    (:num-ctx (integer 1) :doc "context window in tokens" :nest "options")
    (:num-predict integer :doc "cap on generated tokens" :nest "options")
    (:stop (array-of string) :doc "sequences that end the turn" :nest "options")
    (:seed integer :doc "sampling seed" :nest "options")
    (:mirostat (integer 0) :doc "mirostat sampling mode" :nest "options")
    (:format string :doc "response format: json, or a JSON schema")
    (:keep-alive string :doc "how long to hold the model resident"))
  "The parameters the native endpoint understands. :NEST names an option a
declared parameter is not itself: an entry with one groups under that key in
the request body rather than at the top level.")

(define-protocol :protocol-ollama
    (:summary "Ollama native chat" :params +ollama-params+)
    (service request)
  (let ((problem (check-ollama-request request)))
    (if problem
        (bad-request "~a" problem)
        (perform-completion request #'open-chat #'read-chat))))

(defun check-ollama-request (request)
  "NIL when REQUEST carries the provider data this protocol needs, else a
problem string. The shared CHECK-REQUEST stays neutral, so this runs on top."
  (let ((base-url (getf request :base-url))
        (model (getf request :model)))
    (cond
      ((not (named-p base-url)) ":base-url is required, naming the backend")
      ((not (http-url-p base-url)) ":base-url must be an http or https URL")
      ((not (named-p model)) ":model is required")
      (t nil))))

(defun chat-url (base-url)
  (format nil "~a/api/chat" (string-right-trim "/" base-url)))

;;; --- the request --------------------------------------------------------

(defun chat-body (request streaming)
  (let* ((tools (getf request :tools))
         (json (json-object "model" (getf request :model)
                            "messages" (map 'vector
                                            (lambda (message)
                                              (ollama-message->json message tools))
                                            (getf request :messages))
                            "stream" streaming))
         (options (json-object)))
    (when tools
      (setf (json-get json "tools") (tools->json tools)))
    (dolist (param +ollama-params+)
      (let ((value (getf request (param-name param) *absent*)))
        (unless (eq value *absent*)
          (let ((rendered (value->json value (param-type param))))
            (if (getf (param-options param) :nest)
                (setf (json-get options (wire-key (param-name param))) rendered)
                (setf (json-get json (wire-key (param-name param))) rendered))))))
    (when (json-object-pairs options)
      (setf (json-get json "options") options))
    json))

(defun ollama-message->json (message tools)
  "Like MESSAGE->JSON, but a call's arguments are a JSON object on the wire
here rather than a stringified one, and there is no tool_call_id: native
Ollama correlates a tool result by position, not by id."
  (let* ((role (getf message :role))
         (content (getf message :content))
         (json (json-object "role" (string-downcase (symbol-name role))
                            "content" (if content (content-text content) 'null))))
    (a:when-let ((calls (getf message :tool-calls)))
      (setf (json-get json "tool_calls")
            (map 'vector (lambda (call) (ollama-call->json call tools)) calls)))
    json))

(defun ollama-call->json (call tools)
  (json-object
   "function" (json-object
               "name" (wire-tool-name (getf call :name))
               "arguments" (call-arguments->json call tools))))

;;; --- the exchange ---------------------------------------------------------

(defun open-chat (request connect)
  (let ((streaming (and (getf request :stream) t))
        (url (chat-url (getf request :base-url))))
    (multiple-value-bind (body status headers)
        (drakma:http-request
         url
         :method :post
         :stream (funcall connect url)
         :redirect nil
         :want-stream t
         :close t
         :additional-headers (header-alist (getf request :headers))
         :content-type "application/json"
         :external-format-out :utf-8
         :content (json:stringify (chat-body request streaming)))
      (values (character-stream body) status headers))))

(defun read-chat (request stream status)
  (if (getf request :stream)
      (read-streamed-chat request stream status)
      (read-whole-chat request stream status)))

;;; --- the reply --------------------------------------------------------

;;; The native reply is flat -- no choices array -- and its usage counters sit
;;; at the top level rather than under a "usage" key.

(defparameter +ollama-usage-keys+
  '("prompt_eval_count" "eval_count" "total_duration" "load_duration"
    "prompt_eval_duration" "eval_duration")
  "Native counters, collected into :META :USAGE as a plist keyed the same way
OpenAI's :usage arrives, plus :PROMPT-TOKENS, the key every protocol's :usage
carries for the prompt's size.")

(defun chat-meta (json)
  (let ((usage (loop for key in +ollama-usage-keys+
                     for value = (gethash key json)
                     when value
                       collect (lisp-key key) and collect value)))
    (a:when-let ((prompt (getf usage :prompt-eval-count)))
      (setf usage (list* :prompt-tokens prompt usage)))
    (append (when usage (list :usage usage))
            (a:when-let ((model (gethash "model" json)))
              (list :model model)))))

(defun read-whole-chat (request stream status)
  (let* ((json (json:parse (uiop:slurp-stream-string stream)))
         (message (gethash "message" json)))
    (unless (hash-table-p message)
      (return-from read-whole-chat
        (backend-error status "a response carries no message")))
    (make-reply (text-of (gethash "content" message))
                (chat-message-tool-calls message (getf request :tools))
                (finish-reason (gethash "done_reason" json))
                (chat-meta json))))

(defun chat-message-tool-calls (message tools)
  (let ((calls (gethash "tool_calls" message)))
    (when (and (vectorp calls) (not (stringp calls)))
      (loop for index from 0
            for call across calls
            collect (chat-call->lisp call index tools)))))

;;; Native tool calls carry no id on the wire; CHECK-TOOL-CALL requires one on
;;; any call sent back in a later message, so one is synthesised here, local
;;; to this reply rather than a value the backend assigned.

(defun chat-call->lisp (call index tools)
  (let* ((function (gethash "function" call))
         (name (lisp-tool-name (gethash "name" function))))
    (make-call (format nil "call_~d" index) name
               (json->arguments (gethash "arguments" function)
                                (call-schema name tools))
               tools)))

;;; --- streaming ----------------------------------------------------------

;;; NDJSON, not SSE: one bare JSON object per line, no "data:" prefix and no
;;; [DONE]. Ollama sends a whole tool_calls array per chunk rather than
;;; splitting it into fragments, so there is no reassembly to do; the usage
;;; counters arrive only on the final done:true line.

(defun read-streamed-chat (request stream status)
  (let ((sink (getf request :stream))
        (ref (getf request :ref))
        (text (make-string-output-stream))
        (calls '())
        (reason nil)
        (meta '())
        (ended nil))
    (loop
      (let ((line (read-line stream nil nil)))
        (cond
          ((null line) (return))
          ((zerop (length (string-trim '(#\Return #\Space) line))))
          (t (let ((chunk (json:parse (string-right-trim '(#\Return) line))))
               (multiple-value-bind (chunk-calls chunk-reason chunk-ended chunk-meta)
                   (absorb-ollama-chunk chunk sink ref text (getf request :tools))
               (setf calls (append calls chunk-calls))
               (when chunk-reason (setf reason chunk-reason))
               (when chunk-meta (setf meta chunk-meta))
               (when chunk-ended (setf ended t) (return))))))))
    (unless ended
      (return-from read-streamed-chat
        (backend-error status "the stream ended before the turn did")))
    (make-reply (get-output-stream-string text) calls reason meta)))

(defun absorb-ollama-chunk (chunk sink ref text tools)
  "One NDJSON line. Answers (values calls reason ended meta), the last two set
only on the final done:true line."
  (let ((message (gethash "message" chunk))
        (done (gethash "done" chunk))
        (calls '()))
    (when (hash-table-p message)
      (a:when-let ((content (gethash "content" message)))
        (when (and (stringp content) (plusp (length content)))
          (write-string content text)
          (emit-event sink (text-delta ref content))))
      (let ((wire-calls (gethash "tool_calls" message)))
        (when (and (vectorp wire-calls) (not (stringp wire-calls)))
          (setf calls (loop for index from 0
                            for call across wire-calls
                            collect (chat-call->lisp call index tools)))
          (dolist (call calls)
            (emit-event sink (tool-call-delta
                              ref :id (getf call :id) :name (getf call :name)
                              :arguments (json:stringify
                                          (call-arguments->json call tools))))))))
    (if (eq done t)
        (values calls (finish-reason (gethash "done_reason" chunk)) t (chat-meta chunk))
        (values calls nil nil nil))))
