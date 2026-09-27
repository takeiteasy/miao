(in-package #:miao)

;;; The OpenAI chat completions wire shape: POST <base-url>/chat/completions,
;;; with optional SSE streaming. One protocol for every backend speaking it --
;;; OpenRouter, Ollama, Groq, vLLM, LM Studio, llama.cpp-server -- which differ
;;; only in provider data. See docs/protocols.md.
;;;
;;; Base URL, model and auth travel in the request rather than in a config
;;; slot: one mounted service answers for every backend of the shape at once.

(defparameter +openai-params+
  '((:temperature number :doc "sampling temperature")
    (:top-p number :doc "nucleus sampling cutoff")
    (:max-tokens (integer 1) :doc "cap on generated tokens")
    (:stop (array-of string) :doc "sequences that end the turn")
    (:seed integer :doc "sampling seed, where the backend honours one"))
  "The sampling parameters the shape understands, advertised and rendered
from one declaration.")

(define-protocol :protocol-openai
    (:summary "OpenAI-compatible chat completions" :params +openai-params+)
    (service request)
  (let ((problem (check-openai-request request)))
    (if problem
        (bad-request "~a" problem)
        (perform-completion request #'open-completion #'read-completion))))

(defun check-openai-request (request)
  "NIL when REQUEST carries the provider data this protocol needs, else a
problem string. The shared CHECK-REQUEST stays neutral, so this runs on top."
  (let ((base-url (getf request :base-url))
        (model (getf request :model)))
    (cond
      ((not (named-p base-url)) ":base-url is required, naming the backend")
      ((not (http-url-p base-url)) ":base-url must be an http or https URL")
      ((not (named-p model)) ":model is required")
      (t nil))))

(defun completion-url (base-url)
  (format nil "~a/chat/completions" (string-right-trim "/" base-url)))

;;; --- the request ------------------------------------------------------

(defun completion-body (request streaming)
  (let* ((tools (getf request :tools))
         (json (json-object "model" (getf request :model)
                            "messages" (map 'vector
                                            (lambda (message)
                                              (message->json message tools))
                                            (getf request :messages))
                            "stream" streaming)))
    (when tools
      (setf (gethash "tools" json) (tools->json tools)))
    (dolist (param +openai-params+ json)
      (let ((value (getf request (param-name param) *absent*)))
        (unless (eq value *absent*)
          (setf (gethash (wire-key (param-name param)) json)
                (value->json value (param-type param))))))))

(defun message->json (message tools)
  (let* ((role (getf message :role))
         (content (getf message :content))
         (json (json-object "role" (string-downcase (symbol-name role))
                            ;; Null, not "": an assistant turn that is only
                            ;; tool calls carries no content at all.
                            "content" (if content (content-text content) 'null))))
    (when (eq role :tool)
      (setf (gethash "tool_call_id" json) (getf message :tool-call-id)))
    (a:when-let ((calls (getf message :tool-calls)))
      (setf (gethash "tool_calls" json)
            (map 'vector (lambda (call) (tool-call->json call tools)) calls)))
    json))

(defun tool-call->json (call tools)
  (json-object
   "id" (getf call :id)
   "type" "function"
   "function" (json-object
               "name" (wire-tool-name (getf call :name))
               "arguments" (json:stringify (call-arguments->json call tools)))))

;;; --- the exchange -----------------------------------------------------

(defun open-completion (request connect)
  "POST the request body, answering the response stream and its status."
  (let ((streaming (and (getf request :stream) t))
        (url (completion-url (getf request :base-url))))
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
         :content (json:stringify (completion-body request streaming)))
      (values (character-stream body) status headers))))

(defun read-completion (request stream status)
  (if (getf request :stream)
      (read-streamed-completion request stream status)
      (read-whole-completion request stream status)))

;;; --- the reply --------------------------------------------------------

(defun reply-meta (json)
  (let ((meta '()))
    (a:when-let ((id (gethash "id" json)))
      (when (stringp id) (setf meta (list :id id))))
    (a:when-let ((usage (gethash "usage" json)))
      (when (hash-table-p usage)
        (setf meta (list* :usage (json->arguments usage nil) meta))))
    meta))

(defun read-whole-completion (request stream status)
  (let* ((json (json:parse (uiop:slurp-stream-string stream)))
         (choice (first-choice json status))
         (message (gethash "message" choice)))
    (unless (hash-table-p message)
      (return-from read-whole-completion
        (backend-error status "a choice carries no message")))
    (make-reply (text-of (gethash "content" message))
                (message-tool-calls message (getf request :tools))
                (finish-reason (gethash "finish_reason" choice))
                (reply-meta json))))

(defun first-choice (json status)
  (let ((choices (gethash "choices" json)))
    (unless (and (vectorp choices) (plusp (length choices)))
      (error "no choices in a ~d response" status))
    (aref choices 0)))

(defun message-tool-calls (message tools)
  (let ((calls (gethash "tool_calls" message)))
    (when (and (vectorp calls) (not (stringp calls)))
      (map 'list (lambda (call) (wire-call->lisp call tools)) calls))))

(defun wire-call->lisp (call tools)
  (let* ((function (gethash "function" call))
         (name (lisp-tool-name (gethash "name" function))))
    (make-call (gethash "id" call) name
               (parse-arguments (gethash "arguments" function)
                                (call-schema name tools))
               tools)))

(defun parse-arguments (text schema)
  "TEXT, a JSON object as a string, as an argument plist. An empty argument
list arrives as \"\" as readily as \"{}\"."
  (if (or (null text) (zerop (length text)))
      nil
      (json->arguments (json:parse text) schema)))

;;; --- streaming --------------------------------------------------------

;;; Deltas accumulate into the same reply the whole-response path returns, so
;;; a caller may ignore the sink entirely and read only the (:ok ...).

(defstruct (streamed-call (:conc-name call-))
  id name (arguments (make-string-output-stream)))

(defun read-streamed-completion (request stream status)
  (let ((sink (getf request :stream))
        (ref (getf request :ref))
        (text (make-string-output-stream))
        (calls '())
        (reason nil)
        (meta '())
        (ended nil))
    (loop
      (multiple-value-bind (data eof) (read-sse-event stream)
        (when data
          (if (string= data "[DONE]")
              (setf ended t)
              (let ((chunk (json:parse data)))
                (setf meta (or (reply-meta chunk) meta))
                (multiple-value-setq (calls reason ended)
                  (absorb-chunk chunk sink ref text calls reason ended)))))
        (when (or eof ended) (return))))
    ;; Neither a [DONE] nor a finish_reason means the stream stopped early.
    (unless ended
      (return-from read-streamed-completion
        (backend-error status "the stream ended before the turn did")))
    (make-reply (get-output-stream-string text)
                (streamed-calls calls (getf request :tools))
                reason meta)))

(defun absorb-chunk (chunk sink ref text calls reason ended)
  (let ((choices (gethash "choices" chunk)))
    (when (and (vectorp choices) (plusp (length choices)))
      (let* ((choice (aref choices 0))
             (delta (gethash "delta" choice))
             (finish (finish-reason (gethash "finish_reason" choice))))
        (when finish (setf reason finish ended t))
        (when (hash-table-p delta)
          (a:when-let ((content (gethash "content" delta)))
            (when (and (stringp content) (plusp (length content)))
              (write-string content text)
              (emit-event sink (text-delta ref content))))
          (let ((fragments (gethash "tool_calls" delta)))
            (when (and (vectorp fragments) (not (stringp fragments)))
              (map nil (lambda (fragment)
                         (setf calls (absorb-call fragment sink ref calls)))
                   fragments)))))))
  (values calls reason ended))

(defun absorb-call (fragment sink ref calls)
  "One tool_calls fragment. Id and name reach the wire on the first fragment
of an index only; both are remembered and stamped on every delta, so a
consumer of the sink alone reassembles without tracking arrival order."
  (let* ((index (or (gethash "index" fragment) 0))
         (call (or (cdr (assoc index calls))
                   (let ((fresh (make-streamed-call)))
                     (setf calls (append calls (list (cons index fresh))))
                     fresh)))
         (function (gethash "function" fragment))
         (arguments (and (hash-table-p function)
                         (gethash "arguments" function))))
    (a:when-let ((id (gethash "id" fragment)))
      (when (stringp id) (setf (call-id call) id)))
    (when (hash-table-p function)
      (a:when-let ((name (gethash "name" function)))
        (when (stringp name) (setf (call-name call) (lisp-tool-name name)))))
    (when (stringp arguments)
      (write-string arguments (call-arguments call)))
    (emit-event sink (tool-call-delta ref
                                      :id (call-id call)
                                      :name (call-name call)
                                      :arguments (if (stringp arguments)
                                                     arguments
                                                     "")))
    calls))

(defun streamed-calls (calls tools)
  (loop for (nil . call) in calls
        collect (make-call (call-id call) (call-name call)
                           (parse-arguments
                            (get-output-stream-string (call-arguments call))
                            (call-schema (call-name call) tools))
                           tools)))

;;; --- SSE --------------------------------------------------------------

(defun read-sse-event (stream)
  "The next event's data payload and T at end of stream. Fields other than
data, and the comment lines a backend sends to keep a connection warm, are
skipped; an event's several data lines join with a newline."
  (let ((data '()))
    (loop
      (let ((line (read-line stream nil nil)))
        (cond
          ((null line) (return (values (join-data data) t)))
          (t (let ((field (string-right-trim '(#\Return) line)))
               (cond
                 ((string= field "")
                  (when data (return (values (join-data data) nil))))
                 ((sse-data-p field)
                  (push (string-left-trim " " (subseq field 5)) data))))))))))

(defun sse-data-p (field)
  (and (>= (length field) 5) (string= "data:" field :end2 5)))

(defun join-data (data)
  (when data
    (format nil "~{~a~^~%~}" (reverse data))))
