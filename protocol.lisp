(in-package #:miao)

;;; The protocol convention. A protocol is a meow service named
;;; :PROTOCOL-<name> whose METADATA carries :KIND :PROTOCOL, and which answers
;;; (:describe) and (:complete . plist). It translates one neutral model
;;; contract onto one wire shape; base URL, auth and model catalogue are
;;; provider data layered on top. See docs/protocols.md.

(defparameter +protocol-roles+ '(:system :user :assistant :tool)
  "The closed set of message roles.")

(defun protocols (&key (registry m:*registry*))
  "Every registered protocol name, sorted."
  (%registered-of-kind :protocol :registry registry))

(defun %protocol-process (name &key (registry m:*registry*))
  "NAME's process and its registration props. A provider answers the same
messages, so both reach a backend through here."
  (multiple-value-bind (process props) (m:lookup name :registry registry)
    (unless process (error "Nothing registered under ~s." name))
    (values process props)))

(defun describe-protocol (name &key (registry m:*registry*))
  "NAME's metadata plist."
  (m:call (%protocol-process name :registry registry) '(:describe)))

;;; --- connect refusal ---------------------------------------------------

;;; usocket's SBCL :timeout connect path does a non-blocking connect, then
;;; polls GETPEERNAME to detect success. On Linux, a refused connect leaves
;;; GETPEERNAME reporting ENOTCONN indefinitely rather than surfacing
;;; SO_ERROR, so a refusal spins until the whole connect timeout elapses
;;; instead of failing immediately.
;;;
;;; TODO: this binds an unexported usocket symbol to fall back to its
;;; legacy blocking connect, which does fail a refusal immediately (an
;;; unreachable host is still bounded by :timeout). Drop it once usocket's
;;; new connect loop checks SO_ERROR itself. Tracked in
;;; ~takeiteasy/miao#61.
(defmacro with-immediate-connect-refusal (&body body)
  "Run BODY -- which must make its USOCKET:SOCKET-CONNECT call directly,
inside the same thread -- so a refused connection fails at once rather
than waiting out the connect timeout."
  `(let ((usocket::*socket-connect-nonblock-wait* nil))
     ,@body))

;;; --- content ----------------------------------------------------------

(defun text-block (text) (list :type :text :text text))

(defun normalize-content (content)
  "CONTENT as a list of typed blocks. A flat string is the degenerate
single-text-block case."
  (etypecase content
    (null nil)
    (string (list (text-block content)))
    (cons content)))

(defun content-text (content)
  "The text blocks of CONTENT, concatenated."
  (with-output-to-string (out)
    (dolist (block (normalize-content content))
      (when (eq (getf block :type) :text)
        (write-string (getf block :text "") out)))))

;;; --- names and keys -----------------------------------------------------

;;; Shared by every protocol that speaks JSON over HTTP: what OpenAI and
;;; Ollama's native endpoint both need, so the second protocol to want it
;;; found it already here rather than duplicated (~takeiteasy/miao#33).

(defun named-p (value)
  (and (stringp value) (plusp (length value))))

(defun http-url-p (url)
  ;; Checked here so that everything past it failing before a status line is
  ;; a transport failure and nothing else.
  (some (lambda (scheme)
          (and (>= (length url) (length scheme))
               (string-equal scheme url :end2 (length scheme))))
        '("http://" "https://")))

(defun wire-key (name)
  "NAME as a JSON property name: :MAX-TOKENS is max_tokens."
  (substitute #\_ #\- (string-downcase (symbol-name name))))

(defun lisp-key (key)
  (a:make-keyword (string-upcase (substitute #\- #\_ key))))

(defun wire-tool-name (name)
  (string-downcase (symbol-name name)))

(defun lisp-tool-name (name)
  (a:make-keyword (string-upcase name)))

;;; --- values -------------------------------------------------------------

;;; Tool arguments are a plist in the contract and a JSON object on the wire.
;;; The tool's own schema names each value's type, so a map and an object stay
;;; distinguishable where a plist alone leaves them ambiguous.

(defun call-schema (name tools)
  "The schema of the tool NAME names among TOOLS, a list of tool metadata."
  (a:when-let ((metadata (find name tools :key (lambda (m) (getf m :name)))))
    (tool-schema metadata)))

(defun make-call (id name arguments tools)
  "A tool call as a reply carries it. It keeps the schema of the tool it names
among TOOLS, so it renders the same wherever it is replayed."
  (let ((schema (call-schema name tools)))
    (list* :id id :name name :arguments arguments
           (when schema (list :schema schema)))))

(defun call-arguments->json (call tools)
  "CALL's arguments as JSON, by the schema the call carries, else by the tool
TOOLS names."
  (arguments->json (getf call :arguments)
                   (or (getf call :schema)
                       (call-schema (getf call :name) tools))))

(defun arguments->json (arguments schema)
  (let ((json (json-object)))
    (loop for (name value) on arguments by #'cddr
          for param = (find name schema :key #'param-name)
          do (setf (json-get json (wire-key name))
                   (value->json value (and param (param-type param)))))
    json))

(defun value->json (value spec)
  (cond
    ((null spec) (untyped->json value))
    ((spec-is spec "OR") (if (null value) 'null (value->json value (third spec))))
    ((spec-is spec "ARRAY-OF")
     (map 'vector (lambda (element) (value->json element (second spec))) value))
    ((spec-is spec "MAP-OF")
     (let ((json (json-object)))
       (loop for (key entry) on value by #'cddr
             do (setf (json-get json (as-text key))
                      (value->json entry (second spec))))
       json))
    ((spec-is spec "OBJECT") (arguments->json value (rest spec)))
    ((spec-is spec "ANY") (untyped->json value))
    (t (json-value value))))

;;; A call built by hand carries no schema, so it renders by this guess: a plist
;;; of keywords is an object and any other list an array.

(defun untyped->json (value)
  (cond
    ((null value) nil)
    ((keywordp value) (json-value value))
    ((not (consp value)) value)
    ((and (evenp (length value))
          (loop for (name) on value by #'cddr always (keywordp name)))
     (let ((json (json-object)))
       (loop for (name entry) on value by #'cddr
             do (setf (json-get json (wire-key name)) (untyped->json entry)))
       json))
    (t (map 'vector #'untyped->json value))))

(defun json->arguments (json schema)
  "JSON, a parsed object, as an argument plist. Names become keywords, which
is what COERCE-ARGS matches a schema on."
  (let ((plist '()))
    (maphash (lambda (key value)
               (let* ((name (lisp-key key))
                      (param (find name schema :key #'param-name)))
                 (push name plist)
                 (push (json->value value (and param (param-type param))) plist)))
             json)
    (nreverse plist)))

(defun json->value (value spec)
  (cond
    ((eq value 'null) nil)
    ((null spec) (untyped->lisp value))
    ((spec-is spec "OR") (json->value value (third spec)))
    ((spec-is spec "ARRAY-OF")
     (map 'list (lambda (element) (json->value element (second spec))) value))
    ((spec-is spec "MAP-OF")
     (loop for key being the hash-keys of value using (hash-value entry)
           collect key collect (json->value entry (second spec))))
    ((spec-is spec "OBJECT") (json->arguments value (rest spec)))
    ((spec-is spec "ANY") (untyped->lisp value))
    ;; A member arrives as its name and an integer as digits; COERCE-ARGS
    ;; takes both, so a scalar passes through untouched.
    (t value)))

(defun untyped->lisp (value)
  (cond
    ((eq value 'null) nil)
    ((hash-table-p value) (json->arguments value nil))
    ((and (vectorp value) (not (stringp value)))
     (map 'list #'untyped->lisp value))
    (t value)))

;;; --- the tools array ------------------------------------------------------

;;; Each tool's schema renders straight through SCHEMA->JSON-SCHEMA, in the
;;; shape both OpenAI and Ollama's native endpoint use:
;;; {"type":"function","function":{name,description,parameters}}.

(defun tools->json (tools)
  (map 'vector
       (lambda (metadata)
         (let ((function (json-object
                          "name" (wire-tool-name (getf metadata :name))
                          "parameters" (schema->json-schema
                                        (tool-schema metadata)))))
           (a:when-let ((summary (getf metadata :summary)))
             (setf (json-get function "description") summary))
           (json-object "type" "function" "function" function)))
       tools))

;;; --- requests ---------------------------------------------------------

;;; COERCE-ARGS is deliberately not used here: it rejects a key its schema
;;; does not name, and the contract ignores unknown keys so a portable caller
;;; may offer a superset. The checks below are the whole pre-flight.

(defun check-request (request)
  "NIL when REQUEST satisfies the contract, else a problem string."
  (cond
    ((not (and (listp request) (evenp (length request))))
     "request must be a plist")
    ((not (listp (getf request :messages)))
     "messages must be a list")
    ((null (getf request :messages))
     "messages is required")
    (t (some #'check-message (getf request :messages)))))

(defun check-message (message)
  (let ((role (getf message :role)))
    (cond
      ((not (and (listp message) (evenp (length message))))
       (format nil "message must be a plist, got ~s" message))
      ((not (member role +protocol-roles+))
       (format nil "role must be one of ~{~(~s~)~^, ~}, got ~s"
               +protocol-roles+ role))
      ((and (eq role :tool) (not (getf message :tool-call-id)))
       "a tool message must carry :tool-call-id")
      (t (some #'check-tool-call (getf message :tool-calls))))))

(defun check-tool-call (call)
  (unless (and (listp call) (getf call :id) (getf call :name))
    (format nil "a tool call must carry :id and :name, got ~s" call)))

;;; A request's :DEPTH, stamped by NESTED-REQUEST, picks the pool its job runs
;;; in. Nothing else sets it, and it never reaches the wire.

(defvar *completion-depth* nil
  "The depth of the completion job this thread is running, or nil outside one.")

(defun carry-completion-depth (function)
  "FUNCTION as a closure that runs at the completion depth of the thread that
called this, for a thread a protocol body spawns to make its own completions."
  (let ((depth *completion-depth*))
    (lambda ()
      (let ((*completion-depth* depth))
        (funcall function)))))

(defun nested-request (request)
  "REQUEST as a completion made from this thread: one deeper than the job
running it, or unchanged in depth when made outside a job."
  (let ((request (a:remove-from-plist request :depth)))
    (if *completion-depth*
        (list* :depth (1+ *completion-depth*) request)
        request)))

(defun %completion-call (name request &key (registry m:*registry*))
  "What to send NAME for REQUEST: (values process message timeout), TIMEOUT
in seconds. A request that fails its pre-flight, or nests too deep, answers
(values nil result) instead. Signals when nothing is registered under NAME."
  (let* ((request (nested-request request))
         (problem (or (check-request request)
                      (and (> (getf request :depth 0) *max-completion-depth*)
                           (format nil "completions nested past depth ~d"
                                   *max-completion-depth*)))))
    (if problem
        (values nil (bad-request "~a" problem))
        (multiple-value-bind (process props) (%protocol-process name :registry registry)
          (values process
                  (list* :complete request)
                  ;; A provider delegates to its protocol, so the reply
                  ;; travels two hops and each waiter needs its own margin.
                  (%caller-timeout request (if (eq (getf props :kind) :provider) 2 1)))))))

(defun complete (name &rest request)
  "Perform one turn against protocol or provider NAME. Returns (:ok plist) or
(:error reason)."
  (multiple-value-bind (process message timeout) (%completion-call name request)
    (if process
        (multiple-value-call #'%call-result (m:call process message :timeout timeout))
        message)))

;;; --- streaming --------------------------------------------------------

;;; A neutral event vocabulary, never raw provider chunks: the agent loop and
;;; the UIs consume these. A turn ends with exactly one :DONE, whose reason is
;;; the finish reason or, when the exchange failed, the failed result.

(defun text-delta (ref text)
  (list :type :text-delta :ref ref :text text))

(defun tool-call-delta (ref &key id name arguments)
  "ARGUMENTS is a fragment of the call's argument text, which arrives split
across deltas."
  (list :type :tool-call-delta :ref ref :id id :name name :arguments arguments))

(defun done (ref &optional reason)
  (list :type :done :ref ref :reason reason))

(defstruct (emitter (:constructor %make-emitter (sink)))
  sink (queue '()) (lock (bt:make-lock))
  scheduled stopping dead finished job thread
  (drained (bt:make-semaphore)))

(defstruct (fanout (:constructor make-fanout (&optional targets recording)))
  (lock (bt:make-lock)) targets recording
  ;; Newest first, kept only when RECORDING.
  history)

(defun delta-field (event)
  "The key of EVENT's streamed fragment when EVENT is a delta, else nil."
  (case (getf event :type)
    (:text-delta :text)
    (:tool-call-delta :arguments)))

(defun continues-p (last event field)
  "Whether EVENT continues the delta LAST, FIELD being the key of its fragment."
  (and (eq (getf event :type) (getf last :type))
       (equal (getf event :ref) (getf last :ref))
       (eq (and (member :parent event) t) (and (member :parent last) t))
       (stringp (getf event field))
       (stringp (getf last field))
       (or (eq field :text)
           (and (getf event :id)
                (equal (getf event :id) (getf last :id))
                (equal (getf event :name) (getf last :name))))))

;; TODO: a run with many separate events or tool calls still stays in the
;; history until it ends; cap it with a truncation marker in the replay if
;; that grows too large (#220).
(defun record-event (fanout event)
  "Add EVENT to FANOUT's history, merging a streamed text or tool-call
fragment into the last entry when it continues it. The merged entry is a new
plist: the one it replaces may still be queued at a sink. Called holding the
lock."
  (let* ((last (first (fanout-history fanout)))
         (field (delta-field event)))
    (if (and last field (continues-p last event field))
        (setf (first (fanout-history fanout))
              (list* field (concatenate 'string (getf last field) (getf event field))
                     (a:remove-from-plist last field)))
        (push event (fanout-history fanout)))))

(defun fanout-add (fanout target &key replay)
  "Add TARGET. With REPLAY, it first gets the events recorded so far, so it
hears each event once, in order: those recorded before now by the replay and
those after by the fanout itself."
  (bt:with-lock-held ((fanout-lock fanout))
    (when (and replay (fanout-recording fanout))
      (dolist (event (reverse (fanout-history fanout)))
        (emit-event target event)))
    (pushnew target (fanout-targets fanout))))

(defun fanout-remove (fanout target)
  (bt:with-lock-held ((fanout-lock fanout))
    (setf (fanout-targets fanout) (remove target (fanout-targets fanout)))))

(defun fanout-listening-p (fanout)
  "Whether anything is on the far end of FANOUT, however deeply nested."
  (some (lambda (target)
          (if (typep target 'fanout) (fanout-listening-p target) t))
        (bt:with-lock-held ((fanout-lock fanout))
          (fanout-targets fanout))))

(defun emit-event (sink event)
  "Deliver EVENT to SINK, a function, a meow process, an emitter or a fanout
of those. A null sink drops it."
  (etypecase sink
    (null nil)
    (m:process (m:send sink event))
    (emitter (emitter-send sink event))
    (fanout (dolist (target (bt:with-lock-held ((fanout-lock sink))
                              (when (fanout-recording sink)
                                (record-event sink event))
                              (reverse (fanout-targets sink))))
              (emit-event target event)))
    ((or function symbol) (funcall sink event)))
  event)

;;; --- results ----------------------------------------------------------

;;; The docs/tools.md error vocabulary, plus one shape protocols add.

(defun backend-error (status detail &key retry-after)
  "The backend was reached and the exchange broke down: a non-OK STATUS, a
malformed payload, a stream cut short. RETRY-AFTER, in milliseconds, is how
long the backend asked a caller to wait before trying again."
  (fail (list* :backend-error status detail
               (when retry-after (list :retry-after retry-after)))))

;;; --- Retry-After ------------------------------------------------------

(defparameter +months+
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun http-date-universal-time (text)
  "TEXT, an IMF-fixdate such as \"Wed, 21 Oct 2026 07:28:00 GMT\", as a
universal time, or nil when it is anything else."
  (ignore-errors
   (destructuring-bind (day-name day month-name year clock zone)
       (uiop:split-string text :separator " ")
     (declare (ignore day-name))
     (let ((month (position month-name +months+ :test #'string-equal)))
       (when (and month (string= zone "GMT"))
         (destructuring-bind (hour minute second)
             (mapcar #'parse-integer (uiop:split-string clock :separator ":"))
           (encode-universal-time second minute hour (parse-integer day) (1+ month)
                                  (parse-integer year) 0)))))))

(defun parse-decimal (text)
  "TEXT as a non-negative decimal, or nil."
  (ignore-errors
   (destructuring-bind (whole &optional (fraction "")) (uiop:split-string text :separator ".")
     (and (every #'digit-char-p whole) (every #'digit-char-p fraction)
          (plusp (+ (length whole) (length fraction)))
          (+ (if (plusp (length whole)) (parse-integer whole) 0)
             (if (plusp (length fraction))
                 (/ (parse-integer fraction) (expt 10 (length fraction)))
                 0))))))

(defun retry-after-ms (headers)
  "The wait, in milliseconds, HEADERS ask for, or nil. HEADERS is drakma's
alist. The non-standard retry-after-ms wins over Retry-After, which is
delta-seconds or an HTTP date; a date already past is 0."
  (flet ((header (name) (a:when-let ((value (cdr (assoc name headers))))
                          (string-trim " " value))))
    (or (a:when-let* ((text (header :retry-after-ms))
                      (ms (parse-decimal text)))
          (round ms))
        (a:when-let ((text (header :retry-after)))
          (a:if-let ((seconds (parse-decimal text)))
            (round (* 1000 seconds))
            (a:when-let ((time (http-date-universal-time text)))
              (max 0 (* 1000 (- time (get-universal-time))))))))))

;;; --- concurrent completions ------------------------------------------------

;;; Each completion runs as a job on the shared worker pool (pool.lisp), so a
;;; protocol or provider answers :DESCRIBE and further completions while one
;;; is in flight. The service still checks and layers the request on its own
;;; process; only the blocking work moves. :MAX-IN-FLIGHT caps how many of a
;;; service's completions run at once, and the rest queue, their time queued
;;; counting against their :TIMEOUT. Stopping the service cancels what is in
;;; flight and what is queued.

(defclass completion-host ()
  ((max-in-flight :initarg :max-in-flight :initform nil
                  :type (or null (integer 1)) :reader host-max-in-flight)
   (in-flight :initform '() :accessor host-in-flight)
   (closing :initform nil :accessor host-closing)
   (drained :initform (bt:make-semaphore) :reader host-drained)
   (in-flight-lock :initform (bt:make-lock) :reader host-lock))
  (:documentation "The state a service needs to run completions concurrently:
its cap, the cancel tokens of those in flight or queued, and whether the
service is stopping."))


(defun track-completion (host token)
  (bt:with-lock-held ((host-lock host))
    (push token (host-in-flight host))))

(defun untrack-completion (host token)
  (bt:with-lock-held ((host-lock host))
    (setf (host-in-flight host) (remove token (host-in-flight host)))
    (when (host-closing host)
      (bt:signal-semaphore (host-drained host)))))

(defparameter *drain-timeout* 5
  "Seconds a stopping service waits for its cancelled completions to answer.")

(defmethod m:dispose ((host completion-host) reason)
  (declare (ignore reason))
  ;; The service's exit settles every call waiting on it as :down, so the
  ;; cancelled workers get to answer first.
  (let ((tokens (bt:with-lock-held ((host-lock host))
                  (setf (host-closing host) t)
                  (copy-list (host-in-flight host))))
        (deadline (+ (get-internal-real-time)
                     (* *drain-timeout* internal-time-units-per-second))))
    (mapc #'cancel tokens)
    (dolist (token tokens)
      (declare (ignore token))
      (bt:wait-on-semaphore
       (host-drained host)
       :timeout (max 0 (/ (- deadline (get-internal-real-time))
                          internal-time-units-per-second)))))
  (call-next-method))

(defun defer-completion (host request function)
  "Call from HANDLE while answering a :complete call: queue FUNCTION on
REQUEST as a pool job and answer the call from there. FUNCTION's request
carries a :CANCEL token of the job's own, which cancelling the caller's token
or stopping HOST also cancels, and the :TIMEOUT left once it starts. The job
runs in the pool of REQUEST's :DEPTH, and completions it makes run one deeper."
  (a:when-let ((cell (m:defer-reply)))
    (let* ((token (make-cancel-token))
           (timeout (getf request :timeout +default-tool-timeout+))
           (depth (getf request :depth 0))
           (queued-at (get-internal-real-time))
           (settled nil)
           (settled-lock (bt:make-lock))
           (job nil))
      (labels ((claim ()
                 (bt:with-lock-held (settled-lock)
                   (unless settled (setf settled t))))
               (answer (result)
                 (when (claim)
                   (m:reply cell result)
                   (untrack-completion host token)))
               (answer-unrun (result)
                 ;; FUNCTION never ran to its end, so nothing ended the stream.
                 (when (claim)
                   (emit-done-detached (getf request :stream) (getf request :ref) result)
                   (m:reply cell result)
                   (untrack-completion host token)))
               (abandon ()
                 (when (pool-abandon job)
                   (spawn-thread (lambda () (answer-unrun (fail :timeout)))
                                 :name "miao-abandon")))
               (remaining ()
                 (- timeout (floor (* 1000 (- (get-internal-real-time) queued-at))
                                   internal-time-units-per-second))))
        (flet ((withdraw (reason)
                 (when (pool-withdraw job)
                   (answer-unrun (fail reason)))))
          (setf job (make-pool-job
                     (lambda ()
                       (let ((result (fail :cancelled))
                             (ran nil)
                             (disarm nil))
                         (unwind-protect
                              (setf result
                                    (let ((left (remaining)))
                                      (if (plusp left)
                                          (handler-case
                                              (let ((*completion-depth* depth))
                                                (setf ran left
                                                      disarm (m:schedule (+ (/ left 1000) *pool-abandon-grace*)
                                                                         #'abandon))
                                                (funcall function
                                                         (list* :cancel token :timeout left
                                                                (a:remove-from-plist request :cancel :timeout :depth))))
                                            (error (e) (fail (list :error (princ-to-string e)))))
                                          (fail :timeout))))
                           (when disarm (funcall disarm))
                           (if ran (answer result) (answer-unrun result)))))
                     :key host :limit (host-max-in-flight host)
                     :registry (m:service-registry host)))
          (if (pool-overloaded-p (pool-for depth))
              (answer-unrun (fail :unavailable))
              (progn
                (track-completion host token)
                (when (pool-submit (pool-for depth) job)
                  (m:after host (/ timeout 1000) (lambda () (withdraw :timeout))))
                (on-cancel token (lambda () (withdraw :cancelled)))
                (a:when-let ((caller (getf request :cancel)))
                  (on-cancel caller (lambda () (cancel token))))))))))
  nil)

;;; --- the exchange -------------------------------------------------------

;;; Shared by every protocol that talks to a backend over a socket: the
;;; deadline-bounded exchange, and the reply and transport shapes a
;;; JSON-over-HTTP protocol needs regardless of wire dialect.
;;;
;;; The connection is opened here rather than left to drakma, so the deadline
;;; has a socket of its own to close: drakma's :connection-timeout only bounds
;;; connecting, not the whole exchange.

(defun header-alist (headers)
  "HEADERS, a coerced plist of names and values, as a lower-cased alist."
  (loop for (name value) on headers by #'cddr
        collect (cons (string-downcase name) value)))

(defstruct (exchange (:conc-name exchange-))
  socket thread reason finished (lock (bt:make-lock)))

(defvar *exchange* nil
  "The exchange this thread is running, so an interrupt meant for one that has
finished does nothing.")

(defun call-with-deadline (timeout-ms function &key cancel)
  "Run FUNCTION on this thread, bounded by TIMEOUT-MS. FUNCTION takes CONNECT,
a function of a URL answering a stream ready for drakma's :STREAM. Answers
(values result reason), REASON being :TIMEOUT or, when CANCEL -- a cancel
token -- fired, :CANCELLED. At either the connection is closed and FUNCTION
is unwound, rather than left running until the backend answers or hangs up."
  (when (and cancel (cancelled-p cancel))
    (return-from call-with-deadline (values nil :cancelled)))
  (let ((result nil)
        (exchange (make-exchange :thread (bt:current-thread)))
        (cancel-timer nil))
    (catch exchange
      (let ((*exchange* exchange))
        (setf cancel-timer (m:schedule (/ timeout-ms 1000)
                                       (lambda () (abandon-exchange exchange :timeout))))
        (when cancel
          (on-cancel cancel (lambda () (abandon-exchange exchange :cancelled))))
        (unwind-protect
             (setf result (funcall function (lambda (url)
                                              (open-connection url exchange timeout-ms))))
          (without-interrupts
            (close-socket (exchange-socket exchange))))))
    (when cancel-timer (funcall cancel-timer))
    (let ((reason (finish-exchange exchange)))
      (if reason (values nil reason) (values result nil)))))

(defun finish-exchange (exchange)
  "End EXCHANGE, so a cancel or timeout arriving now does nothing. Answers the
reason one already claimed it for, if any."
  (bt:with-lock-held ((exchange-lock exchange))
    (setf (exchange-finished exchange) t)
    (exchange-reason exchange)))

(defun close-socket (socket)
  "Shut SOCKET down before closing it: on Linux a close alone leaves a thread
blocked in a read on it asleep, and a shutdown wakes it."
  (when socket
    (ignore-errors (usocket:socket-shutdown socket :io))
    (ignore-errors (usocket:socket-close socket))))

(defun abandon-exchange (exchange reason)
  "End EXCHANGE's run for REASON, :TIMEOUT or :CANCELLED, unless something
already has: shut its socket down, which wakes a read on it, and interrupt
its thread out of FUNCTION, since on Linux a close alone leaves a thread
blocked in a read on it asleep. Cheap, and safe to call from any thread."
  (bt:with-lock-held ((exchange-lock exchange))
    (unless (or (exchange-finished exchange) (exchange-reason exchange))
      (setf (exchange-reason exchange) reason)
      (a:when-let ((socket (exchange-socket exchange)))
        (ignore-errors (usocket:socket-shutdown socket :io)))
      (ignore-errors
       (bt:interrupt-thread (exchange-thread exchange)
                            (lambda ()
                              (when (eq *exchange* exchange)
                                (throw exchange nil))))))))

(defun open-connection (url exchange timeout-ms)
  (let* ((uri (puri:parse-uri url))
         (securep (eq (puri:uri-scheme uri) :https))
         (socket (with-immediate-connect-refusal
                   (usocket:socket-connect
                    (puri:uri-host uri) (or (puri:uri-port uri) (if securep 443 80))
                    :element-type '(unsigned-byte 8)
                    ;; Bounds the connect phase alone, ahead of the whole-
                    ;; exchange deadline. TODO: usocket rejects :timeout on
                    ;; ECL, where only the whole-exchange deadline bounds a
                    ;; connect (#229).
                    #-ecl :timeout #-ecl (max 1 (ceiling timeout-ms 1000))
                    :nodelay :if-supported))))
    ;; A cancel that landed while connecting found no socket to close.
    (when (bt:with-lock-held ((exchange-lock exchange))
            (setf (exchange-socket exchange) socket)
            (exchange-reason exchange))
      (close-socket socket)
      (error "cancelled"))
    (wrap-http-stream socket (puri:uri-host uri) securep)))

(defun wrap-http-stream (socket host securep)
  "SOCKET's stream, wrapped exactly as drakma wraps one it opens itself:
chunked framing under a flexi-stream, with SSL attached first when
SECUREP. Passing :stream skips drakma's own wrapping entirely -- it only
adjusts the flexi-stream's element-type and external-format -- so a stream
given raw fails outright, and one without SSL attached sends a TLS
handshake in the clear."
  (let ((raw (usocket:socket-stream socket)))
    (flexi-streams:make-flexi-stream
     (chunga:make-chunked-stream
      (if securep
          (cl+ssl:make-ssl-client-stream raw :hostname host)
          raw))
     ;; Matches drakma's own +LATIN-1+ (specials.lisp), which is internal.
     :external-format (flexi-streams:make-external-format :latin-1 :eol-style :lf))))

;;; --- emitters -----------------------------------------------------------

;;; A function sink is called through a queue drained by a job on the sink
;;; pool, so a sink that blocks never holds up whoever emits, and every event
;;; reaches it one at a time, in order. A drain holds a pooled thread only
;;; while events wait, and an emitter that stalls is thrown out of its job.

(defparameter *sink-grace* 5
  "Seconds an emitter has to deliver what it was sent once stopped.")

(defvar *emitting* nil
  "The emitter this thread is draining, so an interrupt meant for one that has
finished does nothing.")

(defun start-emitter (sink)
  "An emitter calling SINK, or nil when SINK is not a function."
  (when (and sink (typep sink '(or function symbol)))
    (%make-emitter sink)))

(defun finish-emitter (emitter)
  "Called holding EMITTER's lock."
  (unless (emitter-finished emitter)
    (setf (emitter-finished emitter) t)
    (bt:signal-semaphore (emitter-drained emitter))))

(defun drain-emitter (emitter)
  (catch emitter
    (let ((*emitting* emitter))
      (bt:with-lock-held ((emitter-lock emitter))
        (setf (emitter-thread emitter) (bt:current-thread)))
      (loop
        (let ((event (bt:with-lock-held ((emitter-lock emitter))
                       (if (emitter-queue emitter)
                           (list (pop (emitter-queue emitter)))
                           (progn
                             (setf (emitter-scheduled emitter) nil
                                   (emitter-job emitter) nil
                                   (emitter-thread emitter) nil)
                             (when (emitter-stopping emitter)
                               (finish-emitter emitter))
                             nil)))))
          (unless event (return))
          (ignore-errors (funcall (emitter-sink emitter) (first event))))))))

(defun emitter-send (emitter event)
  (bt:with-lock-held ((emitter-lock emitter))
    (unless (or (emitter-dead emitter) (emitter-stopping emitter))
      (setf (emitter-queue emitter) (nconc (emitter-queue emitter) (list event)))
      (unless (emitter-scheduled emitter)
        (setf (emitter-scheduled emitter) t
              (emitter-job emitter) (make-pool-job (lambda () (drain-emitter emitter))))
        (pool-submit (pool-for :sink) (emitter-job emitter))))))

(defun emit-done-detached (sink ref result)
  "End a turn that never ran with its one :DONE, without waiting on SINK."
  (a:if-let ((emitter (start-emitter sink)))
    (progn (emitter-send emitter (done ref (done-reason result)))
           (stop-emitter emitter)
           (reap-emitter emitter *sink-grace*))
    (emit-event sink (done ref (done-reason result)))))

(defun stop-emitter (emitter)
  "Have EMITTER finish once it has delivered everything sent before this."
  (bt:with-lock-held ((emitter-lock emitter))
    (setf (emitter-stopping emitter) t)
    (unless (or (emitter-scheduled emitter) (emitter-queue emitter))
      (finish-emitter emitter))))

(defun await-emitter (emitter seconds)
  "True once EMITTER has finished, waiting at most SECONDS."
  (bt:wait-on-semaphore (emitter-drained emitter) :timeout (max 0 seconds)))

(defun kill-emitter (emitter)
  "Drop what EMITTER has queued and throw its drain out of the sink it is
stuck in, or withdraw it if it has not started."
  (bt:with-lock-held ((emitter-lock emitter))
    (setf (emitter-dead emitter) t
          (emitter-queue emitter) nil)
    (a:when-let ((job (emitter-job emitter)))
      (unless (pool-withdraw job)
        (a:when-let ((thread (emitter-thread emitter)))
          (ignore-errors
           (bt:interrupt-thread thread
                                (lambda ()
                                  (when (eq *emitting* emitter)
                                    (throw emitter nil))))))))
    (finish-emitter emitter)))

(defun reap-emitter (emitter seconds)
  "Kill EMITTER unless it has finished within SECONDS."
  (m:schedule (max 0 seconds)
              (lambda ()
                (unless (await-emitter emitter 0)
                  (kill-emitter emitter)))))

;;; --- streamed turns ---------------------------------------------------

;;; The worker's deltas and the caller's :DONE reach the sink through one
;;; gate, so the sink sees exactly one :DONE and nothing after it, however
;;; the worker and the deadline race. A function sink is called from an
;;; emitter, so a sink that blocks never holds up the worker or the deadline.
;;; An emitter that has not delivered :DONE by the turn's deadline plus
;;; *SINK-GRACE* is killed, and the events queued behind it with it.

(defstruct (sink-gate (:conc-name gate-))
  sink (lock (bt:make-lock)) closed emitter)

(defun gate-deliver (gate event)
  (if (gate-emitter gate)
      (emitter-send (gate-emitter gate) event)
      (emit-event (gate-sink gate) event)))

(defun gate-emitter-function (gate)
  (lambda (event)
    (bt:with-lock-held ((gate-lock gate))
      (unless (gate-closed gate)
        (gate-deliver gate event)))))

(defun close-gate (gate ref result &optional wait)
  "End the turn: emit its :DONE, then drop whatever the worker still sends.
WAIT, in seconds, bounds how long to wait for the sink to have seen it. True
when the sink has, or has no emitter to wait for."
  (bt:with-lock-held ((gate-lock gate))
    (unless (gate-closed gate)
      (setf (gate-closed gate) t)
      (gate-deliver gate (done ref (done-reason result)))
      (a:when-let ((emitter (gate-emitter gate)))
        (stop-emitter emitter))))
  (or (null (gate-emitter gate))
      (and wait (await-emitter (gate-emitter gate) wait))))

(defun done-reason (result)
  (if (tool-error-p result)
      result
      (getf (getf (second result) :meta) :finish-reason)))

(defun perform-completion (request opener reader)
  "Run the exchange under the caller's deadline, as TOOL-HTTP does: a wedged
backend costs a timeout, not a wedged service. OPENER
takes (request connect) and answers (values stream status headers), HEADERS
being drakma's response alist and optional; READER takes
(request stream status) and answers the reply. A request with a :STREAM sink
ends it with exactly one :DONE."
  (let* ((sink (getf request :stream))
         (gate (make-sink-gate :sink sink :emitter (start-emitter sink)))
         (timeout (getf request :timeout +default-tool-timeout+))
         (started (get-internal-real-time))
         (request (if sink
                      (list* :stream (gate-emitter-function gate) request)
                      request)))
    (multiple-value-bind (result reason)
        (call-with-deadline
         timeout
         (lambda (connect) (attempt-completion request opener reader connect))
         :cancel (getf request :cancel))
      (let ((result (case reason
                      (:timeout (fail :timeout))
                      (:cancelled (fail :cancelled))
                      (t result))))
        (when sink
          (flet ((remaining ()
                   (- (/ timeout 1000)
                      (/ (- (get-internal-real-time) started)
                         internal-time-units-per-second))))
            (unless (close-gate gate (getf request :ref) result
                                (unless reason (remaining)))
              (reap-emitter (gate-emitter gate) (+ (remaining) *sink-grace*)))))
        result))))

(defun attempt-completion (request opener reader connect)
  ;; The two failure regions are kept apart: nothing read yet is a transport
  ;; failure, and everything after the status is the backend misbehaving.
  (let (stream status headers)
    (handler-case
        (multiple-value-setq (stream status headers) (funcall opener request connect))
      ;; Nothing was read, so there is no backend answer to report on: a
      ;; refused connection and a peer that hangs up before the status line
      ;; are the same failure to the caller.
      (error () (return-from attempt-completion (fail :unavailable))))
    (unwind-protect
         (handler-case
             (if (<= 200 status 299)
                 (funcall reader request stream status)
                 (backend-error status (read-detail stream)
                                :retry-after (retry-after-ms headers)))
           (error (e) (backend-error status (princ-to-string e))))
      (ignore-errors (close stream)))))

(defun character-stream (stream)
  "STREAM as UTF-8 characters. Drakma leaves the stream it was given in the
external format of the response's content type, which is Latin-1 unless the
type names a charset."
  (if (typep stream 'flexi-streams:flexi-stream)
      (progn
        (setf (flexi-streams:flexi-stream-external-format stream)
              (flexi-streams:make-external-format :utf-8 :eol-style :lf)
              (flexi-streams:flexi-stream-element-type stream) 'character)
        stream)
      (flexi-streams:make-flexi-stream stream :external-format :utf-8)))

(defun read-detail (stream)
  "An error response's body, for the detail of a (:backend-error ...)."
  (or (ignore-errors (uiop:slurp-stream-string stream)) ""))

(defun make-reply (text calls reason meta)
  (list :ok (list :role :assistant
                  :content (when (plusp (length text)) (normalize-content text))
                  :tool-calls calls
                  :done t
                  :meta (list* :finish-reason reason meta))))

(defun reply-prompt-tokens (reply)
  "The prompt's size in tokens as the backend counted it, from REPLY's :META
:USAGE :PROMPT-TOKENS, or nil when it reported none."
  (let ((tokens (getf (getf (getf reply :meta) :usage) :prompt-tokens)))
    (and (integerp tokens) (plusp tokens) tokens)))

(defun finish-reason (value)
  "A wire finish/done reason as a keyword: tool_calls is :TOOL-CALLS."
  (when (stringp value) (lisp-key value)))

(defun text-of (value)
  (if (stringp value) value ""))

;;; --- the handler ------------------------------------------------------

(defmacro define-protocol-handler (class (service request) &body body)
  "Define HANDLE for CLASS: (:describe) answers METADATA and
(:complete . plist) runs BODY as a pool job, DEFER-COMPLETION, with
REQUEST bound to the plist, checked against the contract. CLASS must inherit
COMPLETION-HOST. Meow intercepts %update-config, %effects and %timer-fire before
HANDLE, so a protocol must not use those heads."
  (a:with-gensyms (problem)
    `(defmethod m:handle ((,service ,class) message)
       (case (first message)
         (:describe (m:metadata ,service))
         ;; COMPLETE checks too; doing it here as well means a protocol
         ;; reached by a bare M:CALL sees the same checked request.
         (:complete (let* ((,request (rest message))
                           (,problem (check-request ,request)))
                      (declare (ignorable ,request))
                      (if ,problem
                          (bad-request "~a" ,problem)
                          (defer-completion ,service ,request
                            (lambda (,request)
                              (declare (ignorable ,request))
                              ,@body)))))
         (t (bad-request "unknown message ~s" (first message)))))))

(defmacro define-protocol (name (&key summary params) (service request) &body body)
  "Define the protocol NAME, a keyword such as :PROTOCOL-OPENAI: a service
class over COMPLETION-HOST, its METADATA and its :COMPLETE handler
(DEFINE-PROTOCOL-HANDLER), registered in the definitions table. SUMMARY and
PARAMS are forms, evaluated for each :describe."
  (let ((class (intern (symbol-name name))))
    `(progn
       (m:defservice ,class (completion-host) ()
         (:name ,name))
       (register-definition ,name :protocol ',class)
       (defmethod m:metadata ((,service ,class))
         (list :kind :protocol :name ,name :summary ,summary :params ,params))
       (define-protocol-handler ,class (,service ,request) ,@body))))
