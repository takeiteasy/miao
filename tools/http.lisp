(in-package #:miao)

;;; Single-shot HTTP client. One request in, one response out, bounded by a
;;; caller deadline. Redirects are not followed and statuses pass through
;;; untouched: callers decide what a 3xx or a 404 means for them. A text body
;;; comes back as a string and any other as base64, named by :BODY-ENCODING.
;;;
;;; The exchange runs under CALL-WITH-DEADLINE, which owns the connection so
;;; the deadline or a cancel can close it.
;;;
;;; Trust posture: arbitrary network egress. Trusted operator only.

(define-tool :tool-http
    (:trust :operator
     :summary "Perform a single-shot HTTP request"
     :params ((:url string :required t :doc "target URL, http or https")
              (:method (member :get :post :put :patch :delete :head :options)
               :default :get :doc "HTTP verb")
              (:headers (map-of string) :doc "extra request headers")
              (:body string :doc "request payload")
              (:timeout (integer 1) :default +default-tool-timeout+
               :doc "whole-exchange deadline in milliseconds")))
  (:invoke (url method headers body timeout)
    (perform-request url method (header-alist headers) body timeout cancel-token)))

(defun perform-request (url method headers body timeout-ms &optional cancel)
  (multiple-value-bind (result reason)
      (call-with-deadline timeout-ms
                          (lambda (connect)
                            (attempt-request url method headers body connect))
                          :cancel cancel)
    (if reason (fail reason) result)))

(defun attempt-request (url method headers body connect)
  (handler-case
      ;; Drakma returns 4xx and 5xx as values rather than signalling, which
      ;; is what lets statuses pass through with no translation layer.
      (multiple-value-bind (payload status response-headers)
          (apply #'drakma:http-request
                 url
                 :method method
                 :stream (funcall connect url)
                 :close t
                 :redirect nil
                 :force-binary t
                 :external-format-out :utf-8
                 ;; Content-Type is drakma's own argument. Leaving it in
                 ;; ADDITIONAL-HEADERS too would send it twice; dropping it
                 ;; without folding it in would silently override what the
                 ;; caller asked for.
                 :additional-headers (remove "content-type" headers
                                             :key #'car :test #'string=)
                 (when body
                   (list :content body
                         :content-type (or (cdr (assoc "content-type" headers
                                                       :test #'string=))
                                           "application/octet-stream"))))
        (multiple-value-bind (body encoding)
            (decode-body payload (cdr (assoc :content-type response-headers)))
          (ok :status status
              :headers (loop for (name . value) in response-headers
                             collect (string-downcase (string name))
                             collect value)
              :body body
              :body-encoding encoding)))
    (usocket:socket-error () (fail :unavailable))
    (error (e) (fail (list :error (princ-to-string e))))))

(defun declared-charset (content-type)
  "The external format CONTENT-TYPE's charset parameter names, or nil when it
names none or one flexi-streams does not know."
  (a:when-let* ((start (and content-type (search "charset=" content-type :test #'char-equal)))
                (name (string-trim " \"'" (subseq content-type (+ start 8)
                                                  (position #\; content-type :start start)))))
    (find-symbol (string-upcase name) :keyword)))

(defun text-media-type-p (content-type)
  "Whether CONTENT-TYPE's media type, ignoring its parameters, is text: text/*,
form data, or JSON, XML or JavaScript in any spelling such as +json."
  (let ((type (string-downcase
               (string-trim " " (subseq content-type 0 (position #\; content-type))))))
    (or (a:starts-with-subseq "text/" type)
        (string= type "application/x-www-form-urlencoded")
        (some (lambda (word) (search word type)) '("json" "xml" "javascript")))))

(defun decode-body (octets content-type)
  "OCTETS as (values body encoding). A text CONTENT-TYPE is decoded to a
string in its declared charset, else UTF-8, else Latin-1, with encoding
\"text\". No CONTENT-TYPE is text only when the bytes are valid UTF-8. Anything
else is base64, encoding \"base64\". Drakma is asked for octets because it
would otherwise decode a text type with no charset as Latin-1."
  (flet ((decode (format)
           (ignore-errors (flexi-streams:octets-to-string octets :external-format format)))
         (base64 ()
           (values (cl-base64:usb8-array-to-base64-string octets) "base64")))
    (cond ((zerop (length octets)) (values "" "text"))
          ((null content-type)
           (a:if-let ((text (decode :utf-8)))
             (values text "text")
             (base64)))
          ((text-media-type-p content-type)
           (values (or (a:when-let ((format (declared-charset content-type)))
                         (decode format))
                       (decode :utf-8)
                       (decode :latin-1))
                   "text"))
          (t (base64)))))
