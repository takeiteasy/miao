(in-package #:miao)

;;; The provider convention. A provider is data -- a protocol to speak, a base
;;; URL, how to authenticate, a model catalogue and any quirks -- and
;;; DEFINE-PROVIDER turns that declaration into a meow service named
;;; :PROVIDER-<name> whose METADATA carries :KIND :PROVIDER. It answers
;;; (:describe) and (:complete . plist), layering its data under the request
;;; and delegating to its protocol. See docs/providers.md.

(defparameter +auth-kinds+ '(:none :bearer :header)
  "The closed set of authentication kinds.")

(defun providers (&key (registry m:*registry*))
  "Every registered provider name, sorted."
  (%registered-of-kind :provider :registry registry))

(defun describe-provider (name &key (registry m:*registry*))
  "NAME's metadata plist."
  (m:call (%protocol-process name :registry registry) '(:describe)))

;;; --- the declaration --------------------------------------------------

(defclass provider (completion-host)
  ((base-url :initarg :base-url :reader provider-base-url :initform nil)
   (model :initarg :model :reader provider-model :initform nil)
   (api-key :initarg :api-key :reader provider-api-key :initform nil))
  (:documentation "The mount-time half of a provider: the fields a mount may
override. The declared half is PROVIDER-DECLARATION."))

(defmethod secret-initargs ((service provider)) '(:api-key))

(defgeneric provider-declaration (service)
  (:documentation "SERVICE's checked declaration plist, as DEFINE-PROVIDER
wrote it."))

(defun provider-protocol (service)
  (getf (provider-declaration service) :protocol))

(defun provider-auth (service)
  (getf (provider-declaration service) :auth))

(defun provider-defaults (service)
  (getf (provider-declaration service) :defaults))

(defun check-provider-declaration (name declaration)
  "DECLARATION with its :AUTH canonicalised, or an error. Checked at load
time, so a malformed provider never reaches a mount."
  (flet ((problem (format &rest args)
           (error "Provider ~s: ~a" name (apply #'format nil format args))))
    (let ((protocol (getf declaration :protocol))
          (base-url (getf declaration :base-url)))
      (unless (keywordp protocol)
        (problem ":protocol is required, naming a protocol service"))
      (unless (named-p base-url)
        (problem ":base-url is required, naming the backend"))
      (unless (http-url-p base-url)
        (problem ":base-url must be an http or https URL"))
      (dolist (key '(:defaults :headers))
        (let ((value (getf declaration key)))
          (unless (and (listp value) (evenp (length value)))
            (problem "~(~a~) must be a plist" key))))
      (unless (listp (getf declaration :models))
        (problem ":models must be a list of model ids"))
      (a:when-let ((extra (set-difference (loop for (key) on declaration by #'cddr
                                                collect key)
                                          '(:protocol :base-url :auth :models
                                            :defaults :headers :summary
                                            :rewrite-request :rewrite-response))))
        (problem "unknown key~p ~{~(~s~)~^, ~}" (length extra) extra))
      (list* :auth (canonical-auth #'problem (or (getf declaration :auth) :none))
             (a:remove-from-plist declaration :auth)))))

(defun canonical-auth (problem auth)
  "AUTH as (:kind k . details), so one GETF reads every kind."
  (let ((kind (if (consp auth) (first auth) auth)))
    (unless (member kind +auth-kinds+)
      (funcall problem ":auth must be one of ~{~(~s~)~^, ~}, got ~s"
               +auth-kinds+ auth))
    (let* ((details (if (eq kind :header) (cddr auth) (and (consp auth) (rest auth))))
           (env (getf details :env))
           (header (and (eq kind :header) (second auth))))
      (when (and (not (eq kind :none)) (not (named-p env)))
        (funcall problem "~(~s~) auth needs :env, naming the variable the key \
comes from" kind))
      (when (and (eq kind :header) (not (named-p header)))
        (funcall problem ":header auth needs a header name"))
      (case kind
        (:none '(:kind :none))
        (:bearer (list :kind :bearer :env env))
        (:header (list :kind :header :name header :env env))))))

;;; --- credentials ------------------------------------------------------

;;; BYOK: a key comes from the environment variable the declaration names, or
;;; from an :api-key mount option. It is never published in metadata and never
;;; read from the user config file, which is startup code rather than a secret
;;; store.

(defun provider-key (service)
  "SERVICE's API key, or NIL when it has none or needs none."
  (let ((auth (provider-auth service)))
    (unless (eq (getf auth :kind) :none)
      (let ((key (or (provider-api-key service) (uiop:getenv (getf auth :env)))))
        (when (named-p key) key)))))

(defun provider-key-problem (service)
  "NIL when SERVICE can authenticate, else a problem string. A keyed provider
with no key still mounts, so discovery lists it and the failure is legible."
  (let ((auth (provider-auth service)))
    (unless (or (eq (getf auth :kind) :none) (provider-key service))
      (format nil "no API key; set ~a or mount with :api-key" (getf auth :env)))))

(defun auth-headers (service)
  "SERVICE's credential as a header plist."
  (let ((auth (provider-auth service))
        (key (provider-key service)))
    (case (getf auth :kind)
      (:bearer (list "authorization" (format nil "Bearer ~a" key)))
      (:header (list (getf auth :name) key)))))

;;; --- layering ---------------------------------------------------------

(defun merge-headers (weak strong)
  "WEAK and STRONG as one header plist. A name STRONG carries wins, compared
without case as HTTP does."
  (append (loop for (name value) on weak by #'cddr
                unless (getf-ci strong name)
                  collect name and collect value)
          strong))

(defun getf-ci (plist name)
  (loop for (key value) on plist by #'cddr
        when (string-equal key name) return value))

(defun layered-request (service request)
  "REQUEST with SERVICE's data under it. A plist reads by its first match, so
appending the provider's keys after the caller's is what lets the caller win."
  (let ((headers (merge-headers (merge-headers
                                 (getf (provider-declaration service) :headers)
                                 (auth-headers service))
                                (getf request :headers))))
    (append (when headers (list :headers headers))
            (a:remove-from-plist request :headers)
            (list :base-url (provider-base-url service))
            (when (provider-model service)
              (list :model (provider-model service)))
            (provider-defaults service))))

;;; --- the exchange -----------------------------------------------------

(defmethod m:metadata ((service provider))
  (let ((declaration (provider-declaration service)))
    (list :kind :provider
          :name (m:service-name service)
          :protocol (provider-protocol service)
          :summary (getf declaration :summary)
          :base-url (provider-base-url service)
          :models (getf declaration :models)
          :model (provider-model service)
          ;; The canonical auth form: a kind, a header name and the variable
          ;; the key comes from. Never the key itself.
          :auth (provider-auth service)
          :defaults (provider-defaults service)
          :status (if (provider-key-problem service) :unavailable :ready))))

(defmethod m:handle ((service provider) message)
  (case (first message)
    (:describe (m:metadata service))
    (:complete (let* ((request (rest message))
                      (problem (check-request request)))
                 (if problem
                     (bad-request "~a" problem)
                     (provider-complete service request))))
    (:snapshot (snapshot service))
    (:restore (restore service (second message)))
    (t (bad-request "unknown message ~s" (first message)))))

(defun provider-complete (service request)
  "Layer SERVICE's data under REQUEST and hand the call to its protocol. With
no :REWRITE-RESPONSE and no :MAX-IN-FLIGHT the call is forwarded whole, and
the protocol answers the caller; otherwise a pool job waits on it."
  (a:if-let ((problem (provider-key-problem service)))
    (bad-request "~a" problem)
    (multiple-value-bind (process props)
        (m:lookup (provider-protocol service) :registry (m:service-registry service))
      (cond
        ((null process) (fail :unavailable))
        ((not (member (getf props :kind) '(:protocol :provider)))
         (bad-request "~(~s~) is not a protocol" (provider-protocol service)))
        (t
         (let ((layered (apply-quirk service :rewrite-request
                                     (layered-request service request))))
           (if (or (getf (provider-declaration service) :rewrite-response)
                   (host-max-in-flight service))
               (defer-completion
                service layered
                ;; Carries the job's own :cancel and the :timeout left to it.
                (lambda (layered)
                  (apply-quirk
                   service :rewrite-response
                   (multiple-value-call #'%call-result
                     (m:call process (list* :complete (nested-request layered))
                             :timeout (%caller-timeout layered))))))
               (m:forward process (list* :complete layered)))))))))

(defun apply-quirk (service hook value)
  "VALUE through SERVICE's HOOK, a quirk the declaration names, or unchanged."
  (a:if-let ((function (getf (provider-declaration service) hook)))
    (funcall function value)
    value))

;;; --- the macro --------------------------------------------------------

(defun provider-class-name (name)
  (a:symbolicate '#:provider- (symbol-name name)))

(defmacro define-provider (name &rest declaration
                           &key protocol base-url auth models defaults headers
                                summary rewrite-request rewrite-response)
  "Define provider NAME, a keyword, as the service PROVIDER-<name> registered
under :PROVIDER-<name>. Every value is a form, evaluated at load time; NAME and
:PROTOCOL must be literal keywords, since the class and its dependency are
named from them. :BASE-URL, :MODEL and :API-KEY are mount initargs overriding
the declaration."
  (declare (ignore base-url auth models defaults headers summary
                   rewrite-request rewrite-response))
  (unless (and (keywordp name) (keywordp protocol))
    (error "DEFINE-PROVIDER: NAME and :protocol must be literal keywords."))
  (let ((class (provider-class-name name)))
    `(progn
       (m:defservice ,class (provider) ()
         (:name ,(a:make-keyword class))
         (:depends-on ,protocol)
         (:default-initargs :base-url ,(getf declaration :base-url)))
       (register-definition ,(a:make-keyword class) :provider ',class '(,protocol))
       (defmethod provider-declaration ((service ,class))
         (load-time-value
          (check-provider-declaration
           ,(a:make-keyword class)
           (list ,@(loop for (key form) on declaration by #'cddr
                         collect key collect form)))
          t))
       ',class)))
