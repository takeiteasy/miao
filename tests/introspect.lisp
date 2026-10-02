(in-package #:miao/tests)
(in-suite :miao)

;;; TOOL-IMAGE and TOOL-SERVICES (~takeiteasy/miao#10). Both mount already,
;;; via WITH-TOOLS (tests/tools.lisp).

;;; --- tool-image ----------------------------------------------------

(test image-describe-a-known-function
  (with-tools
    (let ((result (tool :tool-image :op :describe :symbol "complete" :package "miao")))
      (is (eq :ok (first result)))
      (is (eq :function (result-value result :kind)))
      (is (eq t (result-value result :fboundp)))
      (is (stringp (result-value result :lambda-list)))
      (is (stringp (result-value result :documentation))))))

(defvar *introspect-secret* "sk-secret"
  "A bound special IMAGE-NEVER-RETURNS-A-BOUND-VALUES-VALUE checks never
travels in a TOOL-IMAGE reply.")

(test image-never-returns-a-bound-values-value
  ;; The invariant tests/provider.lisp guards for PROVIDER.LISP's :api-key:
  ;; a bound special's value must never travel, whatever op is asked.
  (with-tools
    (let ((result (tool :tool-image :op :describe :symbol "*introspect-secret*"
                                    :package "miao/tests")))
      (is (eq t (result-value result :boundp)))
      (is (null (search "sk-secret" (format nil "~s" result)))))))

(test image-describe-an-unknown-symbol-is-a-bad-request
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error
                       (tool :tool-image :op :describe
                                        :symbol "totally-unknown-symbol-xyz"
                                        :package "miao")))))))

(test image-describe-requires-symbol
  (with-tools
    (is (equal :bad-request (first (miao:tool-error (tool :tool-image :op :describe)))))))

(test image-apropos-finds-and-caps-results
  (with-tools
    (let ((result (tool :tool-image :op :apropos :pattern "TOOL" :package "miao"
                                    :external-only nil :limit 2)))
      (is (eq :ok (first result)))
      (is (<= (length (result-value result :symbols)) 2))
      (is (integerp (result-value result :total)))
      (when (> (result-value result :total) 2)
        (is (eq t (result-value result :truncated)))))))

(test image-apropos-requires-pattern
  (with-tools
    (is (equal :bad-request (first (miao:tool-error (tool :tool-image :op :apropos)))))))

(test image-documentation-reads-a-docstring
  (with-tools
    (is (stringp (result-value (tool :tool-image :op :documentation
                                                 :symbol "complete" :package "miao")
                               :documentation)))))

(test image-source-locates-a-loaded-function
  (with-tools
    (let ((result (tool :tool-image :op :source :symbol "complete" :package "miao")))
      (is (eq :ok (first result)))
      (is (eq t (result-value result :available)))
      (is (stringp (result-value result :file))))))

(defun define-in-image (name body)
  (handler-bind ((warning #'muffle-warning))
    (eval `(defun ,name (x) ,body))))

(test image-source-shows-the-form-of-an-in-image-definition
  (with-tools
    (define-in-image 'miao/tests::%introspect-repl-fn '(1+ x))
    (let ((result (tool :tool-image :op :source :symbol "%introspect-repl-fn"
                                    :package "miao/tests")))
      (is (eq t (result-value result :available)))
      (is (search "1+" (result-value result :form)))
      (is (eq nil (result-value result :truncated)))
      (is (null (result-value result :file))))))

(test image-source-caps-a-long-form
  (with-tools
    (define-in-image 'miao/tests::%introspect-long-fn
      `(quote ,(loop for i below 2000 collect i)))
    (let ((result (tool :tool-image :op :source :symbol "%introspect-long-fn"
                                    :package "miao/tests")))
      (is (eq t (result-value result :truncated)))
      (is (= 4000 (length (result-value result :form)))))))

(test image-describe-carries-the-form
  (with-tools
    (define-in-image 'miao/tests::%introspect-describe-fn '(1+ x))
    (let ((result (tool :tool-image :op :describe :symbol "%introspect-describe-fn"
                                    :package "miao/tests")))
      (is (stringp (getf (result-value result :source) :form))))))

(defgeneric %introspect-generic (x))
(defmethod %introspect-generic ((x integer)) x)
(defmethod %introspect-generic ((x string)) x)

(test image-source-lists-a-generic-functions-methods
  (with-tools
    (let ((result (tool :tool-image :op :source :symbol "%introspect-generic"
                                    :package "miao/tests")))
      (is (eq t (result-value result :available)))
      (is (= 2 (result-value result :methods-total)))
      (is (eq nil (result-value result :methods-truncated)))
      (is (equal '("(INTEGER)" "(STRING)")
                 (sort (mapcar (lambda (m) (getf m :specializers))
                               (result-value result :methods))
                       #'string<))))))

(test image-describe-carries-a-plain-function-without-methods
  (with-tools
    (let ((result (tool :tool-image :op :describe :symbol "complete" :package "miao")))
      (is (eq t (getf (result-value result :source) :available)))
      (is (null (getf (result-value result :source) :methods))))))

(test image-packages-lists-the-loaded-image
  (with-tools
    (is (member "MIAO" (mapcar (lambda (p) (getf p :name))
                               (result-value (tool :tool-image :op :packages) :packages))
                :test #'string=))))

;;; --- tool-services ---------------------------------------------------

(test services-registry-lists-mounted-tools
  (with-tools
    (let ((names (mapcar (lambda (e) (getf e :name))
                         (result-value (tool :tool-services :op :registry) :entries))))
      (is (member :tool-fs names))
      (is (member :tool-image names))
      (is (member :tool-services names)))))

(test services-registry-filters-by-kind
  (with-tools
    (let ((entries (result-value (tool :tool-services :op :registry :kind :tool) :entries)))
      (is (every (lambda (e) (eq :tool (getf (getf e :props) :kind))) entries)))))

(test services-children-reports-running-tools
  (with-tools
    (let ((children (result-value (tool :tool-services :op :children) :children)))
      (is (find :tool-image children :key (lambda (c) (getf c :name))))
      (is (eq :running
              (getf (find :tool-image children :key (lambda (c) (getf c :name))) :state)))
      (is (every (lambda (c) (eq :ready (getf c :status))) children)))))

(m:defservice stalled-child () ())

(defmethod m:handle ((child stalled-child) message)
  (declare (ignore child message))
  (sleep 2.5))

(test services-children-asks-every-status-at-once
  (with-tools
    (dolist (name '(:stalled-a :stalled-b))
      (m:mount *context* 'stalled-child :name name)
      (m:cast (m:lookup name) :stall))
    (sleep 0.1)
    (let* ((began (get-internal-real-time))
           (children (result-value (tool :tool-services :op :children) :children))
           (elapsed (/ (- (get-internal-real-time) began) internal-time-units-per-second)))
      (dolist (name '(:stalled-a :stalled-b))
        (let ((child (find name children :key (lambda (c) (getf c :name)))))
          (is (getf child :alive))
          (is (null (getf child :status)))))
      (is (eq :ready (getf (find :tool-fs children :key (lambda (c) (getf c :name))) :status)))
      (is (< elapsed 1.8) "two stalled children waited at once, took ~as" elapsed))))

(test services-describe-an-unregistered-name-is-a-bad-request
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error
                       (tool :tool-services :op :describe :name "does-not-exist")))))))

(test services-describe-requires-name
  (with-tools
    (is (equal :bad-request (first (miao:tool-error (tool :tool-services :op :describe)))))))

(test services-describe-reports-props-and-effects
  (with-tools
    (let ((result (tool :tool-services :op :describe :name "tool-fs")))
      (is (eq :ok (first result)))
      (is (eq :tool (getf (result-value result :props) :kind)))
      (is (eq t (result-value result :alive)))
      (is (listp (result-value result :effects))))))

(test services-describe-reports-lifecycle-status
  (with-tools
    (dolist (name '("tool-fs" "tool-services"))
      (is (eq :ready (result-value (tool :tool-services :op :describe :name name) :status))))))
