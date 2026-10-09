(in-package #:miao/tests)
(in-suite :miao)

;;; DEFINE-TOOL, DEFINE-PROTOCOL and DEFINE-PROVIDER fill a table
;;; ENSURE-MOUNTED reads, so a service mounts by name.

(defmacro with-fresh-context ((context) &body body)
  `(let* ((registry (make-instance 'm:registry))
          (m:*registry* registry)
          (,context (m:start-service (make-instance 'm:context :name :definitions)
                                     :registry registry)))
     (unwind-protect (progn ,@body)
       (m:stop ,context))))

(test each-macro-records-its-definition
  (is (member :tool-shell (miao:definitions :kind :tool)))
  (is (member :protocol-ollama (miao:definitions :kind :protocol)))
  (is (member :protocol-openai (miao:definitions :kind :protocol)))
  (is (member :ollama (miao:definitions :kind :provider)))
  (is (not (member :tool-shell (miao:definitions :kind :provider)))))

(test ensure-mounted-mounts-a-client-backend-by-name
  (with-fresh-context (context)
    (miao:ensure-mounted context :ollama :model "llama3.2")
    (is (equal "llama3.2" (getf (miao:describe-provider :ollama) :model)))))

(test ensure-mounted-twice-mounts-once
  (with-fresh-context (context)
    (miao:ensure-mounted context :tool-shell)
    (let ((process (m:lookup :tool-shell)))
      (miao:ensure-mounted context :tool-shell)
      (is (eq process (m:lookup :tool-shell))))))

(test ensure-mounted-refuses-an-undefined-name
  (with-fresh-context (context)
    (signals error (miao:ensure-mounted context :tool-nobody-defined))))
