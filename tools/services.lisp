(in-package #:miao)

;;; Read-only introspection over the meow supervision tree this tool is
;;; itself mounted under: the registry, the mount tree and a service's
;;; published state. See ~takeiteasy/miao#10.
;;;
;;; Every answer is built from what METADATA already publishes and what
;;; M:CHILDREN already reports -- both key-free by construction, so this
;;; tool adds no new way to leak a slot. :STATE is M:CHILDREN's restart
;;; bookkeeping; :STATUS is the service's own lifecycle status, asked of its
;;; process, so it is nil for a child that is restarting, gone or too busy
;;; to answer.

(define-tool :tool-services
    (:trust :agent
     :resumable t
     :summary "Read-only introspection over the meow supervision tree: registry, mount tree, service state"
     :params ((:op (member :registry :children :describe) :required t
               :doc "operation to perform")
              (:kind (member :tool :protocol :provider :agent)
               :doc "filter :registry to one :kind of service")
              (:recursive boolean :default t
               :doc "descend into nested contexts for :children")
              (:name string :required-when (:op :describe) :doc "service name")))
  (:invoke (op kind recursive name)
    (case op
      (:registry (op-registry service kind))
      (:children (op-children service recursive))
      (:describe (op-service-describe service name)))))

;;; M:CHILDREN and M:LOOKUP reach into another process's context; both are
;;; guarded here rather than trusted to return cleanly:
;;;   - %CONTEXT-CALL waits with :TIMEOUT NIL and signals on a non-:OK
;;;     status, so a context in trouble raises rather than hangs this call.
;;;   - a nested call from inside a service's own startup can still be
;;;     mis-settled as a deadlock (~takeiteasy/meow#58).

(defun op-registry (service kind)
  (let ((registry (m:service-registry service)))
    (handler-case
        (let ((names (if kind
                         (%registered-of-kind kind :registry registry)
                         (sort (copy-list (m:names :registry registry)) #'string< :key #'string))))
          (ok :entries (mapcar (lambda (name) (registry-entry name registry)) names)))
      (error (e) (fail (list :error (princ-to-string e)))))))

(defun registry-entry (name registry)
  (multiple-value-bind (process props) (m:lookup name :registry registry)
    (list :name name :alive (and process (m:process-alive-p process) t) :props props)))

(defun op-children (service recursive)
  (let ((context (m:service-context service)))
    (if (null context)
        (fail (list :error "not mounted under a context"))
        (handler-case
            (ok :children (children-tree service (m:service-process context) recursive))
          (error (e) (fail (list :error (princ-to-string e))))))))

(defun children-tree (service context-process recursive)
  (mapcar (lambda (child) (child-entry service child recursive)) (m:children context-process)))

(defconstant +status-timeout+ 1
  "Seconds to wait for a service to report its status.")

(defun process-status (service process)
  "PROCESS's lifecycle status, or nil when it is gone or does not answer.
SERVICE is this tool, which cannot call itself."
  (when (and process (m:process-alive-p process))
    (if (eq process (m:service-process service))
        (m:service-status service)
        (values (m:service-status process :timeout +status-timeout+)))))

(defun child-entry (service child recursive)
  (let ((entry (list :name (getf child :name)
                     :class (string-downcase (symbol-name (getf child :class)))
                     :restart (getf child :restart)
                     :state (getf child :state)
                     ;; TODO: one call per child, so a busy tool costs up to
                     ;; +STATUS-TIMEOUT+ each; fan out with m:call-async if
                     ;; :children latency matters (#149).
                     :status (process-status service (getf child :process))
                     :restart-in (getf child :restart-in)
                     :alive (and (getf child :process) (m:process-alive-p (getf child :process)) t))))
    (if (and recursive (subtypep (getf child :class) 'm:context) (getf child :process))
        (append entry (list :children (children-tree service (getf child :process) recursive)))
        entry)))

(defun op-service-describe (service name)
  (let ((registry (m:service-registry service)))
    (handler-case
        (multiple-value-bind (process props)
            (m:lookup (a:make-keyword (string-upcase name)) :registry registry)
          (if (null process)
              (bad-request "no service named ~a" name)
              (ok :name name :props props :alive (and (m:process-alive-p process) t)
                  :status (process-status service process)
                  :effects (m:effects (if (eq process (m:service-process service))
                                          service
                                          process)))))
      (error (e) (fail (list :error (princ-to-string e)))))))
