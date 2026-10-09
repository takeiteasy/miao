(in-package #:miao)

;;; The table of service definitions DEFINE-TOOL and its kin fill, so a front
;;; end can mount a service by name. A backend the client registers needs no
;;; entry: it mounts as a BACKEND-SERVICE under its own name. See
;;; docs/tools.md#definitions.

(defvar *definitions* (make-hash-table :test 'eq)
  "Service name -> (:kind k :class c :depends-on (names)).")

(defun register-definition (name kind class &optional depends-on)
  (setf (gethash name *definitions*)
        (list :kind kind :class class :depends-on depends-on))
  name)

(defun definitions (&key kind)
  "Every defined service name, sorted, optionally only those of KIND. The
client's protocols and providers count as definitions of their kind."
  (sort (remove-duplicates
         (append (loop for name being the hash-keys of *definitions* using (hash-value entry)
                       when (or (null kind) (eq kind (getf entry :kind)))
                         collect name)
                 (loop for backend-kind in '(:protocol :provider)
                       when (or (null kind) (eq kind backend-kind))
                         append (ci:backends :kind backend-kind))))
        #'string< :key #'string))

(defun ensure-mounted (context name &rest initargs)
  "Mount NAME under CONTEXT, its dependencies first, unless a service is
already registered under it, defined or not. INITARGS apply to NAME only.
Signals for an unregistered name nothing defines."
  (unless (m:lookup name)
    (let ((entry (gethash name *definitions*)))
      (cond
        (entry
         (dolist (dependency (getf entry :depends-on))
           (ensure-mounted context dependency))
         (apply #'m:mount context (getf entry :class) initargs))
        ((ci:find-backend name)
         (apply #'m:mount context 'backend-service :name name initargs))
        (t (error "No definition for ~s." name)))))
  name)
