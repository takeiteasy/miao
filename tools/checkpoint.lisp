(in-package #:miao)

;;; The checkpoint tool: save, list and roll back generations of the
;;; harness's declared state (checkpoint.lisp).
;;;
;;; :trust :operator: writing and reverting the harness's own state is not
;;; something the default :agent trust level should reach. tool-self
;;; takes a checkpoint before every write it makes,
;;; through the CHECKPOINT function this tool also wraps, rather than
;;; through this tool -- so it works whether or not TOOL-CHECKPOINT is
;;; mounted alongside it.

(define-tool :tool-checkpoint
    (:trust :operator
     :summary "Save, list and roll back generations of the harness's declared state"
     :slots ((dir :initarg :dir :initform *generations-directory* :reader checkpoint-dir))
     :params ((:op (member :save :list :restore) :required t
               :doc "operation to perform")
              (:label string :doc "a note describing this generation, for :save")
              (:keep (integer 1) :doc "prune to this many newest generations, for :save")
              (:path string :required-when (:op :restore) :doc "generation path to restore")))
  (:invoke (op label keep path)
    (let ((context (m:service-context service)))
      (if (null context)
          (fail (list :error "not mounted under a context"))
          (case op
            (:save (op-checkpoint-save context (checkpoint-dir service) label keep))
            (:list (op-checkpoint-list (checkpoint-dir service)))
            (:restore (op-checkpoint-restore context path)))))))

(defun op-checkpoint-save (context dir label keep)
  (handler-case
      (multiple-value-bind (path interrupted unavailable)
          (checkpoint (m:service-process context) :dir dir :label label :keep keep)
        (ok :path (namestring path) :interrupted interrupted :unavailable unavailable))
    (file-error (e) (fail (list :error (princ-to-string e))))))

(defun op-checkpoint-list (dir)
  (ok :generations (generations :dir dir)))

(defun op-checkpoint-restore (context path)
  (if (not (probe-file path))
      (bad-request "no generation at ~a" path)
      (handler-case (rollback (m:service-process context) path)
        (error (e) (fail (list :error (princ-to-string e)))))))
