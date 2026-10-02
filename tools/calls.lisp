(in-package #:miao)

;;; The call log tool (~takeiteasy/miao#179): list and compact the call records the journal
;;; keeps, and resume (~takeiteasy/miao#77) the calls in it that a crash or restore
;;; cut short by sending :RESUME to the agent that dispatched them.
;;;
;;; :trust :operator: resuming runs a tool again, and listing shows what its
;;; calls carried, the same posture as tool-vault.

(define-tool :tool-calls
    (:trust :operator
     :summary "List and compact the tool call log, and resume calls it holds as lost"
     :slots ((path :initarg :path :initform nil :reader calls-tool-path
                   :documentation "Log path this mount reads and writes. NIL
(the default) uses the same default an agent's :JOURNAL T does."))
     :params ((:op (member :list :resume :compact) :required t
               :doc "operation to perform")
              (:ids (array-of string) :required-when (:op :resume)
               :doc "call log ids to resume, from :list")
              (:agent string :doc "agent to resume the calls in, for :resume;
by default the one that dispatched them")
              (:force boolean :default nil
               :doc "resume a call to a tool that is not :resumable, for :resume")
              (:status (member :all :accepted :running :ok :error :interrupted
                               :abandoned :lost)
               :default :all :doc "filter for :list")
              (:limit (integer 1 1000) :default 50 :doc "calls to answer, for :list")
              (:max-age (integer 0) :doc "seconds a finished call is kept, for
:compact; 0 drops every finished call (default *journal-max-age*)")))
  (:invoke (op ids agent force status limit max-age)
    (let ((path (%journal-path (or (calls-tool-path service) t))))
      (case op
        (:list (op-calls-list path status limit))
        (:resume (op-calls-resume service path ids agent force))
        (:compact (op-calls-compact path max-age))))))

(defun op-calls-list (path status limit)
  (let ((entries (if (eq status :all)
                     (call-entries path)
                     (remove status (call-entries path) :key (lambda (e) (getf e :status))
                                                        :test-not #'eq))))
    (ok :entries (last entries limit) :total (length entries))))

(defun op-calls-resume (service path ids agent-name force)
  (let* ((entries (call-entries path))
         (agents (remove-duplicates
                  (loop for id in ids
                        for entry = (find id entries :key (lambda (e) (getf e :id))
                                                     :test #'equal)
                        when (and entry (getf entry :agent)) collect (getf entry :agent))))
         (target (if agent-name
                     (a:make-keyword (string-upcase agent-name))
                     (and (null (rest agents)) (first agents)))))
    (if (null target)
        (bad-request ":agent is required: the calls name ~[no agent~:;more than one~]"
                     (length agents))
        (let ((process (m:lookup target :registry (m:service-registry service))))
          (if process
              (multiple-value-call #'%call-result
                (m:call process (list :resume :ids ids :force force)))
              (bad-request "no agent named ~(~a~)" target))))))

(defun op-calls-compact (path max-age)
  (multiple-value-bind (dropped kept)
      (if max-age (journal-compact path :max-age max-age) (journal-compact path))
    (if dropped
        (ok :dropped dropped :kept kept)
        (bad-request "call log has a malformed entry; not compacting"))))
