(in-package #:miao)

;;; The vault tool: list, restore, discard and compact entries
;;; in the steering message log vault.lisp keeps. Restoring casts a :STEER
;;; straight at the named agent (agent.lisp), passing the entry's own id
;;; back as :VAULT-ID so ISSUE-TURN marks it consumed rather than this tool
;;; recording a second entry for the same steer.
;;;
;;; :trust :operator: restoring injects a :USER-role message into whichever
;;; agent is named, and listing surfaces operator-written steering text --
;;; the same posture as tool-checkpoint's own writes to harness state.

(define-tool :tool-vault
    (:trust :operator
     :summary "List, restore and discard entries in the steering message vault"
     :slots ((path :initarg :path :initform nil :reader vault-tool-path
                   :documentation "Log path this mount reads and writes. NIL
(the default) uses the same default an agent's :VAULT T does."))
     :params ((:op (member :list :restore :discard :compact) :required t
               :doc "operation to perform")
              (:id string :required-when (:op (:restore :discard)) :doc "vault entry id")
              (:agent string :doc "target agent name for :restore; required
when the entry was recorded with no agent (a delegated sub-agent has none),
and overrides the recorded one otherwise")
              (:status (member :pending :folded :discarded :all) :default :pending
               :doc "filter for :list")
              (:limit (integer 1 1000) :default 50 :doc "entries to answer, for :list")
              (:max-age (integer 0) :doc "seconds a consumed entry is kept, for
:compact; 0 drops every consumed entry (default *vault-max-age*)")))
  (:invoke (op id agent status limit max-age)
    (case op
      (:list (op-vault-list (vault-log-path service) status limit))
      (:restore (op-vault-restore service id agent))
      (:discard (op-vault-discard (vault-log-path service) id))
      (:compact (op-vault-compact (vault-log-path service) max-age)))))

(defun vault-log-path (service)
  (%vault-path (or (vault-tool-path service) t)))

(defun %vault-find (path id)
  (find id (vault-entries path) :key (lambda (e) (getf e :id)) :test #'equal))

(defun op-vault-list (path status limit)
  (let ((entries (if (eq status :all)
                     (vault-entries path)
                     (remove-if-not (lambda (e) (eq (getf e :status) status))
                                    (vault-entries path)))))
    (ok :entries (last entries limit) :total (length entries))))

(defun op-vault-restore (service id agent-name)
  (let ((entry (%vault-find (vault-log-path service) id)))
    (cond
      ((null entry) (bad-request "no vault entry ~a" id))
      ((not (eq (getf entry :status) :pending))
       (bad-request "vault entry ~a is already ~(~a~)" id (getf entry :status)))
      ((getf entry :claimed)
       (bad-request "vault entry ~a is already queued at an agent" id))
      (t (op-vault-restore-into service id entry agent-name)))))

(defun op-vault-restore-into (service id entry agent-name)
  (let ((target (or (and agent-name (a:make-keyword (string-upcase agent-name)))
                    (getf entry :agent))))
    (if (null target)
        (bad-request ":agent is required to restore an entry recorded with no agent")
        (let ((process (m:lookup target :registry (m:service-registry service)))
              (path (vault-log-path service)))
          (if (null process)
              (bad-request "no agent named ~(~a~)" target)
              (let ((claim (vault-claim-pending path id)))
                (case claim
                  (:claimed
                   (m:cast process (list :steer :content (getf entry :content)
                                         :vault-id id :vault-path path))
                   (ok :agent target :id id))
                  (:unknown (bad-request "no vault entry ~a" id))
                  (:held (bad-request "vault entry ~a is already queued at an agent" id))
                  (t (bad-request "vault entry ~a is already ~(~a~)" id claim)))))))))

(defun op-vault-discard (path id)
  (let ((result (vault-consume-pending path id :discarded)))
    (case result
      (:consumed (ok :id id))
      (:unknown (bad-request "no vault entry ~a" id))
      (:held (bad-request "vault entry ~a is queued at an agent; it cannot be discarded" id))
      (t (bad-request "vault entry ~a is already ~(~a~)" id result)))))

(defun op-vault-compact (path max-age)
  (multiple-value-bind (dropped kept)
      (if max-age (vault-compact path :max-age max-age) (vault-compact path))
    (if dropped
        (ok :dropped dropped :kept kept)
        (bad-request "vault log has a malformed entry; not compacting"))))
