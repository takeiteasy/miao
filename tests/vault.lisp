(in-package #:miao/tests)
(in-suite :miao)

;;; The vault (~takeiteasy/miao#14): the log's own record/consume/fold API,
;;; AGENT's use of it through :VAULT and :STEER, and TOOL-VAULT.

(defun make-vault-log-path ()
  (format nil "~amiao-vault-test-~36r.log"
          (namestring (uiop:temporary-directory))
          (random (expt 2 64) (make-random-state t))))

(defmacro with-vault-path ((path) &body body)
  `(let ((,path (make-vault-log-path)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,path))
       (ignore-errors (delete-file (format nil "~a.lock" ,path))))))

(defun wait-for-vault-status (path status &optional (deadline 3.0))
  "PATH's entries once the first one reaches STATUS, or its current entries
once DEADLINE elapses -- for asserting on a fold that lands asynchronously,
after the message that triggered it has already returned."
  (loop repeat (ceiling deadline 0.05)
        for entries = (miao:vault-entries path)
        when (eq (getf (first entries) :status) status) return entries
        do (sleep 0.05)
        finally (return (miao:vault-entries path))))

;;; --- the log API ---------------------------------------------------------

(test vault-record-then-consume-updates-status
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "keep going")))
      (let ((entries (miao:vault-entries path)))
        (is (eql 1 (length entries)))
        (is (equal id (getf (first entries) :id)))
        (is (eq :assistant (getf (first entries) :agent)))
        (is (equal "keep going" (getf (first entries) :content)))
        (is (eq :pending (getf (first entries) :status))))
      (miao:vault-consume path id :folded)
      (let ((entry (first (miao:vault-entries path))))
        (is (eq :folded (getf entry :status)))
        (is (stringp (getf entry :consumed-at)))))))

(test vault-entries-defaults-a-fresh-record-to-pending
  (with-vault-path (path)
    (miao:vault-record path nil "hi")
    (is (eq :pending (getf (first (miao:vault-entries path)) :status)))))

(test a-malformed-vault-line-is-skipped
  (with-vault-path (path)
    (miao:vault-record path :assistant "one")
    (with-open-file (stream path :direction :output :if-exists :append)
      (write-string "(not-even-balanced" stream)
      (terpri stream))
    ;; The malformed line ends the read rather than being skipped mid-stream
    ;; -- READ signals on it and %READ-LOG stops there -- so only the entry
    ;; written ahead of it survives.
    (is (eql 1 (length (miao:vault-entries path))))))

(test vault-log-reading-never-evaluates
  ;; *READ-EVAL* is nil around the read, the same guard a generation's own
  ;; read and tool-self's log both apply: a #. line fails to read rather
  ;; than running, ending the read there without signalling out.
  (with-vault-path (path)
    (miao:vault-record path :assistant "before")
    (with-open-file (stream path :direction :output :if-exists :append)
      (write-string "(:kind :steer :id \"evil\" :at \"t\" :agent nil :content #.(error \"read-eval ran\"))"
                    stream)
      (terpri stream))
    (is (equal '("before") (mapcar (lambda (e) (getf e :content)) (miao:vault-entries path))))))

;;; --- the agent's own use of the vault -------------------------------------

(test a-steer-mid-run-is-recorded-then-folded-and-consumed
  (let ((n 0))
    (with-vault-path (path)
      (with-agent ((lambda (&rest request)
                     (declare (ignore request))
                     (incf n)
                     (if (= n 1)
                         (tool-call-reply "c1" "tool-echo" "{\"text\":\"hi\"}")
                         (final-reply "done")))
                  'tool-echo)
        (m:with-process (runner)
          (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                   :tools '(:tool-echo) :vault path)))
            (m:cast child (list :run :messages '((:role :user :content "go"))))
            (m:cast child (list :steer :content "also do this"))
            (multiple-value-bind (message received) (m:receive :timeout 5)
              (is-true received)
              (is (eq :agent-done (first message)))))))
      (let ((entries (miao:vault-entries path)))
        (is (eql 1 (length entries)))
        (is (equal "also do this" (getf (first entries) :content)))
        (is (eq :folded (getf (first entries) :status)))))))

(test a-steer-during-the-last-turn-gets-a-turn-of-its-own
  ;; The backend never calls a tool, so the first turn would finish the run;
  ;; a steer queued while it is in flight earns a second turn instead.
  (with-vault-path (path)
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (sleep 0.3)
                   (final-reply "done")))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed :vault path)))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          ;; A moment for :RUN's own :STEP to fold the (empty) queue and
          ;; spawn the request, so this steer queues into the turn in flight.
          (sleep 0.05)
          (m:cast child (list :steer :content "one more thing"))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :agent-done (first message)))
            (is (eql 2 (getf (second (fourth message)) :turns)))
            (is (eq :stop (getf (second (fourth message)) :stop-reason))))))
      (is (eql 2 (length (requests))))
      (is (search "one more thing" (getf (second (requests)) :body))))
    (is (eq :folded (getf (first (miao:vault-entries path)) :status)))))

(test a-steer-during-the-last-allowed-turn-stays-pending
  ;; With :max-turns spent there is no turn left to fold into, so the run
  ;; finishes as the model did and the steer waits for the next :run.
  (with-vault-path (path)
    (with-agent ((lambda (&rest request)
                   (declare (ignore request))
                   (sleep 0.3)
                   (final-reply "done")))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :vault path :max-turns 1)))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (sleep 0.05)
          (m:cast child (list :steer :content "too late"))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :stop (getf (second (fourth message)) :stop-reason)))))))
    (is (eq :pending (getf (first (miao:vault-entries path)) :status)))))

(test an-interrupt-on-the-last-allowed-turn-finishes-max-turns
  (with-vault-path (path)
    (multiple-value-bind (result events) (interrupt-mid-stream :max-turns 1 :vault path)
      (is (eq :max-turns (getf (second result) :stop-reason)))
      (is (equal '(:run-start :turn :text-delta :turn-interrupted :run-done) (event-types events))))
    (is (eq :pending (getf (first (miao:vault-entries path)) :status)))))

(test a-steer-before-run-is-folded-after-the-seed
  (with-vault-path (path)
    (with-agent ((final-reply "done"))
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed :vault path)))
          (m:cast child (list :steer :content "queued early"))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (multiple-value-bind (message received) (m:receive :timeout 5)
            (is-true received)
            (is (eq :agent-done (first message)))
            (is (find "queued early" (getf (second (fourth message)) :messages)
                     :key (lambda (m) (miao:content-text (getf m :content)))
                     :test #'equal)))))
      (is (eq :folded (getf (first (miao:vault-entries path)) :status))))))

(test with-no-vault-option-steering-records-nothing
  (with-agent ((final-reply "done"))
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed)))
        (m:cast child (list :steer :content "hi"))
        (m:cast child (list :run :messages '((:role :user :content "go"))))
        (multiple-value-bind (message received) (m:receive :timeout 5)
          (is-true received)
          (is (eq :agent-done (first message)))))))
  ;; Vault-off never resolves the lazy default path -- checked directly,
  ;; since there is otherwise no path to read the (non-)entries back from.
  (is (null miao::*vault-log*)))

;;; --- tool-vault ------------------------------------------------------

(defun vault (op &rest args)
  (apply #'miao:invoke-tool :tool-vault op args))

(test tool-vault-is-operator-trusted
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (is (eq :operator (miao:tool-trust (miao:describe-tool :tool-vault)))))))

(test tool-vault-list-filters-by-status
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (let ((pending-id (miao:vault-record path :assistant "a"))
            (folded-id (miao:vault-record path :assistant "b")))
        (miao:vault-consume path folded-id :folded)
        (let ((pending (getf (second (vault :op :list)) :entries)))
          (is (eql 1 (length pending)))
          (is (equal pending-id (getf (first pending) :id))))
        (let ((all (getf (second (vault :op :list :status :all)) :entries)))
          (is (eql 2 (length all))))))))

(test tool-vault-restore-delivers-into-a-named-agent-and-consumes-it
  (let ((n 0))
    (with-vault-path (path)
      (with-agent ((lambda (&rest request)
                     (declare (ignore request))
                     (incf n)
                     (final-reply "done")))
        (m:mount *ctx* 'miao:tool-vault :path path)
        (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed :vault path)
        (let ((id (miao:vault-record path :assistant "restored")))
          (let ((result (vault :op :restore :id id)))
            (is (eq :ok (first result)))
            (is (eq :assistant (getf (second result) :agent))))
          ;; Not consumed until the target actually folds it in.
          (is (eq :pending (getf (first (miao:vault-entries path)) :status)))
          (is (eq :ok (m:call (m:lookup :assistant)
                              (list :run :messages '((:role :user :content "go"))))))
          ;; :RUN answers :OK once START-RUN itself is done, before its own
          ;; :STEP -- which folds the queue -- has necessarily been
          ;; processed, so the fold is awaited rather than checked at once.
          (is (eq :folded (getf (first (wait-for-vault-status path :folded)) :status))))))))

(test tool-vault-restore-requires-an-agent-when-none-was-recorded
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (let ((id (miao:vault-record path nil "no agent")))
        (is (equal :bad-request (first (miao:tool-error (vault :op :restore :id id)))))))))

(test tool-vault-restore-rejects-an-unknown-id
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (is (equal :bad-request (first (miao:tool-error (vault :op :restore :id "nope"))))))))

(test tool-vault-discard-marks-an-entry-discarded
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (let ((id (miao:vault-record path :assistant "drop me")))
        (is (eq :ok (first (vault :op :discard :id id))))
        (is (eq :discarded (getf (first (miao:vault-entries path)) :status)))))))

(test tool-vault-discard-rejects-an-already-consumed-id
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (let ((id (miao:vault-record path :assistant "drop me")))
        (vault :op :discard :id id)
        (is (equal :bad-request (first (miao:tool-error (vault :op :discard :id id)))))))))

;;; --- compaction (~takeiteasy/miao#67) ---------------------------------------

(defun append-raw-vault-line (path form)
  (with-open-file (stream path :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((*package* (find-package "KEYWORD")) (*print-case* :downcase))
      (prin1 form stream)
      (terpri stream))))

(defun record-old-consumed (path id at)
  "A steer at PATH consumed at the iso8601 time AT."
  (append-raw-vault-line path (list :kind :steer :id id :at at :agent :assistant :content id))
  (append-raw-vault-line path (list :kind :consumed :id id :at at :how :folded)))

(test vault-compact-drops-only-consumed-entries-past-max-age
  (with-vault-path (path)
    (record-old-consumed path "old" "2020-01-01T00:00:00Z")
    (let ((recent (miao:vault-record path :assistant "recent"))
          (pending (miao:vault-record path :assistant "pending")))
      (miao:vault-consume path recent :folded)
      (is (equal '(1 2) (multiple-value-list (miao:vault-compact path :max-age 3600))))
      (is (equal (list recent pending)
                 (mapcar (lambda (e) (getf e :id)) (miao:vault-entries path))))
      (is (eq :folded (getf (first (miao:vault-entries path)) :status)))
      (is (eq :pending (getf (second (miao:vault-entries path)) :status))))))

(test vault-compact-keeps-a-pending-entry-consumable
  (with-vault-path (path)
    (record-old-consumed path "old" "2020-01-01T00:00:00Z")
    (let ((id (miao:vault-record path :assistant "pending")))
      (miao:vault-compact path)
      (miao:vault-consume path id :discarded)
      (is (eq :discarded (getf (first (miao:vault-entries path)) :status))))))

(test vault-compact-max-age-zero-drops-every-consumed-entry
  (with-vault-path (path)
    (let ((a (miao:vault-record path :assistant "a"))
          (b (miao:vault-record path :assistant "b")))
      (miao:vault-consume path a :folded)
      (is (equal '(1 1) (multiple-value-list (miao:vault-compact path :max-age 0))))
      (is (equal (list b) (mapcar (lambda (e) (getf e :id)) (miao:vault-entries path)))))))

(test vault-compact-refuses-a-malformed-log
  (with-vault-path (path)
    (record-old-consumed path "old" "2020-01-01T00:00:00Z")
    (with-open-file (stream path :direction :output :if-exists :append)
      (write-string "(torn" stream))
    (let ((before (uiop:read-file-string path)))
      (is (null (miao:vault-compact path)))
      (is (equal before (uiop:read-file-string path))))))

(test appending-past-the-size-threshold-compacts
  (with-vault-path (path)
    (record-old-consumed path "old" "2020-01-01T00:00:00Z")
    (let ((miao:*vault-compact-size* 1))
      (miao:vault-record path :assistant "new"))
    (is (equal '("new") (mapcar (lambda (e) (getf e :content)) (miao:vault-entries path))))))

(test tool-vault-compact-answers-counts
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (let ((id (miao:vault-record path :assistant "a")))
        (miao:vault-record path :assistant "b")
        (vault :op :discard :id id)
        (let ((result (vault :op :compact :max-age 0)))
          (is (eq :ok (first result)))
          (is (eql 1 (getf (second result) :dropped)))
          (is (eql 1 (getf (second result) :kept))))))))

(test tool-vault-compact-reports-a-malformed-log
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (miao:vault-record path :assistant "a")
      (with-open-file (stream path :direction :output :if-exists :append)
        (write-string "(torn" stream))
      (is (equal :bad-request (first (miao:tool-error (vault :op :compact))))))))

;;; --- atomic discard (~takeiteasy/miao#85) -------------------------------------

(test vault-consume-pending-answers-the-existing-status
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "a")))
      (is (eq :unknown (miao:vault-consume-pending path "nope" :discarded)))
      (is (eq :consumed (miao:vault-consume-pending path id :discarded)))
      (is (eq :discarded (miao:vault-consume-pending path id :folded))))))

(test concurrent-discards-of-one-id-consume-it-once
  (with-vault-path (path)
    (let* ((id (miao:vault-record path :assistant "a"))
           (results '())
           (rlock (bt:make-lock))
           (threads (loop repeat 8
                          collect (bt:make-thread
                                   (lambda ()
                                     (let ((r (miao:vault-consume-pending path id :discarded)))
                                       (bt:with-lock-held (rlock) (push r results))))))))
      (mapc #'bt:join-thread threads)
      (is (eql 1 (count :consumed results)))
      (is (eql 2 (length (miao::%read-log path)))))))

;;; --- cross-process lock (~takeiteasy/miao#84) ---------------------------------

(test vault-compact-waits-for-a-lock-held-by-another-process
  (with-vault-path (path)
    (miao:vault-record path :assistant "a")
    (is-true (blocks-on-foreign-flock-p path (lambda () (miao:vault-compact path :max-age 0))))))

(test vault-consume-pending-waits-for-a-lock-held-by-another-process
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "a")))
      (is-true (blocks-on-foreign-flock-p
                path (lambda () (miao:vault-consume-pending path id :discarded))))
      (is (eq :discarded (getf (first (miao:vault-entries path)) :status))))))

;;; --- single-delivery restore (~takeiteasy/miao#87) ----------------------------

(defun wait-for-claimed (path &optional (deadline 3.0))
  (loop repeat (ceiling deadline 0.05)
        for entries = (miao:vault-entries path)
        when (getf (first entries) :claimed) return entries
        do (sleep 0.05)
        finally (return (miao:vault-entries path))))

(test concurrent-claims-of-one-id-succeed-once
  (with-vault-path (path)
    (let* ((id (miao:vault-record path :assistant "a"))
           (results '())
           (rlock (bt:make-lock))
           (threads (loop repeat 8
                          collect (bt:make-thread
                                   (lambda ()
                                     (let ((r (miao:vault-claim-pending path id)))
                                       (bt:with-lock-held (rlock) (push r results))))))))
      (mapc #'bt:join-thread threads)
      (is (eql 1 (count :claimed results)))
      (is (eql 7 (count :held results))))))

(test tool-vault-restore-of-a-claimed-entry-is-refused
  (with-vault-path (path)
    (with-agent ((lambda (&rest request) (declare (ignore request)) (final-reply "done")))
      (m:mount *ctx* 'miao:tool-vault :path path)
      (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed :vault path)
      (let ((id (miao:vault-record path :assistant "once")))
        (is (eq :ok (first (vault :op :restore :id id))))
        (is (eq :bad-request (first (miao:tool-error (vault :op :restore :id id)))))
        (is (eq :bad-request (first (miao:tool-error (vault :op :discard :id id)))))
        (is (eq :pending (getf (first (miao:vault-entries path)) :status)))
        (is-true (getf (first (miao:vault-entries path)) :claimed))))))

(test a-restore-is-delivered-once
  (let ((n 0))
    (with-vault-path (path)
      (with-agent ((lambda (&rest request)
                     (declare (ignore request))
                     (incf n)
                     (final-reply "done")))
        (m:mount *ctx* 'miao:tool-vault :path path)
        (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed :vault path)
        (let ((id (miao:vault-record path :assistant "once")))
          (vault :op :restore :id id)
          (vault :op :restore :id id)
          (m:call (m:lookup :assistant) (list :run :messages '((:role :user :content "go"))))
          (wait-for-vault-status path :folded)
          (is (eql 1 (length (remove-if-not (lambda (e) (eq (getf e :kind) :consumed))
                                            (miao::%read-log path))))))))))

(test a-queued-steer-is-claimed-until-it-folds
  (with-vault-path (path)
    (with-agent ((lambda (&rest request) (declare (ignore request)) (final-reply "done")))
      (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed :vault path)
      (m:cast (m:lookup :assistant) '(:steer :content "queued"))
      (is-true (getf (first (wait-for-claimed path)) :claimed))
      (m:call (m:lookup :assistant) (list :run :messages '((:role :user :content "go"))))
      (let ((entry (first (wait-for-vault-status path :folded))))
        (is (eq :folded (getf entry :status)))
        (is-false (getf entry :claimed))))))

(test rolling-an-agent-back-releases-its-queued-claims
  (with-vault-path (path)
    (with-agent (nil)
      (m:mount *ctx* 'miao:tool-vault :path path)
      (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed :vault path)
      (m:cast (m:lookup :assistant) '(:steer :content "queued"))
      (is-true (getf (first (wait-for-claimed path)) :claimed))
      (m:call (m:lookup :assistant) (list :restore '(:messages nil :turns 0)))
      (is-false (getf (first (miao:vault-entries path)) :claimed))
      (is (eq :ok (first (vault :op :discard :id (getf (first (miao:vault-entries path)) :id))))))))

(test restoring-into-an-agent-with-no-vault-folds-into-the-tools-log
  (with-vault-path (path)
    (with-agent ((lambda (&rest request) (declare (ignore request)) (final-reply "done")))
      (m:mount *ctx* 'miao:tool-vault :path path)
      (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed)
      (let ((id (miao:vault-record path :assistant "hi")))
        (vault :op :restore :id id)
        (m:call (m:lookup :assistant) (list :run :messages '((:role :user :content "go"))))
        (is (eq :folded (getf (first (wait-for-vault-status path :folded)) :status)))))))

;;; --- claims across processes (~takeiteasy/miao#88) ------------------------------

(defun foreign-owner (&key (pid 1) (host (machine-instance)) start)
  (list* :pid pid :host host :token "another-image" (and start (list :start start))))

(defun append-claim (path id owner)
  (miao::%append-log path (list :kind :claimed :id id :at (miao::%now-iso8601) :by owner)))

(defun dead-pid ()
  "A pid with no running process."
  (loop for pid from 99990 downto 2
        unless (handler-case (progn (sb-posix:kill pid 0) t)
                 (sb-posix:syscall-error (e) (= (sb-posix:syscall-errno e) sb-posix:eperm)))
          return pid))

(test another-process-claim-blocks-restore-and-discard
  (with-vault-path (path)
    (with-agent ((lambda (&rest request) (declare (ignore request)) (final-reply "done")))
      (m:mount *ctx* 'miao:tool-vault :path path)
      (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed :vault path)
      (let ((id (miao:vault-record path :assistant "once")))
        (append-claim path id (foreign-owner))
        (is-true (getf (first (miao:vault-entries path)) :claimed))
        (is (eq :held (miao:vault-claim-pending path id)))
        (is (eq :bad-request (first (miao:tool-error (vault :op :restore :id id)))))
        (is (eq :bad-request (first (miao:tool-error (vault :op :discard :id id)))))
        (is (eq :pending (getf (first (miao:vault-entries path)) :status)))))))

(test a-claim-on-another-host-is-always-live
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "once")))
      (append-claim path id (foreign-owner :pid (dead-pid) :host "elsewhere.invalid"))
      (is (eq :held (miao:vault-claim-pending path id))))))

(test a-claim-whose-owner-process-is-gone-is-restorable
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "once")))
      (append-claim path id (foreign-owner :pid (dead-pid)))
      (is-false (getf (first (miao:vault-entries path)) :claimed))
      (is (eq :claimed (miao:vault-claim-pending path id))))))

(test process-start-time-reads-this-process
  (let ((start (miao::%process-start-time (sb-posix:getpid))))
    (is (integerp start))
    (is (eql start (miao::%process-start-time (sb-posix:getpid)))))
  (is (null (miao::%process-start-time (dead-pid)))))

(test a-claim-whose-pid-was-reused-is-restorable
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "once"))
          (start (miao::%process-start-time (sb-posix:getpid))))
      (append-claim path id (foreign-owner :pid (sb-posix:getpid) :start (1- start)))
      (is-false (getf (first (miao:vault-entries path)) :claimed))
      (is (eq :claimed (miao:vault-claim-pending path id))))))

(test a-claim-with-a-matching-start-time-is-live
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "once"))
          (ppid (sb-posix:getppid)))
      (append-claim path id (foreign-owner :pid ppid :start (miao::%process-start-time ppid)))
      (is (eq :held (miao:vault-claim-pending path id))))))

(test a-claim-from-an-earlier-image-of-this-process-is-restorable
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "once")))
      (append-claim path id (foreign-owner :pid (sb-posix:getpid)
                                           :start (miao::%process-start-time (sb-posix:getpid))))
      (is-false (getf (first (miao:vault-entries path)) :claimed))
      (is (eq :claimed (miao:vault-claim-pending path id))))))

(test saving-an-image-forgets-the-vault-token
  (is (member 'miao::%forget-vault-token sb-ext:*save-hooks*))
  (let ((miao::*vault-token* "abc"))
    (miao::%forget-vault-token)
    (is (null miao::*vault-token*))))

(defun queue-cell (path id content)
  (list* id path (list :role :user :content content)))

(defun claim-token (path id)
  (getf (gethash id (miao::%claims-by-id (miao::%read-log path))) :token))

(test reclaiming-keeps-a-free-steer-and-claims-it-as-this-image
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "hi" :claim t))
          (agent (make-instance 'miao:agent)))
      (miao:vault-release path id)
      (setf (miao::%steer-queue agent) (list (queue-cell path id "hi")))
      (miao::reclaim-steer-claims agent)
      (is (eql 1 (length (miao::%steer-queue agent))))
      (is (equal (getf (miao::%vault-owner) :token) (claim-token path id))))))

(test reclaiming-drops-a-steer-another-process-holds
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "hi"))
          (agent (make-instance 'miao:agent)))
      (append-claim path id (foreign-owner))
      (setf (miao::%steer-queue agent) (list (queue-cell path id "hi")))
      (miao::reclaim-steer-claims agent)
      (is (null (miao::%steer-queue agent))))))

(test reclaiming-drops-a-consumed-steer
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "hi"))
          (agent (make-instance 'miao:agent)))
      (miao:vault-consume path id :discarded)
      (setf (miao::%steer-queue agent) (list (queue-cell path id "hi")))
      (miao::reclaim-steer-claims agent)
      (is (null (miao::%steer-queue agent))))))

(test reclaiming-keeps-a-steer-with-no-vault-entry
  (let ((agent (make-instance 'miao:agent)))
    (setf (miao::%steer-queue agent) (list (queue-cell nil nil "hi")))
    (miao::reclaim-steer-claims agent)
    (is (eql 1 (length (miao::%steer-queue agent))))))

(test a-released-claim-can-be-claimed-again
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "once")))
      (is (eq :claimed (miao:vault-claim-pending path id)))
      (miao:vault-release path id)
      (is (member :released (miao::%read-log path) :key (lambda (e) (getf e :kind))))
      (is (eq :claimed (miao:vault-claim-pending path id))))))

(test releasing-does-not-touch-another-processs-claim
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "once")))
      (append-claim path id (foreign-owner))
      (miao:vault-release path id)
      (is (eq :held (miao:vault-claim-pending path id))))))

(test releasing-many-claims-appends-one-line-each-and-skips-the-rest
  (with-vault-path (path)
    (let ((mine-a (miao:vault-record path :assistant "a" :claim t))
          (mine-b (miao:vault-record path :assistant "b" :claim t))
          (theirs (miao:vault-record path :assistant "c"))
          (done (miao:vault-record path :assistant "d" :claim t)))
      (append-claim path theirs (foreign-owner))
      (miao:vault-consume path done :discarded)
      (miao:vault-release-all path (list mine-a mine-b mine-b theirs done "no-such-id"))
      (is (equal (sort (loop for e in (miao::%read-log path)
                             when (eq (getf e :kind) :released) collect (getf e :id))
                       #'string<)
                 (sort (list mine-a mine-b) #'string<)))
      (is (eq :claimed (miao:vault-claim-pending path mine-a)))
      (is (eq :held (miao:vault-claim-pending path theirs))))))

(test dropping-an-agent-releases-all-its-queued-claims
  (with-vault-path (path)
    (let ((agent (make-instance 'miao:agent))
          (ids (loop for content in '("a" "b" "c")
                     collect (miao:vault-record path :assistant content :claim t))))
      (setf (miao::%steer-queue agent)
            (append (mapcar (lambda (id) (queue-cell path id "x")) ids)
                    (list (queue-cell nil nil "plain"))))
      (miao::release-steer-claims agent)
      (is (notany (lambda (e) (getf e :claimed)) (miao:vault-entries path))))))

(test a-recorded-agent-steer-carries-its-claim
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "hi" :claim t)))
      (is (equal (getf (miao::%vault-owner) :token)
                 (getf (getf (first (miao::%read-log path)) :claimed-by) :token)))
      (is (eql (miao::%process-start-time (sb-posix:getpid))
               (getf (getf (first (miao::%read-log path)) :claimed-by) :start)))
      (is (eq :held (miao:vault-claim-pending path id))))))

(test compaction-keeps-a-live-claim-and-drops-claim-lines
  (with-vault-path (path)
    (let ((id (miao:vault-record path :assistant "hi")))
      (append-claim path id (foreign-owner))
      (miao:vault-compact path)
      (is (eql 1 (length (miao::%read-log path))))
      (is (eq :held (miao:vault-claim-pending path id))))))
