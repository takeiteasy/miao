(in-package #:miao)

;;; The run journal (~takeiteasy/miao#119): an append-only s-expression log of
;;; what an agent's runs did, read back the same guarded way the vault is
;;; (checkpoint.lisp's %APPEND-LOG/%READ-LOG). Every entry carries :ID, :AT,
;;; :AGENT, :PARENT (a sub-agent's only) and :RUN, the key of the root run.
;;;
;;;   (:kind :settings :settings plist)
;;;   (:kind :messages :reset t-or-nil :messages (message ...))
;;;   (:kind :message  :message message)
;;;   (:kind :event    :type :tool-result :ref r ...)
;;;
;;; :MESSAGES and :MESSAGE are the conversation, kept exactly: a :RESET :MESSAGES
;;; replaces it, any other adds to it. An :EVENT is one the loop emits, less the
;;; streamed deltas; its :ARGUMENTS, :RESULT, :MESSAGES, :CONTENT and :MESSAGE
;;; are text cut to the cap. See docs/journal.md.

(defvar *journal-max-age* (* 7 24 60 60)
  "Seconds before an entry is old enough for JOURNAL-COMPACT to fold away.")

(defvar *journal-compact-size* (* 1024 1024)
  "Log size in bytes past which a run's start compacts the log.")

(defvar *journal-max-content* (* 16 1024)
  "Characters of an event's payload kept when the agent has no :MAX-TOOL-RESULT.")

(defvar *journal* nil
  "Default log path: ~/.miao/journal.log, resolved lazily.")

(defparameter +journal-skipped-events+ '(:text-delta :tool-call-delta :done)
  "Streamed events, which the :REPLY event and the :MESSAGE entry hold whole.")

(defparameter +journal-capped-keys+ '(:arguments :result :messages :content :message))

(defun %journal-path (spec)
  "SPEC, an agent's :JOURNAL mount option, as a log path: NIL means off, T the
default, anything else is used as given."
  (cond ((eq spec t)
         (or *journal*
             (setf *journal* (merge-pathnames ".miao/journal.log" (user-homedir-pathname)))))
        (t spec)))

;;; --- the writer -------------------------------------------------------------

;;; The conversation, events and :RUNNING lines are queued to a thread for each
;;; log, which writes them in batches, so the agent does not wait on the disk.
;;; What must be on disk before something happens, a :CALL before its tool runs
;;; or a :DONE, is written once the queue has drained, which keeps the log in
;;; order. A reader in this process drains first, so it reads what it wrote.

(defvar *journal-writer-idle* 5
  "Seconds a log's writer thread waits for an entry before it exits.")

(defvar *journal-writer-max-bytes* (* 8 1024 1024)
  "Characters a log's queue holds before the agent waits to add more. The batch
being written is counted apart, so twice this can be held. A line that is longer
is queued once the queue is empty.")

(defvar *journal-exit-timeout* 5
  "Seconds the process's exit waits in all for the writers to drain.")

(defvar *journal-write-retries* '(0.1 0.5 2)
  "Seconds to wait before each retry of a batch the disk refuses.")

(defstruct (journal-writer (:conc-name jw-))
  path
  (lock (bt:make-lock :name "miao-journal-writer"))
  (wake (bt:make-condition-variable))
  (queue '())
  (bytes 0)
  (busy nil)
  (retiring nil)
  (thread nil))

(defvar *journal-writers* (make-hash-table :test 'equal)
  "Canonical log namestring -> its writer. Guarded by *LOG-LOCKS-LOCK*.")

(defvar *journal-writers-by-spelling* (make-hash-table :test 'equal)
  "A path as spelled -> its writer, so an append asks the file system nothing once
a log has been used. Guarded by *LOG-LOCKS-LOCK*.")

(defun %journal-writer (path)
  "PATH's writer: the one every spelling of the log shares, made on first use."
  (let ((spelling (namestring (merge-pathnames path))))
    (or (bt:with-lock-held (*log-locks-lock*)
          (gethash spelling *journal-writers-by-spelling*))
        (progn
          (ensure-directories-exist path)
          (let ((key (%log-key path)))
            (bt:with-lock-held (*log-locks-lock*)
              (setf (gethash spelling *journal-writers-by-spelling*)
                    (or (gethash key *journal-writers*)
                        (setf (gethash key *journal-writers*)
                              (make-journal-writer :path key))))))))))

(defun %journal-take-batch (writer)
  "The entries queued at WRITER, oldest first, once there are some. Nil, after
the thread has waited *JOURNAL-WRITER-IDLE* seconds for them, means it is retired."
  (bt:with-lock-held ((jw-lock writer))
    (when (and (null (jw-queue writer)) (not (jw-retiring writer)))
      (bt:condition-wait (jw-wake writer) (jw-lock writer) :timeout *journal-writer-idle*))
    (cond ((jw-queue writer)
           (setf (jw-busy writer) t)
           (prog1 (reverse (jw-queue writer))
             (setf (jw-queue writer) nil
                   (jw-bytes writer) 0)
             (bt:condition-broadcast (jw-wake writer))))
          (t (setf (jw-thread writer) nil)
             nil))))

(defun %journal-write-batch (writer batch)
  "Append BATCH, less its :CHECK markers, to WRITER's log, then wake whoever drains.
A batch the disk refuses is retried after each of *JOURNAL-WRITE-RETRIES*, then
dropped, with a warning."
  (unwind-protect
       (let ((lines (remove :check batch))
             (path (jw-path writer))
             (delays *journal-write-retries*))
         (when lines
           (loop
             (handler-case (progn (with-log-lock (path) (%append-log-lines-locked path lines))
                                  (return))
               (error (e)
                 (cond (delays (sleep (pop delays)))
                       (t (format *error-output* "~&miao: ~d journal entries dropped, ~a: ~a~%"
                                  (length lines) path e)
                          (return))))))))
    (bt:with-lock-held ((jw-lock writer))
      (setf (jw-busy writer) nil)
      (bt:condition-broadcast (jw-wake writer)))))

(defun %journal-writer-loop (writer)
  (loop for batch = (%journal-take-batch writer)
        while batch
        do (%journal-write-batch writer batch)
           (%maybe-compact (jw-path writer) *journal-compact-size* #'journal-compact)))

(defun %journal-line (entry)
  "ENTRY, a plist, printed as a log line, or nil when it would not read back. The
agent prints it, so the writer never reads data the agent goes on to change."
  (handler-case (let ((line (let ((*print-readably* t) (*print-pretty* nil))
                              (%print-initargs entry))))
                  (%read-initargs line)
                  line)
    (error () nil)))

(defun %journal-enqueue (path line)
  "Queue LINE, an entry printed by %JOURNAL-LINE, or :CHECK, which only asks the
writer to see whether the log wants compacting, for PATH's writer. Waits while
the queue is full."
  (let ((writer (%journal-writer path))
        (size (if (stringp line) (length line) 0)))
    (bt:with-lock-held ((jw-lock writer))
      (when (plusp size)
        (loop while (and (jw-queue writer)
                         (> (+ (jw-bytes writer) size) *journal-writer-max-bytes*))
              do (bt:condition-wait (jw-wake writer) (jw-lock writer))))
      (push line (jw-queue writer))
      (incf (jw-bytes writer) size)
      (unless (jw-thread writer)
        (setf (jw-thread writer)
              (bt:make-thread (lambda () (%journal-writer-loop writer))
                              :name "miao-journal-writer")))
      (bt:condition-broadcast (jw-wake writer)))))

(defun journal-drain (path &key timeout)
  "Wait until every entry queued for PATH is on disk, or for TIMEOUT seconds if
given. True once it is, nil if the time ran out first."
  (let* ((key (ignore-errors (%log-key path)))
         (writer (and key (bt:with-lock-held (*log-locks-lock*)
                            (gethash key *journal-writers*)))))
    (flet ((pending ()
             (bt:with-lock-held ((jw-lock writer))
               (or (jw-queue writer) (jw-busy writer)))))
      (cond ((or (null writer) (eq (bt:current-thread) (jw-thread writer))) t)
            (timeout
             (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
               (loop while (pending)
                     do (when (> (get-internal-real-time) deadline)
                          (return-from journal-drain nil))
                        (sleep 0.01))
               t))
            (t (bt:with-lock-held ((jw-lock writer))
                 (loop while (or (jw-queue writer) (jw-busy writer))
                       do (bt:condition-wait (jw-wake writer) (jw-lock writer))))
               t)))))

(defun journal-drain-all ()
  "Drain every log, for the process's exit, waiting at most *JOURNAL-EXIT-TIMEOUT*
seconds in all. A log still queued then is left, with a warning."
  (let ((deadline (+ (get-internal-real-time)
                     (* *journal-exit-timeout* internal-time-units-per-second))))
    (dolist (path (bt:with-lock-held (*log-locks-lock*)
                    (loop for writer being the hash-values of *journal-writers*
                          collect (jw-path writer))))
      (unless (journal-drain path :timeout (max 0 (/ (- deadline (get-internal-real-time))
                                                     internal-time-units-per-second)))
        (format *error-output* "~&miao: journal entries left queued for ~a~%" path)))))

(defun journal-retire-writers (&key (timeout 5))
  "Drain every log's writer and stop its thread, for SAVE-IMAGE, which needs the
main thread alone, waiting at most TIMEOUT seconds. A writer that is still
writing then is left running. A later entry starts the writer again."
  (let ((writers (bt:with-lock-held (*log-locks-lock*)
                   (loop for writer being the hash-values of *journal-writers* collect writer)))
        (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (dolist (writer writers)
      (bt:with-lock-held ((jw-lock writer))
        (setf (jw-retiring writer) t)
        (bt:condition-broadcast (jw-wake writer))))
    (unwind-protect
         (loop until (or (every (lambda (writer)
                                  (bt:with-lock-held ((jw-lock writer))
                                    (null (jw-thread writer))))
                                writers)
                         (> (get-internal-real-time) deadline))
               do (sleep 0.01))
      (dolist (writer writers)
        (bt:with-lock-held ((jw-lock writer))
          (setf (jw-retiring writer) nil))))))

(add-exit-hook 'journal-drain-all)

(defun journal-append (path run agent parent kind &rest fields)
  "Queue an entry of KIND with FIELDS for PATH and return its id. An entry that
would not read back is written as an :UNWRITABLE one, so it cannot end the read."
  (let* ((id (%vault-id))
         (entry (list* :kind kind :id id :at (%now-iso8601) :agent agent :run run
                       (append (and parent (list :parent parent)) fields))))
    (%journal-enqueue path (or (%journal-line entry)
                               (%journal-line (list :kind :unwritable :id id :at (getf entry :at)
                                                    :agent agent :run run :for kind))))
    id))

(defun journal-text (value cap)
  (%cut-text (if (stringp value)
                 value
                 (handler-case (json:stringify (untyped->json value))
                   (error () (prin1-to-string value))))
             cap))

(defun journal-event-fields (event cap)
  "EVENT's fields, its payloads cut to CAP characters."
  (loop for (key value) on event by #'cddr
        append (list key (if (member key +journal-capped-keys+) (journal-text value cap) value))))

;;; --- reading ---------------------------------------------------------------

(defun journal-entries (path &key agent run)
  "The entries of PATH, oldest first, of AGENT and of the run keyed RUN when given."
  (journal-drain path)
  (remove-if-not (lambda (entry)
                   (and (or (null agent) (eq agent (getf entry :agent)))
                        (or (null run) (equal run (getf entry :run)))))
                 (%read-log path)))

(defun journal-runs (path &key agent)
  "The keys of the runs PATH holds, oldest first."
  (remove-duplicates (remove nil (mapcar (lambda (entry) (getf entry :run))
                                         (journal-entries path :agent agent)))
                     :test #'equal :from-end t))

(defun %close-tail-calls (messages)
  "MESSAGES, with an :INTERRUPTED reply for each tool call of a last assistant
turn that has none, as the agent closes a turn it abandons."
  (let* ((index (position :assistant messages :key (lambda (m) (getf m :role)) :from-end t))
         (calls (and index (getf (nth index messages) :tool-calls)))
         (answered (and index (loop for m in (nthcdr (1+ index) messages)
                                    when (eq (getf m :role) :tool) collect (getf m :tool-call-id)))))
    (append messages
            (loop for call in calls
                  unless (member (getf call :id) answered :test #'equal)
                    collect (tool-message (getf call :id) (fail :interrupted))))))

(defun %fold-conversation (entries)
  (let ((messages '()))
    (dolist (entry entries (nreverse messages))
      (case (getf entry :kind)
        (:messages (when (getf entry :reset) (setf messages nil))
         (dolist (message (getf entry :messages)) (push message messages)))
        (:message (push (getf entry :message) messages))))))

(defun journal-conversation (path &key agent run)
  "The conversation AGENT held at the end of the run keyed RUN, or of its last
one, oldest first, and the number of assistant messages in it. A last turn left
without its tool replies is closed as an abandoned one is."
  (let* ((entries (journal-entries path :agent agent))
         (end (if run (position run entries :key (lambda (e) (getf e :run)) :test #'equal :from-end t)
                  (1- (length entries)))))
    (unless end (error "No run ~s in ~a." run path))
    (let ((messages (%close-tail-calls (%fold-conversation (subseq entries 0 (1+ end))))))
      (values messages (count :assistant messages :key (lambda (m) (getf m :role)))))))

;;; --- call records -----------------------------------------------------------

;;; Call records (~takeiteasy/miao#73): one record per tool call an agent
;;; dispatches, so a call's status outlives the process running it. They share the
;;; journal's file and its guarded read.
;;;
;;;   (:kind :call    :id "..." :at "iso" :agent name-or-nil :call-id "c1"
;;;    :name :tool-echo :arguments "<json>" :turn 2 :by owner)
;;;   (:kind :running :id "..." :at "iso")
;;;   (:kind :done    :id "..." :at "iso" :outcome :ok :content "<json>")
;;;
;;; A :CALL also carries :CUT T when its :ARGUMENTS were cut to the cap, and
;;; :RESUMES, the id of the call it runs again (JOURNAL-CALL-RESUME).
;;;
;;;   (:kind :input   :id "..." :at "iso" :agent name-or-nil :input-id "k"
;;;    :digest "md5hex" :by owner)
;;;
;;; An :INPUT is a :RUN a caller keyed with :INPUT-ID (~takeiteasy/miao#75); it
;;; is finished by a :DONE line like a call. CALL-ENTRIES leaves it out.
;;;
;;; :ID is the log's own, fresh per dispatch: a provider may reuse :CALL-ID on
;;; a later turn. CALL-ENTRIES folds the log into current state.

(defun journal-call-accept (path agent turn calls &key (cap *journal-max-content*))
  "Append a :CALL entry for each of CALLS, plists of :ID, :NAME and :ARGUMENTS
and, where a hook rewrote them, :RAW-ARGUMENTS, under one lock hold, and return
their log ids in order."
  (journal-drain path)
  (let ((owner (%vault-owner))
        (base (%vault-id)))
    (with-log-lock (path)
      (prog1 (loop for call in calls
                   for index from 0
                   for id = (format nil "~a-~d" base index)
                   do (let ((text (json:stringify (untyped->json (getf call :arguments)))))
                        (%append-log-locked
                         path (list* :kind :call :id id :at (%now-iso8601) :agent agent
                                     :call-id (getf call :id) :name (getf call :name)
                                     :arguments (%cut-text text cap)
                                     :turn turn :by owner
                                     (append (and cap (> (length text) cap) '(:cut t))
                                             (a:when-let ((raw (getf call :raw-arguments)))
                                               (list :raw-arguments
                                                     (%cut-text (json:stringify (untyped->json raw))
                                                                cap)))))))
                   collect id)))))

(defun journal-call-running (path ids)
  (dolist (id ids)
    (%journal-enqueue path (%journal-line (list :kind :running :id id :at (%now-iso8601))))))

(defun journal-call-done (path results)
  "Append a :DONE entry for each of RESULTS, lists of a log id, an outcome
(:OK, :ERROR, :DENIED, :INTERRUPTED or :ABANDONED), the text kept and, where a
hook rewrote the result, the raw text kept, under one lock hold. A call already
done keeps its first outcome."
  (when results
    (journal-drain path)
    (with-log-lock (path)
      (dolist (result results)
        (destructuring-bind (id outcome content &optional raw) result
          (%append-log-locked path (list* :kind :done :id id :at (%now-iso8601)
                                          :outcome outcome :content content
                                          (and raw (list :raw-content raw)))))))
    (%journal-enqueue path :check)))

(defun %done-by-id (log)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry log table)
      (when (eq (getf entry :kind) :done)
        (unless (nth-value 1 (gethash (getf entry :id) table))
          (setf (gethash (getf entry :id) table) entry))))))

(defun %live-status (log-entry id done running)
  "The status of LOG-ENTRY, a :CALL or :INPUT: its :DONE outcome, else :LOST
when its owner is gone, :RUNNING or :ACCEPTED."
  (let ((end (gethash id done)))
    (cond (end (getf end :outcome))
          ((not (%claim-live-p (getf log-entry :by))) :lost)
          ((gethash id running) :running)
          (t :accepted))))

(defun journal-input (path agent input-id digest &key (record t))
  "Record a :RUN keyed INPUT-ID at PATH and return its log id. When PATH holds
one under INPUT-ID already nothing is appended: the answer is its id,
:DUPLICATE, its status (its :DONE outcome, :LOST when its owner is gone, else
:RUNNING) and its digest. The check and the append share one lock hold. With
RECORD false a new input is not appended and the answer is nil."
  (journal-drain path)
  (with-log-lock (path)
    (let* ((log (%read-log path))
           (prior (find-if (lambda (e) (and (eq (getf e :kind) :input)
                                            (equal (getf e :input-id) input-id)))
                           log)))
      (if prior
          (let ((id (getf prior :id)))
            (values id :duplicate
                    (let ((status (%live-status prior id (%done-by-id log) (make-hash-table))))
                      (if (eq status :accepted) :running status))
                    (getf prior :digest)))
          (when record
            (let ((id (format nil "~a-in" (%vault-id))))
              (%append-log-locked path (list :kind :input :id id :at (%now-iso8601)
                                             :agent agent :input-id input-id
                                             :digest digest :by (%vault-owner)))
              id))))))

(defun input-entries (path)
  "Every :RUN keyed with an :INPUT-ID logged at PATH, oldest first: (:id :at
:agent :input-id :digest :status :done-at). :STATUS is the :OUTCOME of its
:DONE entry (a stop reason, :ERROR, :INTERRUPTED or :ABANDONED), :RUNNING, or
:LOST when the process running it is gone and nothing finished it."
  (journal-drain path)
  (let* ((log (%read-log path))
         (done (%done-by-id log)))
    (loop for entry in log
          when (eq (getf entry :kind) :input)
            collect (let* ((id (getf entry :id))
                           (status (%live-status entry id done (make-hash-table))))
                      (list :id id :at (getf entry :at) :agent (getf entry :agent)
                            :input-id (getf entry :input-id) :digest (getf entry :digest)
                            :status (if (eq status :accepted) :running status)
                            :done-at (getf (gethash id done) :at))))))

(defun %fold-calls (log)
  "Every :CALL in LOG, oldest first, as CALL-ENTRIES answers it."
  (let ((done (%done-by-id log))
        (running (make-hash-table :test 'equal))
        (resumed-by (make-hash-table :test 'equal)))
    (dolist (entry log)
      (case (getf entry :kind)
        (:running (setf (gethash (getf entry :id) running) t))
        (:call (a:when-let ((old (getf entry :resumes)))
                 (setf (gethash old resumed-by) (getf entry :id))))))
    (loop for entry in log
          when (eq (getf entry :kind) :call)
            collect (let* ((id (getf entry :id))
                           (end (gethash id done)))
                      (list :id id :at (getf entry :at) :agent (getf entry :agent)
                            :call-id (getf entry :call-id) :name (getf entry :name)
                            :arguments (getf entry :arguments) :turn (getf entry :turn)
                            :status (%live-status entry id done running)
                            :done-at (getf end :at)
                            :content (getf end :content)
                            :cut (getf entry :cut)
                            :resumes (getf entry :resumes)
                            :resumed-by (gethash id resumed-by))))))

(defun call-entries (path)
  "Every call logged at PATH, oldest first: (:id :at :agent :call-id :name
:arguments :turn :status :done-at :content :cut :resumes :resumed-by). :STATUS
is :ACCEPTED, :RUNNING, the :OUTCOME of its :DONE entry, or :LOST when the
process that accepted it is gone and nothing finished it. :CUT is true when
:ARGUMENTS were cut to fit the log. :RESUMES is the id of the call this one runs
again, :RESUMED-BY the id of the call that runs this one again."
  (journal-drain path)
  (%fold-calls (%read-log path)))

(defun %resume-refusal (entry check)
  "Why ENTRY, a call from %FOLD-CALLS or nil, cannot be resumed, or nil and
what CHECK, the caller's own test, answered with when it can."
  (cond ((null entry) "no such call")
        ((getf entry :resumed-by)
         (format nil "already resumed as ~a" (getf entry :resumed-by)))
        ((not (member (getf entry :status) '(:lost :abandoned :interrupted)))
         (format nil "the call is ~(~a~), not lost, abandoned or interrupted"
                 (getf entry :status)))
        ((getf entry :cut) "its arguments were cut when it was logged")
        (t (funcall check entry))))

(defun journal-call-resume (path ids agent turn check)
  "Log a new :CALL, resuming it, for each call in IDS that can be run again:
one that ended :LOST, :ABANDONED or :INTERRUPTED, whose arguments were not cut,
that no call resumes already, and that CHECK accepts. CHECK is given the call's
entry, see CALL-ENTRIES, and answers a reason to refuse it, or nil and a value
to pass on. The checks and the appends share one lock hold, so two resumes of
one call cannot both succeed. Answers the resumed, (old id, new id, entry, CHECK's
value), and the refused, (id reason), each in the order of IDS. CHECK runs
holding the lock, so it must not read PATH."
  (journal-drain path)
  (with-log-lock (path)
    (let ((calls (%fold-calls (%read-log path)))
          (base (%vault-id))
          (owner (%vault-owner))
          (index 0)
          (resumed '())
          (refused '()))
      (dolist (id (remove-duplicates ids :test #'equal :from-end t))
        (let ((entry (find id calls :key (lambda (call) (getf call :id)) :test #'equal)))
          (multiple-value-bind (reason value) (%resume-refusal entry check)
            (if reason
                (push (list id reason) refused)
                (let ((new (format nil "~a-~d" base (prog1 index (incf index)))))
                  (%append-log-locked
                   path (list :kind :call :id new :at (%now-iso8601) :agent agent
                              :call-id (getf entry :call-id) :name (getf entry :name)
                              :arguments (getf entry :arguments) :turn turn :by owner
                              :resumes id))
                  (push (list id new entry value) resumed))))))
      (values (nreverse resumed) (nreverse refused)))))

;;; --- compacting ------------------------------------------------------------

;;; TODO: the whole log is read and rewritten under its lock, so other processes
;;; wait on it; a segmented log if that is measured to stall them (#216).

(defun %expired-p (entry cutoff max-age)
  (let ((at (getf entry :at)))
    (or (zerop max-age) (not (stringp at)) (string< at cutoff))))

(defun %without-finished-calls (log cutoff max-age)
  "LOG less the calls finished before CUTOFF, their inputs, and the :RUNNING and
:DONE lines of both, and how many calls that dropped."
  (let ((expired (make-hash-table :test 'equal))
        (dropped 0))
    (maphash (lambda (id entry)
               (when (%expired-p entry cutoff max-age)
                 (setf (gethash id expired) t)))
             (%done-by-id log))
    (dolist (entry log)
      (when (and (eq (getf entry :kind) :call) (gethash (getf entry :id) expired))
        (incf dropped)))
    (values (remove-if (lambda (entry) (gethash (getf entry :id) expired)) log)
            dropped)))

(defun %conversation-segments (log)
  "A table of each conversation entry of LOG, a :MESSAGES, :MESSAGE or :EVENT, to
its agent, parent and segment: the stretch of one agent's entries that share a
run key. Restores outside a run, which have none, so stay apart from the runs
they separate."
  (let ((streams (make-hash-table :test 'equal))
        (segments (make-hash-table :test 'eq)))
    (dolist (entry log segments)
      (when (member (getf entry :kind) '(:messages :message :event))
        (let* ((stream (list (getf entry :agent) (getf entry :parent)))
               (state (gethash stream streams (cons :none 0))))
          (unless (equal (car state) (getf entry :run))
            (setf state (cons (getf entry :run) (1+ (cdr state)))))
          (setf (gethash stream streams) state
                (gethash entry segments) (append stream (list (cdr state)))))))))

(defun %with-old-conversations-folded (log cutoff max-age)
  "LOG with each segment's conversation entries and events older than CUTOFF
replaced by one :MESSAGES entry, at the place of the last of them: a :RESET only
when they held one, so a run that continued another adds to it still."
  (let ((segments (%conversation-segments log))
        (old (make-hash-table :test 'eq))
        (groups (make-hash-table :test 'equal))
        (result '()))
    (dolist (entry log)
      (when (and (gethash entry segments) (%expired-p entry cutoff max-age))
        (setf (gethash entry old) t)
        (push entry (gethash (gethash entry segments) groups))))
    (dolist (entry log (nreverse result))
      (let ((group (gethash (gethash entry segments) groups)))
        (cond ((not (gethash entry old)) (push entry result))
              ((and (eq entry (first group))
                    (find-if (lambda (e) (member (getf e :kind) '(:messages :message))) group))
               (push (list* :kind :messages :id (getf entry :id) :at (getf entry :at)
                            :agent (getf entry :agent) :run (getf entry :run)
                            (append (and (getf entry :parent) (list :parent (getf entry :parent)))
                                    (list :reset (and (find-if (lambda (e) (and (eq (getf e :kind) :messages)
                                                                                (getf e :reset)))
                                                               group)
                                                      t)
                                          :messages (%fold-conversation (reverse group)))))
                     result)))))))

(defun journal-compact (path &key (max-age *journal-max-age*))
  "Rewrite PATH without the calls finished more than MAX-AGE seconds ago (0 drops
every finished one), their inputs and their :RUNNING and :DONE lines, and with
each run's conversation entries older than that folded into one :MESSAGES entry,
the events dropped. Calls not finished are kept. Returns the calls dropped
and kept, or nil, leaving the file untouched, when the log has a malformed entry."
  (journal-drain path)
  (with-log-lock (path)
    (multiple-value-bind (log clean) (%read-log path)
      (when clean
        (let ((cutoff (%now-iso8601 (- (get-universal-time) max-age))))
          (multiple-value-bind (live dropped) (%without-finished-calls log cutoff max-age)
            (let ((survivors (%with-old-conversations-folded live cutoff max-age)))
              (unless (equal survivors log) (%write-log path survivors))
              (values dropped (count :call survivors :key (lambda (e) (getf e :kind)))))))))))
