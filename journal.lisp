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

(defun journal-append (path run agent parent kind &rest fields)
  "Append an entry of KIND with FIELDS to PATH and return its id. An entry that
would not read back is written as an :UNWRITABLE one, so it cannot end the read."
  (let* ((id (%vault-id))
         (entry (list* :kind kind :id id :at (%now-iso8601) :agent agent :run run
                       (append (and parent (list :parent parent)) fields))))
    (%append-log path (if (%readable-p entry)
                          entry
                          (list :kind :unwritable :id id :at (getf entry :at) :agent agent
                                :run run :for kind)))
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
  (when ids
    (with-log-lock (path)
      (dolist (id ids)
        (%append-log-locked path (list :kind :running :id id :at (%now-iso8601)))))))

(defun journal-call-done (path results)
  "Append a :DONE entry for each of RESULTS, lists of a log id, an outcome
(:OK, :ERROR, :DENIED, :INTERRUPTED or :ABANDONED), the text kept and, where a
hook rewrote the result, the raw text kept, under one lock hold. A call already
done keeps its first outcome."
  (when results
    (with-log-lock (path)
      (dolist (result results)
        (destructuring-bind (id outcome content &optional raw) result
          (%append-log-locked path (list* :kind :done :id id :at (%now-iso8601)
                                          :outcome outcome :content content
                                          (and raw (list :raw-content raw)))))))
    (%maybe-compact path *journal-compact-size* #'journal-compact)))

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
value), and the refused, (id reason), each in the order of IDS."
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

;;; TODO: the whole log is read and rewritten under its lock, and an agent's old
;;; conversation is folded as one :MESSAGES entry, so a run's key is lost from it;
;;; a segmented log if it grows large (#216).

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

(defun %with-old-conversations-folded (log cutoff max-age)
  "LOG with each agent's conversation entries and events older than CUTOFF
replaced by one :RESET :MESSAGES entry, at the place of its first."
  (let ((old (make-hash-table :test 'eq))
        (groups (make-hash-table :test 'equal))
        (placed (make-hash-table :test 'equal))
        (result '()))
    (dolist (entry log)
      (when (and (member (getf entry :kind) '(:messages :message :event))
                 (%expired-p entry cutoff max-age))
        (setf (gethash entry old) t)
        (push entry (gethash (cons (getf entry :agent) (getf entry :parent)) groups))))
    (dolist (entry log (nreverse result))
      (let ((key (cons (getf entry :agent) (getf entry :parent))))
        (cond ((not (gethash entry old)) (push entry result))
              ((not (gethash key placed))
               (setf (gethash key placed) t)
               (let* ((group (reverse (gethash key groups)))
                      (last (car (last group))))
                 (push (list* :kind :messages :id (getf last :id) :at (getf last :at)
                              :agent (getf last :agent) :run (getf last :run)
                              (append (and (getf last :parent) (list :parent (getf last :parent)))
                                      (list :reset t :messages (%fold-conversation group))))
                       result))))))))

(defun journal-compact (path &key (max-age *journal-max-age*))
  "Rewrite PATH without the calls finished more than MAX-AGE seconds ago (0 drops
every finished one), their inputs and their :RUNNING and :DONE lines, and with
each agent's conversation entries older than that folded into one :RESET :MESSAGES
entry, the events dropped. Calls not finished are kept. Returns the calls dropped
and kept, or nil, leaving the file untouched, when the log has a malformed entry."
  (with-log-lock (path)
    (multiple-value-bind (log clean) (%read-log path)
      (when clean
        (let ((cutoff (%now-iso8601 (- (get-universal-time) max-age))))
          (multiple-value-bind (live dropped) (%without-finished-calls log cutoff max-age)
            (let ((survivors (%with-old-conversations-folded live cutoff max-age)))
              (unless (equal survivors log) (%write-log path survivors))
              (values dropped (count :call survivors :key (lambda (e) (getf e :kind)))))))))))
