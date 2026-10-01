(in-package #:miao)

;;; The call log (~takeiteasy/miao#73): an append-only s-expression log with
;;; one record per tool call an agent dispatches, so a call's status outlives
;;; the process running it. Read back the same guarded way the vault is
;;; (checkpoint.lisp's %APPEND-LOG/%READ-LOG).
;;;
;;;   (:kind :call    :id "..." :at "iso" :agent name-or-nil :call-id "c1"
;;;    :name :tool-echo :arguments "<json>" :turn 2 :by owner)
;;;   (:kind :running :id "..." :at "iso")
;;;   (:kind :done    :id "..." :at "iso" :outcome :ok :content "<json>")
;;;
;;; A :CALL also carries :CUT T when its :ARGUMENTS were cut to the cap, and
;;; :RESUMES, the id of the call it runs again (CALL-LOG-RESUME).
;;;
;;;   (:kind :input   :id "..." :at "iso" :agent name-or-nil :input-id "k"
;;;    :digest "md5hex" :by owner)
;;;
;;; An :INPUT is a :RUN a caller keyed with :INPUT-ID (~takeiteasy/miao#75); it
;;; is finished by a :DONE line like a call. CALL-ENTRIES leaves it out.
;;;
;;; :ID is the log's own, fresh per dispatch: a provider may reuse :CALL-ID on
;;; a later turn. CALL-ENTRIES folds the log into current state.

(defvar *call-log-max-age* (* 7 24 60 60)
  "Seconds a finished call is kept before CALL-LOG-COMPACT drops it.")

(defvar *call-log-compact-size* (* 1024 1024)
  "Log size in bytes past which an append compacts the log.")

(defvar *call-log-max-content* (* 16 1024)
  "Characters of a call's arguments or result kept when the agent has no
:MAX-TOOL-RESULT.")

(defvar *call-log* nil
  "Default log path: ~/.miao/calls.log, resolved lazily.")

(defun %call-log-path (spec)
  "SPEC, an agent's :CALL-LOG mount option, as a log path: NIL means off, T the
default, anything else is used as given."
  (cond ((eq spec t)
         (or *call-log*
             (setf *call-log* (merge-pathnames ".miao/calls.log" (user-homedir-pathname)))))
        (t spec)))

(defun call-log-accept (path agent turn calls &key (cap *call-log-max-content*))
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

(defun call-log-running (path ids)
  (when ids
    (with-log-lock (path)
      (dolist (id ids)
        (%append-log-locked path (list :kind :running :id id :at (%now-iso8601)))))))

(defun call-log-done (path results)
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
    (%maybe-compact path *call-log-compact-size* #'call-log-compact)))

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

(defun call-log-input (path agent input-id digest &key (record t))
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

(defun call-log-resume (path ids agent turn check)
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

(defun call-log-compact (path &key (max-age *call-log-max-age*))
  "Rewrite PATH without the calls finished more than MAX-AGE seconds ago (0
drops every finished one) and their :RUNNING and :DONE lines. Calls not
finished are kept. Returns the calls dropped and kept, or nil, leaving the
file untouched, when the log has a malformed entry."
  (with-log-lock (path)
    (multiple-value-bind (log clean) (%read-log path)
      (when clean
        (let ((cutoff (%now-iso8601 (- (get-universal-time) max-age)))
              (expired (make-hash-table :test 'equal))
              (dropped 0))
          (maphash (lambda (id entry)
                     (let ((at (getf entry :at)))
                       (when (or (zerop max-age) (not (stringp at)) (string< at cutoff))
                         (setf (gethash id expired) t))))
                   (%done-by-id log))
          (let ((survivors (remove-if (lambda (entry) (gethash (getf entry :id) expired)) log)))
            (dolist (entry log)
              (when (and (eq (getf entry :kind) :call) (gethash (getf entry :id) expired))
                (incf dropped)))
            (unless (equal survivors log) (%write-log path survivors))
            (values dropped (count :call survivors :key (lambda (e) (getf e :kind))))))))))
