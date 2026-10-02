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

;;; --- compacting ------------------------------------------------------------

;;; TODO: the whole log is read and rewritten under its lock, and an agent's old
;;; conversation is folded as one :MESSAGES entry, so a run's key is lost from it;
;;; a segmented log if it grows large (#216).

(defun journal-compact (path &key (max-age *journal-max-age*))
  "Rewrite PATH with each agent's entries older than MAX-AGE seconds (0 folds
them all) that hold a conversation or an event folded into one :RESET :MESSAGES
entry, and without the events. Returns the entries dropped and kept, or nil,
leaving the file untouched, when the log has a malformed entry."
  (with-log-lock (path)
    (multiple-value-bind (log clean) (%read-log path)
      (when clean
        (let* ((cutoff (%now-iso8601 (- (get-universal-time) max-age)))
               (old (make-hash-table :test 'eq))
               (folded (make-hash-table :test 'equal))
               (survivors '())
               (dropped 0))
          (dolist (entry log)
            (when (and (member (getf entry :kind) '(:messages :message :event))
                       (let ((at (getf entry :at)))
                         (or (zerop max-age) (not (stringp at)) (string< at cutoff))))
              (setf (gethash entry old) t)
              (push entry (gethash (cons (getf entry :agent) (getf entry :parent)) folded))))
          (let ((placed (make-hash-table :test 'equal)))
            (dolist (entry log)
              (let ((key (cons (getf entry :agent) (getf entry :parent))))
                (cond ((not (gethash entry old)) (push entry survivors))
                      (t (incf dropped)
                         (unless (gethash key placed)
                           (setf (gethash key placed) t)
                           (let* ((group (reverse (gethash key folded)))
                                  (last (car (last group))))
                             (push (list* :kind :messages :id (getf last :id) :at (getf last :at)
                                          :agent (getf last :agent) :run (getf last :run)
                                          (append (and (getf last :parent) (list :parent (getf last :parent)))
                                                  (list :reset t :messages (%fold-conversation group))))
                                   survivors))
                           (decf dropped)))))))
          (setf survivors (nreverse survivors))
          (unless (equal survivors log) (%write-log path survivors))
          (values dropped (length survivors)))))))
