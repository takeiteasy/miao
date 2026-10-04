(in-package #:miao/cli)

;;; `miao chat`: a line REPL over one agent, drawn from miao/ui's state.
;;; docs/chat.md.

(defvar *output-lock* (bt:make-lock :name "miao chat output")
  "Held while the chat writes, so what it printed can be read without a tear.")

(defparameter *abbreviate-at* 200)

(defparameter *prompt* "> ")

;;; --- drawing --------------------------------------------------------------

;;; The renderer prints only what a state adds to what it printed before. PROGRESS
;;; holds, per transcript index, the characters of a text entry printed, the phase
;;; of a call (1 announced, 2 answered), or :FINISHED. START is the first index that
;;; is not.

(defstruct (renderer (:constructor make-renderer (out)))
  out (progress (make-hash-table)) (start 0) (last-status :idle))

(defun abbreviate (object)
  (let ((text (let ((*print-pretty* nil)) (prin1-to-string object))))
    (if (> (length text) *abbreviate-at*)
        (format nil "~a..." (subseq text 0 *abbreviate-at*))
        text)))

(defun render-entry (renderer entry index superseded)
  "Print what ENTRY, at INDEX in the transcript, has added. SUPERSEDED is true
when a later entry exists, so it can no longer grow."
  (let ((out (renderer-out renderer))
        (progress (renderer-progress renderer)))
    (ecase (ui:entry-kind entry)
      (:text
       (let* ((text (ui:entry-text entry))
              (printed (gethash index progress 0)))
         (when (> (length text) printed)
           (write-string text out :start printed)
           (setf (gethash index progress) (length text)))
         (when superseded
           (setf (gethash index progress) :finished))))
      (:call
       (let ((phase (gethash index progress 0))
             (status (ui:entry-status entry)))
         (when (and (< phase 1) (member status '(:running :detached :done)))
           (format out "~&[~(~a~) ~a]~%" (ui:entry-name entry) (abbreviate (ui:entry-arguments entry)))
           (setf phase 1))
         (when (and (< phase 2) (eq status :done))
           (format out "~&[result ~a]~%" (abbreviate (ui:entry-result entry)))
           (setf phase 2))
         (setf (gethash index progress) (if (or (= phase 2) (eq status :abandoned)) :finished phase))))
      (:notice
       (format out "~&[~(~a~) ~a]~%" (ui:entry-name entry) (abbreviate (ui:entry-result entry)))
       (setf (gethash index progress) :finished))
      ((:message :steer)
       (setf (gethash index progress) :finished)))))

(defun render (renderer state)
  "Print what STATE adds to what RENDERER has printed, and a prompt when a run
has just ended."
  (bt:with-lock-held (*output-lock*)
    (let* ((out (renderer-out renderer))
           (entries (coerce (ui:state-transcript state) 'vector))
           (count (length entries))
           (progress (renderer-progress renderer)))
      (loop for index from (renderer-start renderer) below count
            unless (eq :finished (gethash index progress))
              do (render-entry renderer (aref entries index) index (< index (1- count))))
      (setf (renderer-start renderer)
            (or (loop for index from (renderer-start renderer) below count
                      unless (eq :finished (gethash index progress)) return index)
                count))
      (let ((status (ui:state-status state)))
        (when (and (eq status :done) (not (eq (renderer-last-status renderer) :done)))
          (fresh-line out)
          (unless (eq :stop (ui:state-reason state))
            (format out "[run ended: ~(~a~)]~%" (ui:state-reason state)))
          (write-string *prompt* out))
        (setf (renderer-last-status renderer) status))
      (finish-output out))))

;;; --- the conversation -----------------------------------------------------

(defparameter *label-length* 60)

(defun conversation (&optional (agent :chat))
  (getf (m:call (m:lookup agent) '(:snapshot)) :messages))

(defun first-user-line (messages)
  (a:when-let ((message (find :user messages :key (lambda (message) (getf message :role)))))
    (let* ((text (miao:content-text (getf message :content)))
           (line (subseq text 0 (position #\Newline text))))
      (if (> (length line) *label-length*)
          (format nil "~a..." (subseq line 0 *label-length*))
          line))))

;;; --- sessions -------------------------------------------------------------

(defun session-id ()
  (miao:generation-id))

(defun chats-directory (home)
  (merge-pathnames "chats/" home))

(defun session-directory (home id)
  (merge-pathnames (format nil "~a/" id) (chats-directory home)))

(defun session-journal (directory)
  (merge-pathnames "journal.log" directory))

(defun session-ids (home)
  "The saved sessions' ids, newest first: those that ran something."
  (sort (loop for directory in (uiop:subdirectories (chats-directory home))
              when (probe-file (session-journal directory))
                collect (car (last (pathname-directory directory))))
        #'string>))

;;; A session is a folder holding the chat's journal and the options its agent
;;; was mounted with, which a resume mounts it with again.

(defun write-session-options (directory options system)
  (ensure-directories-exist directory)
  (with-open-file (out (merge-pathnames "options.sexp" directory)
                       :direction :output :if-exists :supersede)
    (let ((*package* (find-package "KEYWORD")))
      (prin1 (list :model (getf options :model) :tools (getf options :tools)
                   :max-turns (getf options :max-turns) :system system)
             out))))

(defun read-session-options (directory)
  (let ((path (merge-pathnames "options.sexp" directory)))
    (with-open-file (in path)
      (let ((*read-eval* nil) (*package* (find-package "KEYWORD")))
        (read in)))))

(defun resolve-session (home resume)
  "The id RESUME, :LATEST or an id, names, or a usage error."
  (let ((ids (session-ids home)))
    (cond ((null ids) (usage-error "no saved chats"))
          ((eq resume :latest) (first ids))
          ((member resume ids :test #'string=) resume)
          (t (usage-error "no saved chat ~a" resume)))))

;;; --- listing --------------------------------------------------------------

(defun list-chats (home out)
  "One line per saved chat, newest first: id, when it was last written, its first line."
  (dolist (id (session-ids home))
    (let* ((path (session-journal (session-directory home id)))
           (label (first-user-line (miao:journal-conversation path :agent :chat))))
      (when label
        (format out "~a  ~a  ~a~%" id (miao::%now-iso8601 (file-write-date path)) label)))))

;;; --- replaying ------------------------------------------------------------

(defun replay (messages out)
  "Print MESSAGES, a saved conversation, as the chat would have drawn it."
  (dolist (message messages)
    (let ((text (miao:content-text (getf message :content))))
      (ecase (getf message :role)
        (:system)
        (:user (format out "~a~a~%" *prompt* text))
        (:assistant
         (when (plusp (length text)) (format out "~a~%" text))
         (dolist (call (getf message :tool-calls))
           (format out "[~(~a~) ~a]~%" (getf call :name) (abbreviate (getf call :arguments)))))
        (:tool (format out "[result ~a]~%" (abbreviate text))))))
  (finish-output out))

;;; --- the session ----------------------------------------------------------

(defun already-running-p (answer)
  (equal answer '(:error (:bad-request "agent is already running"))))

(defun submit (client line)
  "Send LINE as the next prompt, or, when the agent is mid-run, as a steer. The
agent decides which: the state a client holds trails it. Answers what the
agent answered."
  (let ((answer (ui:continue-run client line)))
    (if (already-running-p answer)
        (ui:steer client line)
        answer)))

(defun handle-interrupt (client)
  "Ctrl-C: cancel the run under way and answer :CANCELLED, or :EXIT when idle."
  (cond ((eq :running (ui:state-status (ui:client-state client)))
         (ignore-errors (ui:cancel client))
         :cancelled)
        (t :exit)))

(defun next-line (in)
  (if (functionp in) (funcall in) (read-line in nil)))

(defun mount-chat (options context home)
  "Mount the agent for a new chat, or, for --resume, bring the saved one back from
its journal. Answers the session's directory."
  (if (getf options :resume)
      (let* ((directory (session-directory home (resolve-session home (getf options :resume))))
             (saved (read-session-options directory))
             (path (session-journal directory)))
        (multiple-value-bind (provider-name tools) (prepare saved context)
          (apply #'m:mount context 'miao:agent :name :chat :model provider-name
                 :system (getf saved :system) :journal path (agent-options saved tools)))
        (multiple-value-bind (messages turns) (miao:journal-conversation path :agent :chat)
          (m:call (m:lookup :chat) (list :restore (list :messages messages :turns turns))))
        directory)
      (multiple-value-bind (provider-name tools system) (prepare options context)
        (let ((directory (session-directory home (session-id))))
          (write-session-options directory options system)
          (apply #'m:mount context 'miao:agent :name :chat :model provider-name :system system
                 :journal (session-journal directory) (agent-options options tools))
          directory))))

(defun chat (options context in out err home)
  "Chat with one agent until IN, a stream or a function answering a line or
nil, ends, and return an exit code. Ctrl-C cancels a run, or leaves the chat
when none is under way. Each run is journaled under HOME, and options :resume
brings a saved chat back."
  (let ((directory nil))
    (unwind-protect
         (let* ((renderer (make-renderer out))
                (client nil))
           (setf directory (mount-chat options context home))
           (replay (conversation) out)
           (setf client (ui:attach :chat :on-change (lambda (state) (render renderer state))))
           (bt:with-lock-held (*output-lock*)
             (write-string *prompt* out)
             (finish-output out))
           (block session
             (handler-bind ((miao:interactive-interrupt
                              (lambda (condition)
                                (declare (ignore condition))
                                (if (eq :cancelled (handle-interrupt client))
                                    (a:when-let ((restart (find-restart 'continue)))
                                      (invoke-restart restart))
                                    (return-from session 0)))))
               (loop
                 (let ((line (next-line in)))
                   (cond ((null line) (return-from session 0))
                         ((string= "" (string-trim '(#\Space #\Tab) line)))
                         (t (submit client line))))))))
      (when (m:lookup :chat)
        (m:unmount context :chat))
      (when (and directory (not (probe-file (session-journal directory))))
        (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))))
