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

(defstruct (renderer (:constructor make-renderer (out &optional on-done)))
  out on-done (progress (make-hash-table)) (start 0) (last-status :idle))

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
          (write-string *prompt* out)
          (a:when-let ((on-done (renderer-on-done renderer)))
            (funcall on-done)))
        (setf (renderer-last-status renderer) status))
      (finish-output out))))

;;; --- saving ---------------------------------------------------------------

;;; A run's end only asks the saver for a save: checkpointing from the thread
;;; that delivers events would ask the agent to snapshot itself while it waits
;;; on that thread.

(defparameter *label-length* 60)

(defstruct (saver (:constructor make-saver (context directory err)))
  context directory err
  (lock (bt:make-lock :name "miao chat saver"))
  (wake (bt:make-condition-variable))
  pending stopping thread)

(defun conversation (&optional (agent :chat))
  (getf (m:call (m:lookup agent) '(:snapshot)) :messages))

(defun first-user-line (messages)
  (a:when-let ((message (find :user messages :key (lambda (message) (getf message :role)))))
    (let* ((text (miao:content-text (getf message :content)))
           (line (subseq text 0 (position #\Newline text))))
      (if (> (length line) *label-length*)
          (format nil "~a..." (subseq line 0 *label-length*))
          line))))

(defun save-chat (saver)
  (handler-case
      (let ((label (first-user-line (conversation))))
        (when label
          (miao:checkpoint (saver-context saver) :dir (saver-directory saver) :keep 1 :label label)))
    (error (e)
      (format (saver-err saver) "~&miao: could not save the chat: ~a~%" e))))

(defun request-save (saver)
  (bt:with-lock-held ((saver-lock saver))
    (setf (saver-pending saver) t)
    (bt:condition-notify (saver-wake saver))))

(defun saver-loop (saver)
  (loop
    (let ((stop nil))
      (bt:with-lock-held ((saver-lock saver))
        (loop until (or (saver-pending saver) (saver-stopping saver))
              do (bt:condition-wait (saver-wake saver) (saver-lock saver)))
        (setf stop (saver-stopping saver)
              (saver-pending saver) nil))
      (when stop (return))
      (save-chat saver))))

(defun start-saver (saver)
  (setf (saver-thread saver) (bt:make-thread (lambda () (saver-loop saver)) :name "miao chat saver")))

(defun stop-saver (saver)
  "Let a save under way finish, then save once more so a run cut short is kept."
  (bt:with-lock-held ((saver-lock saver))
    (setf (saver-stopping saver) t)
    (bt:condition-notify (saver-wake saver)))
  (bt:join-thread (saver-thread saver))
  (save-chat saver))

;;; --- sessions -------------------------------------------------------------

(defun session-id ()
  (miao:generation-id))

(defun chats-directory (home)
  (merge-pathnames "chats/" home))

(defun session-ids (home)
  "The saved sessions' ids, newest first."
  (sort (mapcar (lambda (directory) (car (last (pathname-directory directory))))
                (uiop:subdirectories (chats-directory home)))
        #'string>))

(defun session-directory (home id)
  (merge-pathnames (format nil "~a/" id) (chats-directory home)))

(defun resolve-session (home resume)
  "The id RESUME, :LATEST or an id, names, or a usage error."
  (let ((ids (session-ids home)))
    (cond ((null ids) (usage-error "no saved chats"))
          ((eq resume :latest) (first ids))
          ((member resume ids :test #'string=) resume)
          (t (usage-error "no saved chat ~a" resume)))))

;;; --- listing --------------------------------------------------------------

(defun list-chats (home out)
  "One line per saved chat, newest first: id, when it was last saved, its first line."
  (dolist (id (session-ids home))
    (let ((generation (first (miao:generations :dir (session-directory home id)))))
      (when generation
        (format out "~a  ~a  ~a~%" id (getf generation :created) (getf generation :label))))))

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
  "Mount the agent for a new chat, or, for --resume, bring the saved one back.
Answers the session's id."
  (if (getf options :resume)
      (let* ((id (resolve-session home (getf options :resume)))
             (generation (first (miao:generations :dir (session-directory home id))))
             (outcome (and generation (miao:rollback context (getf generation :path)))))
        (unless (and outcome (member :chat (getf (second outcome) :restored)))
          (error "could not resume ~a: ~a" id
                 (or (second (assoc :chat (getf (second outcome) :unremounted))) "no saved conversation")))
        id)
      (multiple-value-bind (provider-name tools system) (prepare options context)
        (apply #'m:mount context 'miao:agent :name :chat :model provider-name :system system
               (agent-options options tools))
        (session-id))))

(defun chat (options context in out err home)
  "Chat with one agent until IN, a stream or a function answering a line or
nil, ends, and return an exit code. Ctrl-C cancels a run, or leaves the chat
when none is under way. Each run is saved under HOME, and options :resume
brings a saved chat back."
  (let ((saver nil))
    (unwind-protect
         (let* ((id (mount-chat options context home))
                (renderer (make-renderer out))
                (client nil))
           (setf saver (make-saver context (session-directory home id) err))
           (setf (renderer-on-done renderer) (lambda () (request-save saver)))
           (replay (conversation) out)
           (start-saver saver)
           (setf client (ui:attach :chat :on-change (lambda (state) (render renderer state))))
           (bt:with-lock-held (*output-lock*)
             (write-string *prompt* out)
             (finish-output out))
           (block session
             (handler-bind ((sb-sys:interactive-interrupt
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
      (when (and saver (saver-thread saver))
        (stop-saver saver))
      (when (m:lookup :chat)
        (m:unmount context :chat)))))
