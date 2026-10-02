(in-package #:miao/tests)
(in-suite :miao)

;;; `miao chat` (~takeiteasy/miao#99): the session against the echo provider,
;;; and its pieces against a stalling backend.

(defun chat-session (lines &rest args)
  "ARGS after `chat`, with LINES typed one at a time, each once the last run has
drawn its prompt, against *HOME* or a home of its own. Answers the exit code,
everything printed, from the agent as the input ended its conversation's roles,
and stderr."
  (let* ((out (make-string-output-stream))
         (text "")
         (roles nil)
         (mark 0)
         (reader (lambda ()
                   (is-true (eventually
                             (lambda ()
                               (setf text (concatenate
                                           'string text
                                           (bt:with-lock-held (miao/cli::*output-lock*)
                                             (get-output-stream-string out))))
                               (and (> (length text) mark)
                                    (alexandria:ends-with-subseq "> " text)))
                             5))
                   (if lines
                       (let ((line (pop lines)))
                         (unless (string= "" (string-trim " " line))
                           (setf mark (length text)))
                         line)
                       (progn
                         (setf roles (mapcar (lambda (message) (getf message :role))
                                             (getf (m:call (m:lookup :chat) '(:snapshot))
                                                   :messages)))
                         nil))))
         (err (make-string-output-stream))
         (code (flet ((in-home (home)
                        (miao/cli:main (cons "chat" args) :context *protocol-context* :home home
                                                          :in reader :out out :err err)))
                 (if *home*
                     (in-home *home*)
                     (with-home () (in-home *home*))))))
    (values code text roles (get-output-stream-string err))))

(defun saved-chats ()
  (miao/cli::session-ids *home*))

(defun saved-conversation (id)
  "The roles of the conversation the journal of session ID holds."
  (mapcar (lambda (message) (getf message :role))
          (miao:journal-conversation (miao/cli::session-journal (miao/cli::session-directory *home* id))
                                     :agent :chat)))

(test chat-answers-each-line-and-continues-the-conversation
  (with-protocol
    (multiple-value-bind (code text roles)
        (chat-session '("hello" "again") "--model" "test-echo:any")
      (is (= 0 code))
      (is (search "hello!" text))
      (is (search "again!" text))
      (is (equal '(:system :user :assistant :user :assistant) roles)))))

(test chat-ignores-blank-lines-and-ends-at-once-on-no-input
  (with-protocol
    (with-home ()
      (multiple-value-bind (code text roles err)
          (chat-session '("" "   ") "--model" "test-echo:any")
        (is (= 0 code) "~a" err)
        (is (equal "> " text))
        (is (null roles))
        (is (null (saved-chats)) "a chat that ran nothing saves nothing")))))

(test chat-unmounts-its-agent
  (with-protocol
    (chat-session '("hi") "--model" "test-echo:any")
    (is (null (m:lookup :chat)))))

(test chat-takes-no-prompt-and-shares-run-s-options
  (with-protocol
    (dolist (args '(("chat" "hello") ("chat" "--model" "nobody:x") ("chat" "--max-turns" "0")
                    ("chat" "--system-replace")))
      (multiple-value-bind (code out err) (apply #'cli args)
        (is (= 2 code) "~s exited ~a" args code)
        (is (equal "" out))
        (is (search "miao chat" err))))
    (is (= 3 (getf (miao/cli::parse-args '("--max-turns" "3") :takes-prompt nil) :max-turns)))))

;;; --- drawing --------------------------------------------------------------

(defun rendered (&rest event-lists)
  "What a renderer prints as each of EVENT-LISTS, in turn, is folded into a state."
  (let ((out (make-string-output-stream))
        (renderer (miao/cli::make-renderer nil))
        (state (ui:make-state))
        (printed '()))
    (setf (miao/cli::renderer-out renderer) out)
    (dolist (events event-lists)
      (setf state (ui:fold-events events state))
      (miao/cli::render renderer state)
      (push (get-output-stream-string out) printed))
    (nreverse printed)))

(test the-renderer-prints-only-what-a-state-adds
  (destructuring-bind (one two three four)
      (rendered (list (ev :run-start :messages '((:role :user :content "hi")) :continue nil)
                      (ev :turn :turn 1)
                      (ev :text-delta :text "hel"))
                (list (ev :text-delta :text "lo"))
                (list (ev :tool-call :id "c1" :name :shell :arguments '(:cmd "ls"))
                      (ev :tool-result :id "c1" :result '(:ok "x")))
                (list (ev :text-delta :text "done")
                      (ev :run-done :reason :stop)))
    (is (equal "hel" one))
    (is (equal "lo" two))
    (is (equal (format nil "~&[shell (:CMD \"ls\")]~%[result (:OK \"x\")]~%") (string-left-trim '(#\Newline) three)))
    (is (equal (format nil "done~%> ") four))))

(test a-run-that-does-not-stop-says-why
  (destructuring-bind (text)
      (rendered (list (ev :turn :turn 1) (ev :text-delta :text "x") (ev :run-done :reason :max-turns)))
    (is (search "[run ended: max-turns]" text))
    (is (equal "> " (subseq text (- (length text) 2))))))

(test a-long-result-is-abbreviated
  (let ((text (miao/cli::abbreviate (make-string 500 :initial-element #\a))))
    (is (< (length text) 300))
    (is (search "..." text))))

;;; --- the session against a run under way -----------------------------------

(test a-line-typed-mid-run-is-a-steer
  (let ((release (list nil)))
    (with-agent ((interruptible-backend release))
      (unwind-protect
           (progn
             (mount-assistant)
             (let ((client (ui:attach :assistant)))
               (is (eq :ok (miao/cli::submit client "go")))
               (is-true (eventually (lambda ()
                                      (entries-of :text (ui:state-root (ui:client-state client))))
                                    5))
               (is (eq :ok (miao/cli::submit client "shorter"))
                   "the second line is a steer, not a refused run")
               (is (eq :ok (miao/cli::submit client "shorter still")))))
        (setf (car release) t)))))

(test ctrl-c-cancels-a-run-and-leaves-an-idle-chat
  (let ((release (list nil)))
    (with-agent ((interruptible-backend release))
      (unwind-protect
           (progn
             (mount-assistant)
             (let ((client (ui:attach :assistant)))
               (is (eq :exit (miao/cli::handle-interrupt client)))
               (miao/cli::submit client "go")
               (is-true (eventually (lambda ()
                                      (entries-of :text (ui:state-root (ui:client-state client))))
                                    5))
               (is (eq :cancelled (miao/cli::handle-interrupt client)))
               (wait-for-status client :done)
               (is (eq :cancelled (ui:state-reason (ui:client-state client))))
               (is (eq :exit (miao/cli::handle-interrupt client)))))
        (setf (car release) t)))))

;;; --- saving and resuming (#100) --------------------------------------------

(test a-chat-is-saved-and-listed
  (with-protocol
    (with-home ()
      (chat-session '("hello there" "again") "--model" "test-echo:any")
      (is (= 1 (length (saved-chats))))
      (is (equal '(:system :user :assistant :user :assistant)
                 (saved-conversation (first (saved-chats)))))
      (let ((directory (miao/cli::session-directory *home* (first (saved-chats)))))
        (is-true (probe-file (miao/cli::session-journal directory)))
        (is (equal "test-echo:any" (getf (miao/cli::read-session-options directory) :model))))
      (multiple-value-bind (code out) (cli "chats")
        (is (= 0 code))
        (is (search (first (saved-chats)) out))
        (is (search "hello there" out))))))

(test a-chat-that-ran-nothing-leaves-no-session-folder
  (with-protocol
    (with-home ()
      (chat-session '() "--model" "test-echo:any")
      (is (null (uiop:subdirectories (miao/cli::chats-directory *home*)))))))

(test chats-lists-nothing-for-an-empty-home-and-takes-no-arguments
  (with-protocol
    (multiple-value-bind (code out) (cli "chats")
      (is (= 0 code))
      (is (equal "" out)))
    (is (= 2 (cli "chats" "extra")))))

(test chats-lists-the-newest-first
  (with-protocol
    (with-home ()
      (chat-session '("first") "--model" "test-echo:any")
      (chat-session '("second") "--model" "test-echo:any")
      (let ((out (nth-value 1 (cli "chats"))))
        (is (< (search "second" out) (search "first" out)))))))

(test resume-carries-on-from-the-saved-conversation
  (with-protocol
    (with-home ()
      (chat-session '("hello") "--model" "test-echo:any")
      (let ((id (first (saved-chats))))
        (multiple-value-bind (code text roles err)
            (chat-session '("again") "--resume")
          (is (= 0 code) "~a" err)
          (is (equal '(:system :user :assistant :user :assistant) roles))
          (is (search "> hello" text) "the saved conversation is replayed")
          (is (search "hello!" text))
          (is (search "again!" text)))
        (is (equal (list id) (saved-chats)) "a resumed chat keeps saving into its own session")
        (is (equal '(:system :user :assistant :user :assistant) (saved-conversation id)))))))

(test resume-names-a-session
  (with-protocol
    (with-home ()
      (chat-session '("one") "--model" "test-echo:any")
      (let ((old (first (saved-chats))))
        (chat-session '("two") "--model" "test-echo:any")
        (multiple-value-bind (code text)
            (chat-session '() "--resume" old)
          (is (= 0 code))
          (is (search "> one" text))
          (is (not (search "> two" text))))))))

(test resume-mounts-the-saved-agent-and-provider-again
  (with-protocol
    (with-home ()
      (chat-session '("hello") "--model" "test-echo:any"
                    "--system-file" (namestring (merge-pathnames "sys.txt" (make-system-file))))
      (let* ((registry (make-instance 'm:registry))
             (m:*registry* registry)
             (context (m:start-service (make-instance 'm:context :name :fresh) :registry registry)))
        (m:mount context 'protocol-echo)
        (unwind-protect
             (let ((out (make-string-output-stream)) (err (make-string-output-stream)))
               (is (null (m:lookup :chat)))
               (is (= 0 (miao/cli:main '("chat" "--resume") :context context :home *home*
                                                             :in (lambda () nil) :out out :err err))
                   "~a" (get-output-stream-string err))
               (is (search "> hello" (get-output-stream-string out))))
          (m:stop context))))))

(defun make-system-file ()
  (let ((directory (uiop:ensure-directory-pathname (make-sandbox-directory))))
    (alexandria:write-string-into-file "Be terse." (merge-pathnames "sys.txt" directory))
    directory))

(test resume-is-a-usage-error-when-there-is-nothing-to-resume
  (with-protocol
    (with-home ()
      (dolist (args '(("chat" "--resume") ("chat" "--resume" "nosuch")))
        (multiple-value-bind (code out err) (apply #'cli args)
          (is (= 2 code) "~s" args)
          (is (equal "" out))
          (is (search "no saved chat" err)))))))

(test resume-refuses-the-options-that-describe-a-new-agent
  (with-protocol
    (dolist (args '(("chat" "--resume" "--model" "test-echo:any")
                    ("chat" "--resume" "--tools" "tool-fs")
                    ("chat" "--resume" "--max-turns" "3")
                    ("chat" "--resume" "--system-file" "x")))
      (is (= 2 (apply #'cli args)) "~s" args))
    (is (= 2 (cli "run" "hi" "--resume")))
    (is (eq :latest (getf (miao/cli::parse-args '("--resume") :takes-prompt nil) :resume)))
    (is (equal "abc" (getf (miao/cli::parse-args '("--resume" "abc") :takes-prompt nil) :resume)))))
