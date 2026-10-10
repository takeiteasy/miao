(in-package #:miao/tests)
(in-suite :miao)

;;; The run journal: what an agent writes to it, and the
;;; conversation read back from it.

(defmacro with-journaled-agent ((agent recorder path replies &rest options) &body body)
  "Mount an echo agent named :J journaling to a temporary PATH, behind a model that
answers REPLIES, and call BODY with it and a recorder on its events."
  `(with-vault-path (,path)
     (let ((,recorder (make-recorder)))
       (call-with-agent (apply #'scripted ,replies) '(tool-echo)
                        (lambda (*ctx*)
                          (let ((,agent (m:mount *ctx* 'miao:agent :name :j
                                                 :model :test-keyed :tools '(:tool-echo)
                                                 :journal ,path :sink (recorder-sink ,recorder)
                                                 ,@options)))
                            (declare (ignorable ,agent))
                            ,@body))))))

(defun run-j (agent recorder text &rest keys)
  "Start a run of AGENT and wait for its :RUN-DONE."
  (let ((before (length (remove :run-done (recorded-events recorder) :key (lambda (e) (getf e :type))
                                                                      :test-not #'eq))))
    (m:cast agent (list* :run :messages (list (list :role :user :content text)) keys))
    (is-true (eventually (lambda ()
                           (> (length (remove :run-done (recorded-events recorder)
                                              :key (lambda (e) (getf e :type)) :test-not #'eq))
                              before))
                         5))))

(defun snapshot-messages (agent)
  (getf (m:call agent '(:snapshot)) :messages))

(defun echo-then-done (&optional (id "c1"))
  (list (sse-tool-call id "tool-echo" "{\"text\":\"hi\"}") (streamed-reply "done")))

;;; --- the conversation ---------------------------------------------------------

(test the-journal-holds-the-conversation-of-a-run
  (with-journaled-agent (agent recorder path (echo-then-done))
    (run-j agent recorder "go")
    (multiple-value-bind (messages turns) (miao:journal-conversation path :agent :j)
      (is (equal (snapshot-messages agent) messages))
      (is (= 2 turns))
      (is (equal '(:user :assistant :tool :assistant)
                 (mapcar (lambda (m) (getf m :role)) messages))))))

(test a-continued-run-adds-to-the-conversation-and-another-replaces-it
  (with-journaled-agent (agent recorder path (list (streamed-reply "one") (streamed-reply "two")
                                                   (streamed-reply "three")))
    (run-j agent recorder "a")
    (run-j agent recorder "b" :continue t)
    (is (equal (snapshot-messages agent) (miao:journal-conversation path :agent :j)))
    (is (= 4 (length (miao:journal-conversation path :agent :j))))
    (run-j agent recorder "c")
    (is (equal (snapshot-messages agent) (miao:journal-conversation path :agent :j)))
    (is (= 2 (length (miao:journal-conversation path :agent :j))))))

(test a-steer-and-a-system-message-are-in-the-conversation
  (with-journaled-agent (agent recorder path (list (streamed-reply "ok")) :system "be brief")
    (m:call agent '(:steer :content "psst"))
    (run-j agent recorder "go")
    (let ((messages (miao:journal-conversation path :agent :j)))
      (is (equal (snapshot-messages agent) messages))
      (is (eq :system (getf (first messages) :role)))
      (is-true (find "psst" messages :key (lambda (m) (getf m :content)) :test #'equal)))))

(test a-restore-replaces-the-conversation-in-the-journal
  (with-journaled-agent (agent recorder path (list (streamed-reply "ok")))
    (run-j agent recorder "go")
    (let ((state '(:messages ((:role :user :content "earlier") (:role :assistant :content "yes"))
                   :turns 1)))
      (m:call agent (list :restore state))
      (is (equal (getf state :messages) (miao:journal-conversation path :agent :j))))))

(test a-conversation-is-read-up-to-the-end-of-a-run
  (with-journaled-agent (agent recorder path (list (streamed-reply "one") (streamed-reply "two")))
    (run-j agent recorder "a")
    (run-j agent recorder "b" :continue t)
    (let ((runs (miao:journal-runs path :agent :j)))
      (is (= 2 (length runs)))
      (is (= 2 (length (miao:journal-conversation path :agent :j :run (first runs)))))
      (is (= 4 (length (miao:journal-conversation path :agent :j :run (second runs)))))
      (signals error (miao:journal-conversation path :agent :j :run "no-such-run")))))

(test a-last-turn-left-without-its-tool-replies-is-closed-as-interrupted
  (with-vault-path (path)
    (miao::journal-append path "r" :j nil :messages
                          :reset t
                          :messages '((:role :user :content "go")
                                      (:role :assistant :tool-calls ((:id "c1" :name :tool-echo :arguments (:text "hi"))
                                                                     (:id "c2" :name :tool-echo :arguments nil)))))
    (miao::journal-append path "r" :j nil :message
                          :message '(:role :tool :tool-call-id "c1" :content "{}"))
    (let ((messages (miao:journal-conversation path :agent :j)))
      (is (equal '("c1" "c2") (mapcar (lambda (m) (getf m :tool-call-id)) (cddr messages))))
      (is (search "interrupted" (getf (fourth messages) :content))))))

;;; --- the events -------------------------------------------------------------------

(test the-journal-holds-the-loops-events-without-the-streamed-deltas
  (with-journaled-agent (agent recorder path (echo-then-done))
    (run-j agent recorder "go")
    (let ((types (mapcar (lambda (e) (getf e :type))
                         (remove-if-not (lambda (e) (eq :event (getf e :kind)))
                                        (miao:journal-entries path :agent :j)))))
      (is-true (member :run-start types))
      (is-true (member :tool-result types))
      (is-true (member :run-done types))
      (is (null (intersection types '(:text-delta :tool-call-delta :done))))
      (is (= 2 (count :reply types))))))

(test an-events-payload-is-text-cut-to-the-cap
  (let ((fields (miao::journal-event-fields
                 '(:type :tool-result :id "c1" :result (:ok (:text "aaaaaaaaaaaaaaaaaaaaaaaa"))) 10)))
    (is (eq :tool-result (getf fields :type)))
    (is (equal "c1" (getf fields :id)))
    (is (stringp (getf fields :result)))
    (is (search "truncated" (getf fields :result)))))

(test a-hook-cannot-forge-a-reply-event
  (is-true (member :reply miao::+loop-event-types+)))

(test an-entry-that-would-not-read-back-is-written-as-unwritable
  (with-vault-path (path)
    (miao::journal-append path "r" :j nil :message :message (list :role :user :content #'identity))
    (miao::journal-append path "r" :j nil :message :message '(:role :user :content "after"))
    (is (equal '(:unwritable :message) (mapcar (lambda (e) (getf e :kind)) (miao:journal-entries path))))))

;;; --- the writer -------------------------------------------------------------------

(test a-reader-sees-every-entry-queued-before-it-in-order
  (with-vault-path (path)
    (dotimes (i 200)
      (miao::journal-append path "r" :j nil :message :message (list :role :user :content i)))
    (is (equal (loop for i below 200 collect i)
               (mapcar (lambda (e) (getf (getf e :message) :content))
                       (miao:journal-entries path))))))

(test a-call-is-written-after-the-entries-queued-before-it
  (with-vault-path (path)
    (miao::journal-append path "r" :j nil :message :message '(:role :user :content "go"))
    (miao::journal-call-accept path :j 1 (list (list :id "c1" :name :tool-echo :arguments nil)))
    (miao::journal-append path "r" :j nil :message :message '(:role :user :content "next"))
    (miao:journal-drain path)
    (is (equal '(:message :call :message) (mapcar (lambda (e) (getf e :kind)) (miao::%read-log path))))))

(test draining-puts-every-queued-entry-on-disk
  (with-vault-path (path)
    (dotimes (i 50)
      (miao::journal-append path "r" :j nil :event :type :tick))
    (miao:journal-drain path)
    (is (= 50 (length (miao::%read-log path))))))

(test retiring-the-writers-flushes-them-and-stops-their-threads
  (with-vault-path (path)
    (dotimes (i 20)
      (miao::journal-append path "r" :j nil :event :type :tick))
    (miao:journal-retire-writers)
    (is (null (miao::jw-thread (miao::%journal-writer path))))
    (is (= 20 (length (miao::%read-log path))))
    (miao::journal-append path "r" :j nil :event :type :tick)
    (is (= 21 (length (miao:journal-entries path))))))

(defmacro with-writer-setting ((variable value) &body body)
  "Set VARIABLE globally to VALUE for BODY, which the writer's thread needs to see."
  (let ((old (gensym)))
    `(let ((,old ,variable))
       (setf ,variable ,value)
       (unwind-protect (progn ,@body)
         (setf ,variable ,old)))))

(defun append-ticks (path count)
  (dotimes (i count)
    (miao::journal-append path "r" :j nil :message :message
                          (list :role :user :content (format nil "entry ~3,'0d of padding text" i)))))

(test a-full-queue-makes-the-agent-wait-and-every-entry-is-kept-in-order
  (with-vault-path (path)
    (with-writer-setting (miao::*journal-writer-max-bytes* 400)
      (let ((count 0)
            (thread nil))
        (miao::with-log-lock (path)
          (setf thread (bt:make-thread
                        (lambda ()
                          (dotimes (i 30)
                            (miao::journal-append
                             path "r" :j nil :message :message
                             (list :role :user :content (format nil "entry ~3,'0d of padding text" i)))
                            (setf count (1+ i))))))
          (sleep 0.3)
          (is (< count 30) "the agent waits while the disk is stalled"))
        (bt:join-thread thread)
        (miao:journal-drain path)
        (is (equal (loop for i below 30 collect (format nil "entry ~3,'0d of padding text" i))
                   (mapcar (lambda (e) (getf (getf e :message) :content))
                           (miao::%read-log path))))))))

(test a-line-longer-than-the-bound-is-still-queued
  (with-vault-path (path)
    (with-writer-setting (miao::*journal-writer-max-bytes* 10)
      (append-ticks path 3)
      (miao:journal-drain path)
      (is (= 3 (length (miao::%read-log path)))))))

(test a-batch-the-disk-refuses-is-retried-before-it-is-dropped
  (with-vault-path (path)
    (append-ticks path 1)
    (miao:journal-drain path)
    (with-writer-setting (miao::*journal-write-retries* '(0.2 0.2 0.2 0.2 0.2))
      (sb-posix:chmod (namestring path) #o444)
      (unwind-protect
           (progn (append-ticks path 1)
                  (sleep 0.3))
        (sb-posix:chmod (namestring path) #o644))
      (miao:journal-drain path)
      (is (= 2 (length (miao::%read-log path)))))))

(test a-batch-the-disk-keeps-refusing-is-dropped-after-its-retries
  (with-vault-path (path)
    (append-ticks path 1)
    (miao:journal-drain path)
    (with-writer-setting (miao::*journal-write-retries* '(0.05 0.05))
      (sb-posix:chmod (namestring path) #o444)
      (unwind-protect
           (progn (append-ticks path 1)
                  (miao:journal-drain path)
                  (is (= 1 (length (miao::%read-log path)))))
        (sb-posix:chmod (namestring path) #o644)))))

(test a-drain-with-a-timeout-says-whether-the-queue-emptied
  (with-vault-path (path)
    (append-ticks path 1)
    (is-true (miao:journal-drain path :timeout 1))
    (miao::with-log-lock (path)
      (append-ticks path 1)
      (is (null (miao:journal-drain path :timeout 0.1))))
    (is-true (miao:journal-drain path :timeout 1))))

(test the-exit-drain-stops-waiting-on-a-writer-that-is-stuck
  (with-vault-path (path)
    (append-ticks path 1)
    (miao:journal-drain path)
    (with-writer-setting (miao::*journal-exit-timeout* 0.2)
      (miao::with-log-lock (path)
        (append-ticks path 1)
        (let ((start (get-internal-real-time)))
          (miao::journal-drain-all)
          (is (< (- (get-internal-real-time) start) internal-time-units-per-second)))))
    (miao:journal-drain path)
    (is (= 2 (length (miao::%read-log path))))))

(test retiring-the-writers-stops-waiting-on-one-that-is-stuck
  (with-vault-path (path)
    (append-ticks path 1)
    (miao:journal-drain path)
    (miao::with-log-lock (path)
      (append-ticks path 1)
      (sleep 0.1)
      (let ((start (get-internal-real-time)))
        (miao:journal-retire-writers :timeout 0.2)
        (is (< (- (get-internal-real-time) start) internal-time-units-per-second))))
    (miao:journal-drain path)
    (is (= 2 (length (miao::%read-log path))))))

(test an-idle-writer-retires-and-a-later-entry-starts-another
  (with-vault-path (path)
    (let ((idle miao::*journal-writer-idle*))
      (setf miao::*journal-writer-idle* 0.05)
      (unwind-protect
           (progn (miao::journal-append path "r" :j nil :message :message '(:role :user :content "a"))
                  (sleep 0.3)
                  (is (null (miao::jw-thread (miao::%journal-writer path))))
                  (miao::journal-append path "r" :j nil :message :message '(:role :user :content "b"))
                  (is (= 2 (length (miao:journal-entries path)))))
        (setf miao::*journal-writer-idle* idle)))))

;;; --- compacting --------------------------------------------------------------------

(test compacting-folds-old-entries-and-keeps-the-conversation
  (with-journaled-agent (agent recorder path (echo-then-done))
    (run-j agent recorder "go")
    (let ((before (miao:journal-conversation path :agent :j))
          (size (length (miao:journal-entries path))))
      (is (consp (multiple-value-list (miao:journal-compact path :max-age 0))))
      (is (< (length (miao:journal-entries path)) size))
      (is (equal before (miao:journal-conversation path :agent :j)))
      (is (null (remove :event (miao:journal-entries path)
                        :key (lambda (e) (getf e :kind)) :test-not #'eq))))))

(test compacting-keeps-each-runs-key-and-conversation
  (with-journaled-agent (agent recorder path (list (streamed-reply "one") (streamed-reply "two")
                                                   (streamed-reply "three")))
    (run-j agent recorder "a")
    (run-j agent recorder "b" :continue t)
    (run-j agent recorder "c")
    (let* ((runs (miao:journal-runs path :agent :j))
           (before (mapcar (lambda (run) (miao:journal-conversation path :agent :j :run run)) runs)))
      (is (= 3 (length runs)))
      (miao:journal-compact path :max-age 0)
      (is (equal runs (miao:journal-runs path :agent :j)))
      (is (equal before (mapcar (lambda (run) (miao:journal-conversation path :agent :j :run run))
                                runs))))))

(test compacting-keeps-the-conversations-built-on-restores-outside-a-run
  (with-vault-path (path)
    (flet ((restore (&rest messages)
             (miao::journal-append path nil :j nil :messages :reset t :messages messages))
           (say (run role text)
             (miao::journal-append path run :j nil :message :message (list :role role :content text))))
      (restore '(:role :user :content "x1") '(:role :assistant :content "y1"))
      (say "r1" :user "a")
      (say "r1" :assistant "ra")
      (restore '(:role :user :content "x2") '(:role :assistant :content "y2"))
      (say "r2" :user "b")
      (say "r2" :assistant "rb"))
    (let ((before (mapcar (lambda (run) (miao:journal-conversation path :agent :j :run run))
                          '("r1" "r2"))))
      (is (equal '("x1" "y1" "a" "ra") (mapcar (lambda (m) (getf m :content)) (first before))))
      (miao:journal-compact path :max-age 0)
      (is (equal before (mapcar (lambda (run) (miao:journal-conversation path :agent :j :run run))
                                '("r1" "r2")))))))

(test compacting-leaves-a-log-with-a-malformed-entry-alone
  (with-vault-path (path)
    (miao::journal-append path "r" :j nil :message :message '(:role :user :content "a"))
    (miao:journal-drain path)
    (with-open-file (out path :direction :output :if-exists :append) (write-string "(:kind" out))
    (is (null (miao:journal-compact path :max-age 0)))))

(test a-sub-agent-journals-under-its-own-name-and-the-roots-run
  (with-vault-path (path)
    (let ((recorder (make-recorder)))
      (call-with-agent (scripted (sse-tool-call "c1" "agent-task" "{\"task\":\"help\"}")
                                 (streamed-reply "child done") (streamed-reply "done"))
          '(tool-echo)
        (lambda (*ctx*)
          (let ((agent (m:mount *ctx* 'miao:agent :name :j :model :test-keyed
                                                  :tools '(:tool-echo) :sub-agents t :journal path
                                                  :sink (recorder-sink recorder))))
            (run-j agent recorder "go")
            (let* ((entries (miao:journal-entries path))
                   (children (remove-if-not (lambda (e) (getf e :parent)) entries)))
              (is-true children)
              (is (eq :j (getf (first children) :parent)))
              (is (= 1 (length (miao:journal-runs path)))))))))))

;;; --- forking from a journal -----------------------------------------------------------

(test an-agent-with-a-journal-is-forked-from-it
  (with-journaled-agent (agent recorder path (echo-then-done))
    (run-j agent recorder "go")
    (is (eq :f (miao:fork-agent *ctx* :j :turn 1 :as :f)))
    (is (equal (miao:fork-conversation (snapshot-messages agent) :turn 1)
               (snapshot-messages (m:lookup :f))))
    (is (equal (snapshot-messages agent) (miao:journal-conversation path :agent :j)))))

(test a-fork-can-be-taken-from-a-past-run
  (with-journaled-agent (agent recorder path (list (streamed-reply "one") (streamed-reply "two")))
    (run-j agent recorder "a")
    (run-j agent recorder "b")
    (let ((runs (miao:journal-runs path :agent :j)))
      (miao:fork-agent *ctx* :j :as :past :run (first runs))
      (miao:fork-agent *ctx* :j :as :last)
      (is (equal "a" (getf (first (snapshot-messages (m:lookup :past))) :content)))
      (is (equal "b" (getf (first (snapshot-messages (m:lookup :last))) :content))))))

(test a-run-needs-the-journal-to-fork-from
  (with-agent ((final-reply "ok") 'tool-echo)
    (m:mount *ctx* 'miao:agent :name :plain :model :test-keyed)
    (signals error (miao:fork-agent *ctx* :plain :as :f :run "any"))))

(test a-journal-forks-an-agent-that-is-no-longer-mounted
  (with-journaled-agent (agent recorder path (echo-then-done) :max-turns 5)
    (run-j agent recorder "go")
    (let ((messages (snapshot-messages agent)))
      (m:unmount *ctx* :j)
      (is (eq :revived (miao:fork-journal *ctx* path :agent :j :as :revived)))
      (is (equal messages (snapshot-messages (m:lookup :revived))))
      (let ((described (m:call (m:lookup :revived) '(:describe))))
        (is (eq :test-keyed (getf described :model)))
        (is (eql 5 (getf described :max-turns)))
        (is (equal path (getf described :journal))))
      (is (eq :cut (miao:fork-journal *ctx* path :agent :j :as :cut :turn 1)))
      (is (= 3 (length (snapshot-messages (m:lookup :cut))))))))

(test fork-journal-refuses-what-it-cannot-fork
  (with-journaled-agent (agent recorder path (echo-then-done))
    (run-j agent recorder "go")
    (signals error (miao:fork-journal *ctx* path :agent :nobody :as :f :run "no-such-run"))
    (signals error (miao:fork-journal *ctx* path :agent :j :as :j))
    (signals error (miao:fork-journal *ctx* path :as :f))
    (signals error (miao:fork-journal *ctx* path :agent :j))))
