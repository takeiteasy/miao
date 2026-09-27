(in-package #:miao/tests)
(in-suite :miao)

;;; miao/ui: the fold of events into client state, and the client that
;;; attaches to a named agent (~takeiteasy/miao#167).

(defun ev (type &rest keys)
  (list* :type type :ref nil keys))

(defun kinds (node) (mapcar #'ui:entry-kind (ui:node-transcript node)))

(defun entries-of (kind node)
  (remove kind (ui:node-transcript node) :key #'ui:entry-kind :test-not #'eq))

(test a-fresh-state-is-idle-and-empty
  (let ((state (ui:make-state)))
    (is (eq :idle (ui:state-status state)))
    (is (null (ui:state-transcript state)))))

(test a-run-folds-into-a-transcript-with-streamed-text
  (let ((state (ui:fold-events
                (list (ev :run-start :messages '((:role :user :content "hi")) :continue nil)
                      (ev :turn :turn 1)
                      (ev :text-delta :text "hel")
                      (ev :text-delta :text "lo")
                      (ev :done :reason :stop)))))
    (is (eq :running (ui:state-status state)))
    (is (eql 1 (ui:state-turn state)))
    (is (equal '(:message :text) (kinds (ui:state-root state))))
    (let ((entries (ui:state-transcript state)))
      (is (eq :user (ui:entry-role (first entries))))
      (is (equal "hi" (ui:entry-text (first entries))))
      (is (equal "hello" (ui:entry-text (second entries)))))
    (let ((done (ui:fold-event state (ev :run-done :reason :stop))))
      (is (eq :done (ui:state-status done)))
      (is (eq :stop (ui:state-reason done))))))

(test folding-does-not-change-the-state-it-was-given
  (let* ((before (ui:fold-events (list (ev :turn :turn 1) (ev :text-delta :text "a"))))
         (text-before (ui:entry-text (first (ui:state-transcript before)))))
    (ui:fold-event before (ev :text-delta :text "b"))
    (ui:fold-event before (ev :run-done :reason :stop))
    (is (equal text-before (ui:entry-text (first (ui:state-transcript before)))))
    (is (eq :running (ui:state-status before)))))

(test a-tool-call-goes-from-streaming-to-running-to-done
  (let* ((s1 (ui:fold-events (list (ev :turn :turn 1)
                                   (ev :tool-call-delta :id "c1" :name :fs-read :arguments "{\"pa")
                                   (ev :tool-call-delta :id "c1" :name :fs-read :arguments "th\":1}"))))
         (call (first (entries-of :call (ui:state-root s1)))))
    (is (eq :streaming (ui:entry-status call)))
    (is (equal "{\"path\":1}" (ui:entry-text call)))
    (let* ((s2 (ui:fold-event s1 (ev :tool-call :id "c1" :name :fs-read :arguments '(:path 1))))
           (running (first (entries-of :call (ui:state-root s2)))))
      (is (eq :running (ui:entry-status running)))
      (is (equal '(:path 1) (ui:entry-arguments running)))
      (is (eql 1 (length (entries-of :call (ui:state-root s2)))))
      (let ((done (first (entries-of :call (ui:state-root
                                            (ui:fold-event s2 (ev :tool-result :id "c1"
                                                                  :result '(:ok "x"))))))))
        (is (eq :done (ui:entry-status done)))
        (is (equal '(:ok "x") (ui:entry-result done)))))))

(test a-detached-call-is-marked-until-its-result
  (let ((state (ui:fold-events (list (ev :tool-call :id "c1" :name :shell :arguments nil)
                                     (ev :tool-detached :id "c1" :name :shell)))))
    (is (eq :detached (ui:entry-status (first (entries-of :call (ui:state-root state))))))))

(test a-steer-and-an-interrupted-turn-are-in-the-transcript
  (let ((state (ui:fold-events
                (list (ev :turn :turn 1)
                      (ev :tool-call-delta :id "c1" :name :shell :arguments "{")
                      (ev :turn-interrupted :turn 1)
                      (ev :steer :content "shorter" :interrupt t :input-id nil)))))
    (is (equal '(:call :notice :steer) (kinds (ui:state-root state))))
    (is (eq :abandoned (ui:entry-status (first (entries-of :call (ui:state-root state))))))
    (let ((steer (first (entries-of :steer (ui:state-root state)))))
      (is (equal "shorter" (ui:entry-text steer)))
      (is (eq :interrupt (ui:entry-status steer))))))

(test retries-trims-and-failed-turns-are-notices
  (let ((state (ui:fold-events
                (list (ev :turn-retry :turn 1 :attempt 2 :reason :timeout)
                      (ev :context-trimmed :turn 1 :omitted '(1 2) :truncated nil :over-budget nil)
                      (ev :done :reason '(:error :boom))))))
    (is (equal '(:turn-retry :context-trimmed :turn-failed)
               (mapcar #'ui:entry-name (entries-of :notice (ui:state-root state)))))))

(test a-sub-agent-hangs-under-the-call-that-started-it
  (let* ((ref '(1 . "c1"))
         (state (ui:fold-events
                 (list (ev :run-start :messages '((:role :user :content "go")) :continue nil)
                       (ev :turn :turn 1)
                       (ev :tool-call :id "c1" :name :agent-task :arguments '(:task "help"))
                       (list :type :run-start :ref ref :messages '((:role :user :content "help"))
                             :continue nil :agent nil :parent nil)
                       (list :type :turn :ref ref :turn 1 :agent nil :parent nil)
                       (list :type :text-delta :ref ref :text "sub-answer" :agent nil :parent nil)
                       (list :type :run-done :ref ref :reason :stop :agent nil :parent nil)
                       (ev :tool-result :id "c1" :result '(:ok "sub-answer"))))))
    (let* ((root (ui:state-root state))
           (children (ui:state-children state root)))
      (is (eq :running (ui:node-status root)))
      (is (eql 1 (length children)))
      (let ((child (first children)))
        (is (equal ref (ui:node-key child)))
        (is (equal "c1" (ui:node-call-id child)))
        (is (eq :done (ui:node-status child)))
        (is (equal "sub-answer" (ui:entry-text (first (entries-of :text child)))))
        (is (null (entries-of :text root)) "the child's text is not the root's")))))

(test orphan-events-from-a-mid-run-join-and-unknown-types-are-folded
  (let ((state (ui:fold-events
                (list (ev :text-delta :text "tail")
                      (ev :tool-result :id "gone" :result '(:ok 1))
                      (ev :tool-call-delta :id nil :name nil :arguments nil)
                      (ev :not-in-the-contract :whatever t)
                      (ev :run-done :reason :stop)))))
    (is (eq :done (ui:state-status state)))
    (is (equal "tail" (ui:entry-text (first (entries-of :text (ui:state-root state))))))
    (is (eq :done (ui:entry-status (first (entries-of :call (ui:state-root state))))))))

(test a-second-run-adds-to-the-transcript
  (let ((state (ui:fold-events
                (list (ev :run-start :messages '((:role :user :content "one")) :continue nil)
                      (ev :run-done :reason :stop)
                      (ev :run-start :messages '((:role :user :content "two")) :continue nil)))))
    (is (eq :running (ui:state-status state)))
    (is (null (ui:state-reason state)))
    (is (equal '("one" "two") (mapcar #'ui:entry-text (entries-of :message (ui:state-root state)))))))

;;; --- the client -----------------------------------------------------------

(defun wait-for-status (client status)
  (is-true (eventually (lambda () (eq status (ui:state-status (ui:client-state client)))) 5)))

(test a-client-follows-a-run-and-detaches
  (with-agent ((streamed-reply "ok"))
    (mount-assistant)
    (let* ((changes 0)
           (client (ui:attach :assistant :on-change (lambda (state) (declare (ignore state)) (incf changes)))))
      (ui:run client "hi")
      (wait-for-status client :done)
      (let ((state (ui:client-state client)))
        (is (eq :stop (ui:state-reason state)))
        (is (equal '(:message :text) (kinds (ui:state-root state))))
        (is (equal "ok" (ui:entry-text (second (ui:state-transcript state))))))
      (is (plusp changes))
      (is (eq :ok (ui:detach client)))
      (let ((frozen (ui:client-state client)))
        (ui:run client "again")
        (sleep 0.3)
        (is (eq frozen (ui:client-state client)) "a detached client hears nothing")))))

(test a-client-sends-steer-and-cancel-to-a-run-under-way
  (let ((release (list nil)))
    (with-agent ((interruptible-backend release))
      (unwind-protect
           (progn
             (mount-assistant)
             (let ((client (ui:attach :assistant)))
               (ui:run client "go")
               (is-true (eventually (lambda ()
                                      (entries-of :text (ui:state-root (ui:client-state client))))
                                    5))
               (is (eq :ok (ui:steer client "shorter")))
               (is (eq :ok (first (alexandria:ensure-list (ui:cancel client)))))
               (wait-for-status client :done)
               (let ((state (ui:client-state client)))
                 (is (eq :cancelled (ui:state-reason state))))))
        (setf (car release) t)))))

(test a-client-attached-mid-run-catches-up-on-the-run-so-far
  (let ((release (list nil)))
    (with-agent ((interruptible-backend release))
      (unwind-protect
           (let ((early (progn (mount-assistant) (ui:attach :assistant))))
             (ui:run early "go")
             (is-true (eventually (lambda ()
                                    (entries-of :text (ui:state-root (ui:client-state early))))
                                  5))
             (let ((late (ui:attach :assistant)))
               (is (eq :running (ui:state-status (ui:client-state late))))
               (is-true (eventually
                         (lambda ()
                           (let ((root (ui:state-root (ui:client-state late))))
                             (and (entries-of :message root) (entries-of :text root))))
                         5))
               (let ((root (ui:state-root (ui:client-state late))))
                 (is (equal "go" (ui:entry-text (first (entries-of :message root)))))
                 (is (equal (ui:entry-text (first (entries-of :text (ui:state-root (ui:client-state early)))))
                            (ui:entry-text (first (entries-of :text root))))))
               (is (eql 1 (ui:state-turn (ui:client-state late))))))
        (setf (car release) t)))))

(test a-command-to-an-agent-that-is-not-mounted-signals
  (with-agent ((streamed-reply "ok"))
    (let ((ui:*command-timeout* 0.1))
      (signals ui:agent-unavailable (ui:attach :nobody)))))
