(in-package #:miao/tests)
(in-suite :miao)

;;; Resuming a call (~takeiteasy/miao#77): a logged call that ended :LOST,
;;; :ABANDONED or :INTERRUPTED is run again on request, and TOOL-CALLS
;;; (~takeiteasy/miao#179) lists, compacts and resumes them.

(m:defservice tool-again () () (:name :tool-again))

(defmethod m:metadata ((service tool-again))
  (list :kind :tool :name :tool-again :trust :agent :resumable t
        :summary "Safe to run twice" :params nil))

(miao::define-tool-handler tool-again (service args)
  args
  (miao::ok :again t))

(m:defservice tool-again-slow () () (:name :tool-again-slow))

(defmethod m:metadata ((service tool-again-slow))
  (list :kind :tool :name :tool-again-slow :trust :agent :resumable t
        :summary "Safe to run twice, and slow" :params nil))

(miao::define-tool-handler tool-again-slow (service args)
  args
  (sleep 0.6)
  (miao::ok :again-slow t))

(defparameter *dead-owner*
  (list :pid 999999 :host (machine-instance) :start 1 :token "other"))

(defun seed-call (path id &key (name :tool-again) (arguments "{}") (agent :assistant)
                            done cut (call-id "c1"))
  "Append a call dispatched by a process that is gone, finished with DONE, an
outcome, if given."
  (miao::%append-log-locked
   path (list* :kind :call :id id :at "2026-01-01T00:00:00Z" :agent agent
               :call-id call-id :name name :arguments arguments :turn 1 :by *dead-owner*
               (and cut '(:cut t))))
  (when done
    (miao::%append-log-locked
     path (list :kind :done :id id :at "2026-01-01T00:00:01Z" :outcome done :content nil))))

(defun call-with-id (path id)
  (find id (miao:call-entries path) :key (lambda (e) (getf e :id)) :test #'equal))

(defun call-child (child message)
  (multiple-value-call #'miao::%call-result (m:call child message)))

(defun accept-all (entry) (declare (ignore entry)) nil)

;;; --- the log API ---------------------------------------------------------

(test a-lost-call-is-resumed-as-a-new-call-that-links-back
  (with-vault-path (path)
    (seed-call path "x-0")
    (multiple-value-bind (resumed refused)
        (miao::call-log-resume path '("x-0") :assistant 3 #'accept-all)
      (is (null refused))
      (destructuring-bind ((old new entry value)) resumed
        (declare (ignore value))
        (is (equal "x-0" old))
        (is (equal "c1" (getf entry :call-id)))
        (let ((call (call-with-id path new)))
          (is (equal "x-0" (getf call :resumes)))
          (is (equal "c1" (getf call :call-id)))
          (is (eq :tool-again (getf call :name)))
          (is (equal "{}" (getf call :arguments)))
          (is (eql 3 (getf call :turn)))
          (is (eq :accepted (getf call :status))))
        (is (equal new (getf (call-with-id path "x-0") :resumed-by)))
        (is (eq :lost (getf (call-with-id path "x-0") :status)))))))

(test an-abandoned-or-interrupted-call-may-be-resumed
  (with-vault-path (path)
    (seed-call path "a" :done :abandoned)
    (seed-call path "i" :done :interrupted)
    (is (= 2 (length (miao::call-log-resume path '("a" "i") nil 0 #'accept-all))))))

(test a-call-is-resumed-once
  (with-vault-path (path)
    (seed-call path "x-0")
    (let ((new (second (first (miao::call-log-resume path '("x-0") nil 0 #'accept-all)))))
      (multiple-value-bind (resumed refused)
          (miao::call-log-resume path '("x-0") nil 0 #'accept-all)
        (is (null resumed))
        (is (equal (list (list "x-0" (format nil "already resumed as ~a" new))) refused))))
    (is (= 2 (length (miao:call-entries path))))))

(test an-id-given-twice-is-resumed-once
  (with-vault-path (path)
    (seed-call path "x-0")
    (is (= 1 (length (miao::call-log-resume path '("x-0" "x-0") nil 0 #'accept-all))))))

(test a-finished-cut-or-unknown-call-is-refused
  (with-vault-path (path)
    (seed-call path "done" :done :ok)
    (seed-call path "failed" :done :error)
    (seed-call path "cut" :cut t)
    (multiple-value-bind (resumed refused)
        (miao::call-log-resume path '("done" "failed" "cut" "nope") nil 0 #'accept-all)
      (is (null resumed))
      (is (equal '("done" "failed" "cut" "nope") (mapcar #'first refused)))
      (is (search "not lost" (second (first refused))))
      (is (search "cut" (second (third refused))))
      (is (equal "no such call" (second (fourth refused)))))
    (is (= 3 (length (miao:call-entries path))))))

(test a-call-the-check-refuses-is-not-logged
  (with-vault-path (path)
    (seed-call path "x-0")
    (multiple-value-bind (resumed refused)
        (miao::call-log-resume path '("x-0") nil 0 (lambda (entry) (declare (ignore entry)) "no"))
      (is (null resumed))
      (is (equal '(("x-0" "no")) refused)))
    (is (= 1 (length (miao:call-entries path))))))

(test a-call-whose-arguments-outgrew-the-cap-is-logged-cut
  (with-vault-path (path)
    (miao::call-log-accept path :assistant 1
                           (list (list :id "c1" :name :tool-echo
                                       :arguments (list :text (make-string 100 :initial-element #\x))))
                           :cap 20)
    (is-true (getf (first (miao:call-entries path)) :cut))
    (miao::call-log-accept path :assistant 1
                           (list (list :id "c2" :name :tool-echo :arguments '(:text "hi"))))
    (is (null (getf (second (miao:call-entries path)) :cut)))))

;;; --- the agent -----------------------------------------------------------

(test a-lost-call-runs-again-and-its-result-folds-into-a-later-turn
  (with-vault-path (path)
    (seed-call path "x-0" :call-id "c9")
    (with-agent ((scripted (final-reply "moving on") (final-reply "got it")) 'tool-again)
      (m:with-process (runner)
        (let* ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                  :tools '(:tool-again) :call-log path))
               (answer (call-child child (list :run :continue t :resume '("x-0")
                                                    :messages '((:role :user :content "go"))))))
          (is (equal '("x-0") (mapcar #'car (getf (second answer) :resumed))))
          (is (null (getf (second answer) :refused)))
          (multiple-value-bind (message received) (m:receive :timeout 8)
            (is-true received)
            (is (eq :stop (getf (second (fourth message)) :stop-reason))))
          (is (search "tool call c9 (tool-again) finished" (request-body 2)))
          (is (search "again" (request-body 2)))
          (let ((old (call-with-id path "x-0")))
            (is (eq :ok (getf (call-with-id path (getf old :resumed-by)) :status)))))))))

(test resuming-with-nothing-to-say-starts-no-run-when-every-call-is-refused
  (with-vault-path (path)
   (let ((recorder (make-recorder)))
    (seed-call path "slow" :name :tool-slow)
    (seed-call path "out" :name :tool-echo)
    (seed-call path "sub" :name :agent-task)
    (with-agent ((scripted (final-reply "unused")) 'tool-again 'tool-slow)
      (m:with-process (runner)
        (let* ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                  :tools '(:tool-again :tool-slow) :call-log path
                                  :sink (recorder-sink recorder)))
               (answer (call-child child (list :resume :ids '("slow" "out" "sub")))))
          (is (null (getf (second answer) :resumed)))
          (is (equal '("slow" "out" "sub") (mapcar #'first (getf (second answer) :refused))))
          (destructuring-bind (slow out sub) (getf (second answer) :refused)
            (is (search "not resumable" (second slow)))
            (is (search "allow-list" (second out)))
            (is (search "sub-agent" (second sub))))
          (is (null (requests)))
          (is (null (getf (call-child child '(:snapshot)) :in-flight))
              "an agent that started no run is still idle")
          (is (null (recorded-events recorder)) "and announced nothing")))))))

(test force-resumes-a-call-to-a-tool-that-is-not-resumable
  (with-vault-path (path)
    (seed-call path "slow" :name :tool-slow)
    (with-agent ((scripted (final-reply "moving on") (final-reply "got it")) 'tool-slow)
      (m:with-process (runner)
        (let* ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                  :tools '(:tool-slow) :call-log path))
               (answer (call-child child (list :resume :ids '("slow") :force t))))
          (is (= 1 (length (getf (second answer) :resumed))))
          (is-true (nth-value 1 (m:receive :timeout 8)))
          (is (search "tool call c1 (tool-slow) finished" (request-body 1))))))))

(test resume-needs-a-call-log-and-ids
  (with-agent ((scripted (final-reply "unused")) 'tool-again)
    (m:with-process (runner)
      (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                               :tools '(:tool-again))))
        (is (search ":call-log" (princ-to-string (call-child child '(:resume :ids ("x"))))))
        (is (search ":call-log" (princ-to-string (call-child child '(:run :resume ("x"))))))
        (is (search ":ids" (princ-to-string (call-child child '(:resume))))))))
  (with-vault-path (path)
    (with-agent ((scripted (final-reply "unused")) 'tool-again)
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :tools '(:tool-again) :call-log path)))
          (is (search ":messages or :continue"
                      (princ-to-string (call-child child '(:run :resume ("x")))))))))))

(test a-resume-sent-to-a-running-agent-joins-its-run
  (with-vault-path (path)
    (seed-call path "x-0" :call-id "c9")
    (with-agent ((scripted (tool-call-reply "c1" "tool-slow" "{}") (final-reply "done"))
                 'tool-again 'tool-slow)
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :tools '(:tool-again :tool-slow) :call-log path)))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually (lambda () (= 1 (length (requests))))))
          (is (= 1 (length (getf (second (call-child child '(:resume :ids ("x-0")))) :resumed))))
          (is-true (nth-value 1 (m:receive :timeout 8)))
          (is (search "tool call c9 (tool-again) finished" (request-body 2))))))))

(test a-detached-call-of-a-restored-agent-is-resumed-from-its-snapshot
  (with-vault-path (path)
    (with-agent ((scripted (tool-call-reply "c1" "tool-again-slow" "{}")
                           (final-reply "moving on")
                           (final-reply "got it"))
                 'tool-again-slow)
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :tools '(:tool-again-slow) :tool-grace 100 :call-log path)))
          (m:cast child (list :run :messages '((:role :user :content "go"))))
          (is-true (eventually
                    (lambda ()
                      (and (= 2 (length (requests)))
                           (getf (getf (m:call child '(:snapshot)) :in-flight) :detached)))))
          (let* ((state (m:call child '(:snapshot)))
                 (ids (getf (getf state :in-flight) :call-log-ids)))
            (is (= 1 (length ids)))
            (call-child child (list :restore state))
            (is (eq :interrupted (getf (call-with-id path (first ids)) :status)))
            (let ((answer (call-child child (list :resume :ids ids))))
              (is (= 1 (length (getf (second answer) :resumed)))))
            (is-true (nth-value 1 (m:receive :timeout 8)))
            (is (search "tool call c1 (tool-again-slow) finished" (request-body 3)))
            (is-true (eventually
                      (lambda ()
                        (let ((new (getf (call-with-id path (first ids)) :resumed-by)))
                          (and new (eq :ok (getf (call-with-id path new) :status)))))))))))))

(test a-resume-past-max-detached-is-refused
  (with-vault-path (path)
    (seed-call path "x-0" :call-id "c9")
    (seed-call path "x-1" :call-id "c8")
    (seed-call path "x-2" :call-id "c7" :done :ok)
    (with-agent ((scripted (final-reply "waiting")) 'tool-again)
      (m:with-process (runner)
        (let ((child (m:delegate *ctx* 'miao:agent :model :provider-test-keyed
                                 :tools '(:tool-again) :call-log path :max-detached 1)))
          (let ((answer (second (call-child child '(:run :continue t :resume ("x-0" "x-1" "x-2")
                                                    :messages ((:role :user :content "go")))))))
            (is (= 1 (length (getf answer :resumed))))
            (is (equal "x-0" (car (first (getf answer :resumed)))))
            (is (equal '(("x-1" "max-detached reached")) (subseq (getf answer :refused) 0 1)))
            (is (search "not lost" (second (second (getf answer :refused)))))))))))

;;; --- tool-calls ----------------------------------------------------------

(defun calls-tool (op &rest args)
  (apply #'miao:invoke-tool :tool-calls :op op args))

(test tool-calls-lists-filters-and-limits
  (with-vault-path (path)
    (seed-call path "a" :done :ok)
    (seed-call path "b")
    (with-agent ((scripted (final-reply "unused")) 'tool-again)
      (m:mount *ctx* 'miao:tool-calls :path path)
      (let ((all (second (calls-tool :list))))
        (is (equal '("a" "b") (mapcar (lambda (e) (getf e :id)) (getf all :entries))))
        (is (eql 2 (getf all :total))))
      (let ((lost (second (calls-tool :list :status :lost))))
        (is (equal '("b") (mapcar (lambda (e) (getf e :id)) (getf lost :entries))))
        (is (eql 1 (getf lost :total))))
      (let ((last (second (calls-tool :list :limit 1))))
        (is (equal '("b") (mapcar (lambda (e) (getf e :id)) (getf last :entries))))
        (is (eql 2 (getf last :total)))))))

(test tool-calls-compacts
  (with-vault-path (path)
    (seed-call path "a" :done :ok)
    (seed-call path "b")
    (with-agent ((scripted (final-reply "unused")) 'tool-again)
      (m:mount *ctx* 'miao:tool-calls :path path)
      (is (equal '(:ok (:dropped 1 :kept 1)) (calls-tool :compact :max-age 0))))))

(test tool-calls-is-operator-trust
  (is (eq :operator (miao:tool-trust (m:metadata (make-instance 'miao:tool-calls))))))

(test tool-calls-resumes-in-the-agent-that-dispatched-the-call
  (with-vault-path (path)
    (seed-call path "x-0")
    (with-agent ((scripted (final-reply "moving on") (final-reply "got it")) 'tool-again)
      (m:mount *ctx* 'miao:tool-calls :path path)
      (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed
                                 :tools '(:tool-again) :call-log path)
      (let ((answer (calls-tool :resume :ids '("x-0"))))
        (is (equal '("x-0") (mapcar #'car (getf (second answer) :resumed))))
        (is (null (getf (second answer) :refused))))
      (is-true (eventually
                (lambda ()
                  (let ((new (getf (call-with-id path "x-0") :resumed-by)))
                    (and new (eq :ok (getf (call-with-id path new) :status)))))
                5))
      (is (equal '("x-0") (mapcar #'car (getf (second (calls-tool :resume :ids '("x-0")
                                                                          :agent "assistant"))
                                              :refused)))))))

(test tool-calls-passes-an-agents-refusals-through
  (with-vault-path (path)
    (seed-call path "slow" :name :tool-slow)
    (with-agent ((scripted (final-reply "unused")) 'tool-again 'tool-slow)
      (m:mount *ctx* 'miao:tool-calls :path path)
      (m:mount *ctx* 'miao:agent :name :assistant :model :provider-test-keyed
                                 :tools '(:tool-again :tool-slow) :call-log path)
      (let ((answer (calls-tool :resume :ids '("slow"))))
        (is (null (getf (second answer) :resumed)))
        (is (search "not resumable" (second (first (getf (second answer) :refused))))))
      (is (= 1 (length (getf (second (calls-tool :resume :ids '("slow") :force t)) :resumed)))))))

(test tool-calls-needs-an-agent-it-can-name
  (with-vault-path (path)
    (seed-call path "x-0" :agent nil)
    (seed-call path "x-1" :agent :ghost)
    (with-agent ((scripted (final-reply "unused")) 'tool-again)
      (m:mount *ctx* 'miao:tool-calls :path path)
      (is (search ":agent is required" (princ-to-string (calls-tool :resume :ids '("x-0")))))
      (is (search "no agent named ghost" (princ-to-string (calls-tool :resume :ids '("x-1")))))
      (is (search "no agent named other"
                  (princ-to-string (calls-tool :resume :ids '("x-0") :agent "other")))))))
