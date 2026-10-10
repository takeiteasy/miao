(in-package #:miao/tests)
(in-suite :miao)

;;; TOOL-PLAN, the DSL gate: checking a whole plan
;;; before any step runs, threading a value through :REF, and the reasons a
;;; step or a whole plan is refused.

;;; A minimal :TRUST :AGENT tool that sleeps, so PLAN-HONOURS-ITS-TIMEOUT can
;;; force real elapsed time between two steps without a tool this trust
;;; level otherwise offering one.

(miao:define-tool :tool-sleep
    (:trust :agent
     :summary "Sleep for :ms milliseconds"
     :params ((:ms (integer 0) :required t :doc "milliseconds to sleep")))
  (:invoke (ms)
    (sleep (/ ms 1000.0))
    (miao::ok)))

;;; Sleeps in small slices, stopping when cancelled, and records that it saw
;;; the cancel.

(defvar *saw-cancel* nil)

(miao:define-tool :tool-patient
    (:trust :agent
     :summary "Sleep for :ms milliseconds, stopping early if cancelled"
     :params ((:ms (integer 0) :required t :doc "milliseconds to sleep")))
  (:invoke (ms)
    (loop repeat (ceiling ms 10)
          do (when (and miao::cancel-token (miao::cancelled-p miao::cancel-token))
               (setf *saw-cancel* t)
               (return))
             (sleep 0.01))
    (miao::ok)))

;;; Ignores its cancel token and any timeout: sleeps the whole time asked.

(miao:define-tool :tool-stubborn
    (:trust :agent
     :summary "Sleep for :ms milliseconds, whatever happens"
     :params ((:ms (integer 0) :required t :doc "milliseconds to sleep")))
  (:invoke (ms)
    (sleep (/ ms 1000.0))
    (miao::ok :slept ms)))

;;; Declares its own :TIMEOUT and reports what it was given.

(miao:define-tool :tool-timeout-echo
    (:trust :agent
     :summary "Answer the :timeout this call was given"
     :params ((:timeout (integer 1) :default miao::+default-tool-timeout+
               :doc "milliseconds")))
  (:invoke (timeout)
    (miao::ok :timeout timeout)))

;;; Answers whatever :value it was given, unchanged.

(miao:define-tool :tool-plan-echo
    (:trust :agent
     :summary "Answer :value"
     :params ((:value any :doc "any value")))
  (:invoke (value)
    (miao::ok :value value)))

(defvar *plan-sandbox* nil "The fs tool's sandbox root for the running test.")

(defun call-with-plan (allow max-steps body &key sleep-tool extra)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (root (make-sandbox-directory))
         (context (m:start-service (make-instance 'm:context :name :plan-tools)
                                   :registry registry)))
    (setf *plan-sandbox* root)
    (unwind-protect
         (progn
           (m:mount context 'miao:tool-fs :root root)
           (m:mount context 'miao:tool-shell)
           (when sleep-tool (m:mount context 'tool-sleep))
           (dolist (tool extra) (apply #'m:mount context (alexandria:ensure-list tool)))
           (m:mount context 'miao:tool-plan :allow allow :max-steps max-steps)
           (funcall body))
      (m:stop context)
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname root)
                                  :validate t :if-does-not-exist :ignore))))

(defmacro with-plan (&body body)
  "The common case: TOOL-FS allowed, the default step limit."
  `(call-with-plan '(:tool-fs) 16 (lambda () ,@body)))

(defun plan (steps &optional (timeout nil timeout-p))
  (if timeout-p
      (miao:invoke-tool :tool-plan :steps steps :timeout timeout)
      (miao:invoke-tool :tool-plan :steps steps)))

(defun plan-results (result)
  (getf (second result) :results))

(defun sandbox-file-exists-p (name)
  (uiop:file-exists-p (concatenate 'string *plan-sandbox* "/" name)))

;;; --- running a plan ----------------------------------------------------

(test plan-threads-a-value-through-ref
  (with-plan
    (let ((result (plan (list (list :as "w" :tool "tool-fs"
                                    :args (list :op :write :path "src.txt" :data "hello ref"))
                              (list :as "r" :tool "tool-fs"
                                    :args (list :op :read :path "src.txt"))
                              (list :as "c" :tool "tool-fs"
                                    :args (list :op :write :path "dst.txt"
                                               :data (list :ref "r.data")))
                              (list :as "final" :tool "tool-fs"
                                    :args (list :op :read :path "dst.txt"))))))
      (is (not (miao:result-error-p result)))
      (is (equal "hello ref" (getf (getf (plan-results result) :final) :data)))
      (is (equal 4 (getf (second result) :steps))))))

;;; --- refused before any step runs ---------------------------------------

(test plan-refuses-a-tool-outside-its-allow-list
  (with-plan
    ;; tool-shell is mounted but not in this plan's :allow.
    (let ((result (plan (list (list :tool "tool-shell" :args (list :cmd "true"))
                              (list :as "w" :tool "tool-fs"
                                    :args (list :op :write :path "never.txt" :data "x"))))))
      (is (search "allow-list" (second (miao:result-error result))))
      (is (not (sandbox-file-exists-p "never.txt"))))))

(test plan-refuses-an-operator-trusted-tool-even-when-allowed
  (call-with-plan '(:tool-fs :tool-shell) 16
    (lambda ()
      (let ((result (plan (list (list :tool "tool-shell" :args (list :cmd "true"))))))
        (is (search "agent-trusted" (second (miao:result-error result))))))))

(test plan-refuses-an-unregistered-tool
  ;; Allowed by name, but nothing is mounted under it.
  (call-with-plan '(:tool-fs :tool-nonexistent) 16
    (lambda ()
      (let ((result (plan (list (list :tool "tool-nonexistent" :args nil)))))
        (is (search "no tool named" (second (miao:result-error result))))))))

(test plans-do-not-nest
  (call-with-plan '(:tool-fs :tool-plan) 16
    (lambda ()
      (let ((result (plan (list (list :tool "tool-plan" :args (list :steps nil))))))
        (is (search "nest" (second (miao:result-error result))))))))

(test plan-refuses-a-duplicate-step-name
  (with-plan
    (let ((result (plan (list (list :as "x" :tool "tool-fs"
                                    :args (list :op :read :path "a.txt"))
                              (list :as "x" :tool "tool-fs"
                                    :args (list :op :read :path "b.txt"))))))
      (is (search "duplicate" (second (miao:result-error result)))))))

(test plan-refuses-a-ref-to-an-unknown-or-later-step
  (with-plan
    (let ((result (plan (list (list :tool "tool-fs"
                                    :args (list :op :read :path (list :ref "later.data")))
                              (list :as "later" :tool "tool-fs"
                                    :args (list :op :read :path "a.txt"))))))
      (is (search "unknown or later step" (second (miao:result-error result)))))))

(test plan-refuses-a-malformed-ref
  (with-plan
    (let ((result (plan (list (list :tool "tool-fs"
                                    :args (list :op :read :path (list :ref "no-dot")))))))
      (is (search "malformed ref" (second (miao:result-error result)))))))

(test plan-refuses-more-than-max-steps
  (call-with-plan '(:tool-fs) 1
    (lambda ()
      (let ((result (plan (list (list :as "a" :tool "tool-fs"
                                      :args (list :op :read :path "a.txt"))
                                (list :as "b" :tool "tool-fs"
                                      :args (list :op :read :path "b.txt"))))))
        (is (search "exceeds" (second (miao:result-error result))))))))

;;; --- a step that fails ends the plan -------------------------------------

(test a-failing-step-ends-the-plan-with-the-results-so-far
  (with-plan
    (let ((result (plan (list (list :as "ok" :tool "tool-fs"
                                    :args (list :op :write :path "a.txt" :data "x"))
                              (list :tool "tool-fs" :args (list :op :bogus))
                              (list :tool "tool-fs"
                                    :args (list :op :write :path "never.txt" :data "x"))))))
      (is (miao:result-error-p result))
      (let ((detail (miao:result-error result)))
        (is (equal 2 (getf detail :step)))
        (is (equal "tool-fs" (getf detail :tool)))
        (is (member :ok (getf detail :results))))
      (is (not (sandbox-file-exists-p "never.txt"))))))

;;; --- the whole-plan deadline, held over every step ----------------------

(defun timed-out-at-step-p (result step)
  (and (miao:result-error-p result)
       (eql step (getf (miao:result-error result) :step))
       (eq :timeout (getf (miao:result-error result) :reason))))

(test plan-honours-its-timeout-between-steps
  (call-with-plan '(:tool-fs :tool-sleep) 16
    (lambda ()
      (let ((result (plan (list (list :as "s" :tool "tool-sleep" :args (list :ms 50))
                                (list :tool "tool-fs"
                                      :args (list :op :write :path "never.txt" :data "x")))
                          1)))
        (is (timed-out-at-step-p result 1))
        (is (not (sandbox-file-exists-p "never.txt")))))
    :sleep-tool t))

(test a-long-step-is-bounded-by-the-plan-timeout
  (setf *saw-cancel* nil)
  (call-with-plan '(:tool-fs :tool-patient) 16
    (lambda ()
      (let* ((start (get-internal-real-time))
             (result (plan (list (list :tool "tool-patient" :args (list :ms 2000))
                                 (list :tool "tool-fs"
                                       :args (list :op :write :path "never.txt" :data "x")))
                           200))
             (elapsed-ms (floor (* 1000 (- (get-internal-real-time) start))
                                internal-time-units-per-second)))
        (is (timed-out-at-step-p result 1))
        (is (< elapsed-ms 1500))
        (is (not (sandbox-file-exists-p "never.txt")))
        (sleep 0.2)
        (is-true *saw-cancel*)))
    :extra '(tool-patient)))

(defun plan-ok-eventually (steps)
  "PLAN STEPS, retried for a few seconds while the tool it names is still
being restarted."
  (loop repeat 50
        for result = (plan steps)
        unless (miao:result-error-p result) do (return result)
        do (sleep 0.1)))

(test a-step-that-ignores-cancel-is-killed-and-restarted
  (call-with-plan '(:tool-fs :tool-stubborn) 16
    (lambda ()
      (let* ((before (m:lookup :tool-stubborn))
             (result (plan (list (list :tool "tool-stubborn" :args (list :ms 30000)))
                           200)))
        (is (timed-out-at-step-p result 1))
        ;; M:KILL interrupts the thread and returns, so the exit lands a moment later.
        (is-true (eventually (lambda () (not (m:process-alive-p before)))))
        (let ((again (plan-ok-eventually
                      (list (list :as "s" :tool "tool-stubborn" :args (list :ms 10))))))
          (is (not (null again)))
          (is (eq :ok (first again))))))
    :extra '(tool-stubborn)))

(test a-step-whose-tool-would-not-be-restarted-is-left-running
  (call-with-plan '(:tool-fs :tool-stubborn) 16
    (lambda ()
      (let* ((before (m:lookup :tool-stubborn))
             (result (plan (list (list :tool "tool-stubborn" :args (list :ms 3000))) 200)))
        (is (timed-out-at-step-p result 1))
        (is (m:process-alive-p before))))
    :extra '((tool-stubborn :restart :temporary))))

(test a-step-that-honours-cancel-is-not-killed
  (call-with-plan '(:tool-fs :tool-patient) 16
    (lambda ()
      (let* ((before (m:lookup :tool-patient))
             (result (plan (list (list :tool "tool-patient" :args (list :ms 5000))) 200)))
        (is (timed-out-at-step-p result 1))
        (sleep 0.2)
        (is (m:process-alive-p before))
        (is (eq before (m:lookup :tool-patient)))))
    :extra '(tool-patient)))

(test a-step-timeout-is-clamped-to-the-plan-deadline
  (call-with-plan '(:tool-timeout-echo) 16
    (lambda ()
      (let ((result (plan (list (list :as "own" :tool "tool-timeout-echo"
                                      :args (list :timeout 20000))
                                (list :as "default" :tool "tool-timeout-echo"
                                      :args nil))
                          5000)))
        (is (not (miao:result-error-p result)))
        (is (<= (getf (getf (plan-results result) :own) :timeout) 5000))
        (is (<= (getf (getf (plan-results result) :default) :timeout) 5000))))
    :extra '(tool-timeout-echo)))

;;; --- (:quote x) passes x as it is --------------------------------------

(defun echoed (value)
  "What tool-plan-echo answers when a plan step passes it VALUE."
  (call-with-plan '(:tool-plan-echo) 16
    (lambda ()
      (getf (getf (plan-results (plan (list (list :as "e" :tool "tool-plan-echo"
                                                  :args (list :value value)))))
                  :e)
            :value))
    :extra '(tool-plan-echo)))

(test quote-passes-a-ref-shape-literally
  (is (equal '(:ref "x.y") (echoed '(:quote (:ref "x.y"))))))

(test quote-nests
  (is (equal '(:quote 1) (echoed '(:quote (:quote 1))))))

(test a-marker-shaped-plist-tail-is-not-a-marker
  (is (equal '(:x 1 :ref "a.b") (echoed '(:x 1 :ref "a.b"))))
  (is (equal '(:x 1 :quote 2) (echoed '(:x 1 :quote 2)))))

(test a-ref-inside-a-list-still-resolves
  (call-with-plan '(:tool-plan-echo) 16
    (lambda ()
      (let ((result (plan (list (list :as "a" :tool "tool-plan-echo" :args (list :value "hi"))
                                (list :as "b" :tool "tool-plan-echo"
                                      :args (list :value (list 1 (list :ref "a.value"))))))))
        (is (equal '(1 "hi") (getf (getf (plan-results result) :b) :value)))))
    :extra '(tool-plan-echo)))

(test a-quoted-ref-is-not-checked-against-earlier-steps
  (is (not (null (echoed '(:quote (:ref "nobody.knows")))))))

(test a-cancelled-plan-refuses-its-remaining-steps
  (call-with-plan '(:tool-fs :tool-sleep) 16
                  (lambda ()
                    (let ((token (miao:make-cancel-token)))
                      (bt:make-thread (lambda () (sleep 0.2) (miao:cancel token)))
                      (let ((result (miao:invoke-tool
                                     :tool-plan :cancel token
                                     :steps (list (list :tool "tool-sleep" :args (list :ms 600))
                                                  (list :tool "tool-fs"
                                                        :args (list :op :write :path "after.txt"
                                                                    :data "x"))))))
                        (is (eql 2 (getf (miao:result-error result) :step)))
                        (is (eq :cancelled (getf (miao:result-error result) :reason)))
                        (is (not (sandbox-file-exists-p "after.txt"))))))
                  :sleep-tool t))
