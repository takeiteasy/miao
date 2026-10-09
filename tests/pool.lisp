(in-package #:miao/tests)
(in-suite :miao)

;;; The shared worker pools: thread reuse, each pool's cap, completions nested
;;; by depth, and withdrawing a job that has not started.

(defun pool-threads ()
  (remove-if-not (lambda (thread) (search "miao-pool" (or (bt:thread-name thread) "")))
                 (bt:all-threads)))

(test the-pool-reuses-threads
  (with-protocol
    (apply #'miao:complete :protocol-echo (hello))
    (let ((spawned (getf (miao:pool-stats 0) :spawned)))
      (dotimes (i 10)
        (is (eq :ok (first (apply #'miao:complete :protocol-echo (hello))))))
      (is (<= (- (getf (miao:pool-stats 0) :spawned) spawned) 1)))))

(test a-tier-never-exceeds-its-size
  (with-pool-sizes (0 2)
    (with-protocol
      (let* ((peak 0)
             (done nil)
             (poller (bt:make-thread
                      (lambda ()
                        (loop until done
                              do (setf peak (max peak (getf (miao:pool-stats 0) :threads)))
                                 (sleep 0.01)))))
             (start (get-internal-real-time))
             (results (concurrently 5 (lambda ()
                                        (apply #'miao:complete :protocol-echo
                                               (hello :delay 0.3))))))
        (setf done t)
        (bt:join-thread poller)
        (is (every (lambda (result) (eq :ok (first result))) results))
        (is (<= peak 2))
        (is (>= (elapsed-since start) 0.85))))))

(test one-thread-per-depth-does-not-deadlock
  (with-pool-sizes (0 1 1 1)
    (call-with-echo-provider
     (lambda (context)
       (let* ((agent (in-thread (lambda ()
                                  (miao:run-agent context :model :test-echo
                                                          :tools '()
                                                          :messages '((:role :user :content "hi"))))))
              (results (concurrently 3 (lambda () (turn :test-echo :delay 0.1)))))
         (is (every (lambda (result) (eq :ok (first result))) results))
         (is (eq :stop (getf (second (bt:join-thread agent)) :stop-reason))))))))

;;; A protocol whose body completes on another waits a depth below itself.

(m:defservice protocol-router (miao:completion-host) ()
  (:name :protocol-router))

(defmethod m:metadata ((service protocol-router))
  (list :kind :protocol :name :protocol-router :summary "Complete on :TARGET"))

(miao:define-protocol-handler protocol-router (service request)
  (let ((target (getf request :target :protocol-echo)))
    (apply #'miao:complete target (hello :delay 0.1 :target target))))

(test a-protocol-that-completes-does-not-wait-in-its-own-pool
  (with-pool-sizes (0 1 1 1)
    (with-protocol
      (m:mount *protocol-context* 'protocol-router)
      (let ((results (concurrently 3 (lambda ()
                                       (miao:complete :protocol-router
                                                      :messages '((:role :user :content "hi"))
                                                      :timeout 3000)))))
        (is (every (lambda (result) (eq :ok (first result))) results))))))

(test completions-nested-past-the-limit-are-refused
  (with-protocol
    (m:mount *protocol-context* 'protocol-router)
    (let ((result (miao:complete :protocol-router :target :protocol-router
                                 :messages '((:role :user :content "hi"))
                                 :timeout 5000)))
      (is (eq :bad-request (first (miao:result-error result)))))))

(test withdrawing-is-exact
  (let* ((pool (miao::%make-pool :test 1))
         (release (bt:make-semaphore))
         (blocker (miao::make-pool-job (lambda () (bt:wait-on-semaphore release :timeout 5))))
         (queued (miao::make-pool-job (lambda ()))))
    (unwind-protect
         (progn
           (is-false (miao::pool-submit pool blocker))
           (is-true (eventually (lambda () (eq :running (miao::pool-job-state blocker)))))
           (is-true (miao::pool-submit pool queued))
           (is-true (miao::pool-withdraw queued))
           (is-false (miao::pool-withdraw queued))
           (is-false (miao::pool-withdraw blocker)))
      (bt:signal-semaphore release)
      (bt:with-lock-held ((miao::pool-lock pool))
        (setf (miao::pool-retiring pool) t)
        (bt:condition-broadcast (miao::pool-cv pool))))
    (is-true (eventually (lambda () (zerop (miao::pool-threads pool)))))))

(test a-key-at-its-limit-holds-no-thread
  (let* ((pool (miao::%make-pool :test 4))
         (release (bt:make-semaphore))
         (jobs (loop repeat 3
                     collect (miao::make-pool-job
                              (lambda () (bt:wait-on-semaphore release :timeout 5))
                              :key :k :limit 1))))
    (unwind-protect
         (progn
           (dolist (job jobs) (miao::pool-submit pool job))
           (sleep 0.1)
           (is (= 1 (miao::pool-threads pool)))
           (is (= 2 (length (miao::pool-queue pool)))))
      (dotimes (i 3) (bt:signal-semaphore release))
      (is-true (eventually (lambda () (null (miao::pool-queue pool)))))
      (bt:with-lock-held ((miao::pool-lock pool))
        (setf (miao::pool-retiring pool) t)
        (bt:condition-broadcast (miao::pool-cv pool))))))

(test idle-threads-retire
  (with-protocol
    (apply #'miao:complete :protocol-echo (hello)))
  (miao::retire-idle-workers)
  (is-true (eventually (lambda () (null (pool-threads))))))

(test a-stalled-sink-is-thrown-out-of-a-full-sink-pool
  (with-pool-sizes (:sink 1)
    (let* ((stuck (bt:make-semaphore))
           (seen '())
           (blocked (miao::start-emitter (lambda (event)
                                           (declare (ignore event))
                                           (bt:wait-on-semaphore stuck :timeout 60))))
           (waiting (miao::start-emitter (lambda (event) (push event seen)))))
      (unwind-protect
           (progn
             (miao::emitter-send blocked :a)
             (is-true (eventually (lambda () (plusp (getf (miao:pool-stats :sink) :running)))))
             (miao::emitter-send waiting :b)
             (miao::stop-emitter waiting)
             (miao::stop-emitter blocked)
             (is-false (miao::await-emitter waiting 0.2))
             (miao::reap-emitter blocked 0.2)
             (is-true (miao::await-emitter waiting 3))
             (is (equal '(:b) seen))
             (is-true (eventually #'sinks-idle-p 5)))
        (bt:signal-semaphore stuck :count 2)))))

(test an-emitter-delivers-in-order-and-then-releases-its-thread
  (let* ((seen '())
         (emitter (miao::start-emitter (lambda (event) (push event seen)))))
    (dotimes (i 50) (miao::emitter-send emitter i))
    (miao::stop-emitter emitter)
    (is-true (miao::await-emitter emitter 3))
    (is (equal (loop for i below 50 collect i) (reverse seen)))
    (is-true (eventually #'sinks-idle-p 5))))

;;; A job stuck where no interrupt lands is abandoned a grace period past its
;;; deadline: its caller is answered and its slot is free again.

(test a-job-stuck-past-its-deadline-is-abandoned-and-its-slot-reused
  (with-pool-sizes (0 1)
    (with-protocol
      (let ((grace miao::*pool-abandon-grace*)
            (abandoned (getf (miao:pool-stats 0) :abandoned))
            (start (get-internal-real-time)))
        ;; Pooled threads read the global value, not a binding made here.
        (setf miao::*pool-abandon-grace* 0.2)
        (unwind-protect
             (let ((stuck (in-thread (lambda ()
                                       (apply #'miao:complete :protocol-echo
                                              (hello :stall 1.5 :timeout 300))))))
               (is (eq :timeout (miao:result-error (bt:join-thread stuck))))
               (is (< (elapsed-since start) 1.2))
               (is (eq :ok (first (apply #'miao:complete :protocol-echo (hello)))))
               (sleep 1.3)
               (let ((stats (miao:pool-stats 0)))
                 (is (<= 0 (getf stats :threads) 1))
                 (is (<= 0 (getf stats :running) 1))
                 (is (= 1 (- (getf stats :abandoned) abandoned)))))
          (setf miao::*pool-abandon-grace* grace))))))

;;; A thread a body spawns makes its completions at the body's depth once it is
;;; wrapped, and at depth 0 when it is not.

(defvar *spawned-depths* nil)

(m:defservice protocol-spawner (miao:completion-host) ()
  (:name :protocol-spawner))

(defmethod m:metadata ((service protocol-spawner))
  (list :kind :protocol :name :protocol-spawner :summary "Record a spawned thread's depth"))

(miao:define-protocol-handler protocol-spawner (service request)
  (flet ((depth-in-thread (wrap)
           (let ((depth nil))
             (bt:join-thread
              (bt:make-thread (funcall wrap (lambda () (setf depth miao::*completion-depth*)))))
             depth)))
    (setf *spawned-depths*
          (list (depth-in-thread #'miao:carry-completion-depth)
                (depth-in-thread #'identity)))
    (list :ok (list :role :assistant :content "" :tool-calls nil :done t))))

(test a-spawned-thread-keeps-the-completion-depth-only-when-wrapped
  (with-protocol
    (m:mount *protocol-context* 'protocol-spawner)
    (miao:complete :protocol-spawner :messages '((:role :user :content "hi")))
    (is (equal '(0 nil) *spawned-depths*))))

;;; A pool holding its cap of abandoned threads still stuck refuses new work
;;; until one returns.

(test a-pool-full-of-stuck-threads-refuses-completions-until-one-returns
  (with-pool-sizes (0 2)
    (with-protocol
      (let ((grace miao::*pool-abandon-grace*)
            (cap miao::*pool-max-abandoned*))
        (setf miao::*pool-abandon-grace* 0.1
              miao::*pool-max-abandoned* 1)
        (unwind-protect
             (progn
               (is (eq :timeout (miao:result-error
                                 (apply #'miao:complete :protocol-echo
                                        (hello :stall 1.2 :timeout 200)))))
               (is-true (eventually (lambda () (= 1 (getf (miao:pool-stats 0) :stuck)))))
               (is (eq :unavailable (miao:result-error
                                     (apply #'miao:complete :protocol-echo (hello)))))
               (is-true (eventually (lambda () (zerop (getf (miao:pool-stats 0) :stuck))) 3))
               (is (eq :ok (first (apply #'miao:complete :protocol-echo (hello))))))
          (setf miao::*pool-abandon-grace* grace
                miao::*pool-max-abandoned* cap))))))
