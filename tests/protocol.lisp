(in-package #:miao/tests)
(in-suite :miao)

;;; The protocol convention exercised through the real registry and service
;;; stack, against an echo protocol that implements the contract and nothing
;;; else: discovery by registration props, pre-flight checking on both the
;;; COMPLETE and the bare M:CALL paths, content normalisation, and the
;;; streaming vocabulary.

(m:defservice protocol-echo (miao:completion-host) ()
  (:name :protocol-echo))

(defmethod m:metadata ((service protocol-echo))
  (list :kind :protocol
        :name :protocol-echo
        :summary "Echo the last user message"
        :params '((:temperature number :doc "sampling temperature"))))

(defun echo-hold (request)
  "Park until REQUEST's cancel token fires."
  (loop until (miao:cancelled-p (getf request :cancel)) do (sleep 0.01))
  (list :error :cancelled))

(defvar *echo-runs* 0 "How many completions the echo protocol has begun.")

(miao:define-protocol-handler protocol-echo (service request)
  (incf *echo-runs*)
  (when (getf request :delay) (sleep (getf request :delay)))
  (when (getf request :stall)
    (sb-sys:without-interrupts (sleep (getf request :stall))))
  (when (getf request :boom) (error "boom"))
  (if (getf request :hold)
      (echo-hold request)
      (let* ((ref (getf request :ref))
             (text (miao:content-text
                    (getf (car (last (getf request :messages))) :content))))
        (when (getf request :stream)
          (miao:emit-event (getf request :stream) (miao:text-delta ref text))
          (miao:emit-event (getf request :stream) (miao:text-delta ref "!"))
          (miao:emit-event (getf request :stream) (miao:done ref :stop)))
        (list :ok (list :role :assistant
                        :content (miao:normalize-content
                                  (concatenate 'string text "!"))
                        :tool-calls nil
                        :done t
                        :meta (list :echoed (length (getf request :messages))
                                    :timeout (getf request :timeout)))))))

(defvar *protocol-context* nil)

(defun call-with-protocol (body &rest mount-args)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :protocols)
                                   :registry registry)))
    (setf *protocol-context* context)
    (unwind-protect
         (progn (apply #'m:mount context 'protocol-echo mount-args)
                (funcall body))
      (m:stop context))))

(defmacro with-protocol (&body body)
  `(call-with-protocol (lambda () ,@body)))

(defmacro with-capped-protocol ((max-in-flight) &body body)
  `(call-with-protocol (lambda () ,@body) :max-in-flight ,max-in-flight))

(defun hello (&rest extra)
  (append (list :messages '((:role :user :content "hello"))) extra))

;;; --- the convention --------------------------------------------------

(test protocols-are-discoverable-via-props
  (with-protocol
    (is (equal '(:protocol-echo) (miao:protocols)))
    (is (null (miao:tools)))))

(test protocol-describes-itself
  (with-protocol
    (let ((metadata (miao:describe-protocol :protocol-echo)))
      (is (eq :protocol (getf metadata :kind)))
      (is (stringp (getf metadata :summary)))
      (is (eq :temperature (caar (getf metadata :params)))))))

(test complete-performs-one-turn
  (with-protocol
    (let ((result (apply #'miao:complete :protocol-echo (hello))))
      (is (eq :ok (first result)))
      (let ((reply (second result)))
        (is (eq :assistant (getf reply :role)))
        (is (eq t (getf reply :done)))
        (is (equal "hello!" (miao:content-text (getf reply :content))))
        (is (= 1 (getf (getf reply :meta) :echoed)))))))

(test unknown-request-keys-are-ignored
  ;; A portable caller may offer a superset: a key the protocol does not
  ;; know must pass through rather than being rejected.
  (with-protocol
    (is (eq :ok (first (apply #'miao:complete :protocol-echo
                              (hello :top-k 40 :seed 7)))))))

(test content-blocks-and-flat-strings-agree
  (with-protocol
    (is (equal (miao:complete :protocol-echo
                              :messages '((:role :user :content "hello")))
               (miao:complete
                :protocol-echo
                :messages '((:role :user
                             :content ((:type :text :text "hello")))))))))

;;; --- pre-flight -------------------------------------------------------

(defun bad-request-p (result)
  (let ((reason (miao:tool-error result)))
    (and (consp reason) (eq :bad-request (first reason)))))

(test a-malformed-request-never-reaches-the-protocol
  (with-protocol
    (is (bad-request-p (miao:complete :protocol-echo)))
    (is (bad-request-p (miao:complete :protocol-echo :messages '())))
    (is (bad-request-p (miao:complete :protocol-echo
                                      :messages '((:role :bard :content "x")))))
    (is (bad-request-p (miao:complete :protocol-echo
                                      :messages '((:role :tool :content "x")))))
    (is (bad-request-p
         (miao:complete :protocol-echo
                        :messages '((:role :assistant
                                     :tool-calls ((:name :tool-shell)))))))))

(test a-bare-call-is-checked-too
  ;; The check lives in the handler as well, so reaching a protocol without
  ;; COMPLETE cannot skip it.
  (with-protocol
    (is (bad-request-p
         (m:call (m:lookup :protocol-echo)
                 '(:complete :messages ((:role :bard :content "x"))))))
    (is (bad-request-p (m:call (m:lookup :protocol-echo) '(:sing))))))

(test the-four-roles-are-accepted
  (with-protocol
    (is (eq :ok (first (miao:complete
                        :protocol-echo
                        :messages '((:role :system :content "be terse")
                                    (:role :user :content "ls")
                                    (:role :assistant :content nil
                                     :tool-calls ((:id "c1" :name :tool-shell
                                                   :arguments (:cmd "ls"))))
                                    (:role :tool :tool-call-id "c1"
                                     :content "a.lisp"))))))))

;;; --- streaming --------------------------------------------------------

(test streaming-to-a-function-sink
  (with-protocol
    (let* ((events '())
           (result (apply #'miao:complete :protocol-echo
                          (hello :ref :r1
                                 :stream (lambda (event) (push event events))))))
      (setf events (nreverse events))
      (is (eq :ok (first result)))
      (is (equal '(:text-delta :text-delta :done)
                 (mapcar (lambda (event) (getf event :type)) events)))
      (is (every (lambda (event) (eq :r1 (getf event :ref))) events))
      (is (equal "hello" (getf (first events) :text)))
      (is (eq :stop (getf (third events) :reason))))))

(test streaming-to-a-process-sink
  (with-protocol
    (let* ((collected '())
           (done (bt:make-semaphore))
           (sink (m:spawn (lambda ()
                            (loop for event = (m:receive :timeout 5)
                                  while event
                                  do (push event collected)
                                  until (eq :done (getf event :type))
                                  finally (bt:signal-semaphore done)))
                          :name "protocol-sink")))
      (apply #'miao:complete :protocol-echo (hello :ref 7 :stream sink))
      (is (bt:wait-on-semaphore done :timeout 5))
      (is (= 3 (length collected)))
      (is (eq :done (getf (first collected) :type))))))

(test a-null-sink-drops-events
  (is (equal '(:type :done :ref nil :reason nil)
             (miao:emit-event nil (miao:done nil)))))


;;; --- concurrency ------------------------------------------------------

(defun elapsed-since (start)
  (/ (- (get-internal-real-time) start) internal-time-units-per-second))

(defun in-thread (function)
  "FUNCTION on a new thread, against the registry in force here."
  (let ((registry m:*registry*))
    (bt:make-thread (lambda ()
                      (let ((m:*registry* registry))
                        (funcall function))))))

(defun concurrently (count function)
  "Call FUNCTION on COUNT threads at once; the list of what each returned."
  (let ((results (make-list count)))
    (mapc #'bt:join-thread
          (loop for cell on results
                collect (let ((cell cell))
                          (in-thread (lambda () (setf (car cell) (funcall function)))))))
    results))

(test completions-run-concurrently
  (with-protocol
    (let* ((start (get-internal-real-time))
           (results (concurrently 3 (lambda ()
                                      (apply #'miao:complete :protocol-echo
                                             (hello :delay 0.5))))))
      (is (every (lambda (result) (eq :ok (first result))) results))
      (is (< (elapsed-since start) 1.2)))))

(test a-service-describes-itself-while-a-completion-is-in-flight
  (with-protocol
    (let* ((token (miao:make-cancel-token))
           (thread (in-thread
                    (lambda ()
                      (apply #'miao:complete :protocol-echo
                             (hello :hold t :cancel token))))))
      (sleep 0.1)
      (let ((start (get-internal-real-time)))
        (is (eq :protocol (getf (miao:describe-protocol :protocol-echo) :kind)))
        (is (< (elapsed-since start) 0.5)))
      (miao:cancel token)
      (is (eq :cancelled (miao:tool-error (bt:join-thread thread)))))))

(test a-worker-that-signals-answers-its-caller
  (with-protocol
    (let ((start (get-internal-real-time))
          (result (apply #'miao:complete :protocol-echo (hello :boom t))))
      (is (miao:tool-error-p result))
      (is (< (elapsed-since start) 2)))))

(test stopping-a-service-cancels-what-it-has-in-flight
  (with-protocol
    (let* ((thread (in-thread
                    (lambda ()
                      (apply #'miao:complete :protocol-echo
                             (hello :hold t :timeout 30000)))))
           (start (get-internal-real-time)))
      (sleep 0.1)
      (m:unmount *protocol-context* :protocol-echo)
      (is (eq :cancelled (miao:tool-error (bt:join-thread thread))))
      (is (< (elapsed-since start) 3)))))

;;; --- the in-flight cap -------------------------------------------------

(defun echo-meta (result) (getf (second result) :meta))

(test max-in-flight-queues-past-the-cap
  (with-capped-protocol (2)
    (let* ((start (get-internal-real-time))
           (results (concurrently 3 (lambda ()
                                      (apply #'miao:complete :protocol-echo
                                             (hello :delay 0.5))))))
      (is (every (lambda (result) (eq :ok (first result))) results))
      (is (<= 0.9 (elapsed-since start) 1.6)))))

(defun hold-one (&rest extra)
  "Start a held completion on a thread of its own; its thread and token."
  (let* ((token (miao:make-cancel-token))
         (thread (in-thread (lambda ()
                              (apply #'miao:complete :protocol-echo
                                     (apply #'hello :hold t :cancel token extra))))))
    (sleep 0.1)
    (values thread token)))

(test a-queued-completion-can-be-cancelled
  (with-capped-protocol (1)
    (multiple-value-bind (held held-token) (hold-one)
      (let* ((token (miao:make-cancel-token))
             (runs *echo-runs*)
             (queued (in-thread (lambda ()
                                  (apply #'miao:complete :protocol-echo
                                         (hello :cancel token))))))
        (sleep 0.1)
        (let ((start (get-internal-real-time)))
          (miao:cancel token)
          (is (eq :cancelled (miao:tool-error (bt:join-thread queued))))
          (is (< (elapsed-since start) 0.5)))
        (is (= runs *echo-runs*))
        (miao:cancel held-token)
        (is (eq :cancelled (miao:tool-error (bt:join-thread held))))))))

(test queued-time-counts-against-the-timeout
  (with-capped-protocol (1)
    (let ((busy (in-thread (lambda ()
                             (apply #'miao:complete :protocol-echo (hello :delay 0.6))))))
      (sleep 0.1)
      (let ((start (get-internal-real-time))
            (result (apply #'miao:complete :protocol-echo (hello :timeout 200))))
        (is (eq :timeout (miao:tool-error result)))
        (is (< (elapsed-since start) 0.45)))
      (is (eq :ok (first (bt:join-thread busy)))))))

(test a-streamed-completion-that-times-out-queued-ends-with-one-done
  (with-capped-protocol (1)
    (let ((busy (in-thread (lambda ()
                             (apply #'miao:complete :protocol-echo (hello :delay 0.6)))))
          (lock (bt:make-lock))
          (events '()))
      (sleep 0.1)
      (apply #'miao:complete :protocol-echo
             (hello :timeout 200 :ref :r1
                    :stream (lambda (event) (bt:with-lock-held (lock) (push event events)))))
      (is-true (eventually (lambda () (bt:with-lock-held (lock) events))))
      (sleep 0.1)
      (is (equal '((:type :done :ref :r1 :reason (:error :timeout))) events))
      (bt:join-thread busy))))

(test a-job-that-starts-late-gets-the-time-left
  (with-capped-protocol (1)
    (let ((busy (in-thread (lambda ()
                             (apply #'miao:complete :protocol-echo (hello :delay 0.3))))))
      (sleep 0.05)
      (let ((result (apply #'miao:complete :protocol-echo (hello :timeout 2000))))
        (is (eq :ok (first result)))
        (is (< (getf (echo-meta result) :timeout) 1800)))
      (bt:join-thread busy))))

(test stopping-a-service-cancels-queued-completions
  (with-capped-protocol (1)
    (let* ((held (in-thread (lambda ()
                              (apply #'miao:complete :protocol-echo
                                     (hello :hold t :timeout 30000)))))
           (queued (progn (sleep 0.1)
                          (in-thread (lambda ()
                                       (apply #'miao:complete :protocol-echo
                                              (hello :timeout 30000))))))
           (start (get-internal-real-time)))
      (sleep 0.1)
      (m:unmount *protocol-context* :protocol-echo)
      (is (eq :cancelled (miao:tool-error (bt:join-thread held))))
      (is (eq :cancelled (miao:tool-error (bt:join-thread queued))))
      (is (< (elapsed-since start) 3)))))

(test a-queued-job-that-signals-still-answers
  (with-capped-protocol (1)
    (let ((busy (in-thread (lambda ()
                             (apply #'miao:complete :protocol-echo (hello :delay 0.2)))))
          (start (get-internal-real-time)))
      (sleep 0.05)
      (let ((result (apply #'miao:complete :protocol-echo (hello :boom t))))
        (is (equal '(:error "boom") (miao:tool-error result)))
        (is (< (elapsed-since start) 1)))
      (bt:join-thread busy))))

(test describe-answers-while-completions-are-queued
  (with-capped-protocol (1)
    (multiple-value-bind (held held-token) (hold-one)
      (let* ((token (miao:make-cancel-token))
             (queued (in-thread (lambda ()
                                  (apply #'miao:complete :protocol-echo
                                         (hello :cancel token))))))
        (sleep 0.1)
        (let ((start (get-internal-real-time)))
          (is (eq :protocol (getf (miao:describe-protocol :protocol-echo) :kind)))
          (is (< (elapsed-since start) 0.5)))
        (miao:cancel token)
        (miao:cancel held-token)
        (bt:join-thread queued)
        (bt:join-thread held)))))

(test a-cast-complete-runs-nothing
  (with-protocol
    (let ((runs *echo-runs*))
      (m:cast (m:lookup :protocol-echo) (list* :complete (hello)))
      (sleep 0.2)
      (is (= runs *echo-runs*)))))

(test max-in-flight-must-be-a-positive-integer
  (signals error (call-with-protocol (lambda ()) :max-in-flight 0)))

;;; --- results ----------------------------------------------------------

(test backend-error-is-its-own-shape
  (let ((result (miao:backend-error 429 "rate limited")))
    (is (miao:tool-error-p result))
    (is (equal '(:backend-error 429 "rate limited") (miao:tool-error result)))))

(test backend-error-carries-a-retry-after-only-when-given
  (is (equal '(:backend-error 429 "slow" :retry-after 2000)
             (miao:tool-error (miao:backend-error 429 "slow" :retry-after 2000)))))

(test retry-after-ms-reads-the-headers
  (flet ((wait (&rest headers) (miao::retry-after-ms headers)))
    (is (= 2000 (wait '(:retry-after . "2"))))
    (is (= 1500 (wait '(:retry-after . "1.5"))))
    (is (= 1500 (wait '(:retry-after-ms . "1500"))))
    (is (= 250 (wait '(:retry-after-ms . "250") '(:retry-after . "9"))))
    (is (null (wait)))
    (is (null (wait '(:retry-after . "soon"))))
    (is (null (wait '(:retry-after . "-3"))))
    (is (null (wait '(:retry-after . ""))))
    (is (= 0 (wait '(:retry-after . "Wed, 21 Oct 2015 07:28:00 GMT"))))))

(test retry-after-ms-reads-an-http-date
  (let ((date (miao::http-date-universal-time "Wed, 21 Oct 2026 07:28:00 GMT")))
    (is (= (encode-universal-time 0 28 7 21 10 2026 0) date))
    (is (null (miao::http-date-universal-time "21 Oct 2026")))
    (is (null (miao::http-date-universal-time "Wed, 21 Foo 2026 07:28:00 GMT")))
    (is (null (miao::http-date-universal-time "Wed, 21 Oct 2026 07:28:00 PST")))))

(test tool-call-deltas-carry-argument-fragments
  (let ((event (miao:tool-call-delta :r :id "c1" :name :tool-shell
                                        :arguments "{\"cmd\"")))
    (is (eq :tool-call-delta (getf event :type)))
    (is (equal "c1" (getf event :id)))
    (is (equal "{\"cmd\"" (getf event :arguments)))))

;;; --- cancel tokens --------------------------------------------------------

(test a-cancel-token-runs-its-actions-once
  (let ((token (miao:make-cancel-token))
        (runs 0))
    (miao::on-cancel token (lambda () (incf runs)))
    (is-false (miao:cancelled-p token))
    (is-true (miao:cancel token))
    (is-false (miao:cancel token))
    (is-true (miao:cancelled-p token))
    (is (= 1 runs))))

(test cancel-answers-true-the-first-time-with-no-actions
  (let ((token (miao:make-cancel-token)))
    (is-true (miao:cancel token))
    (is-false (miao:cancel token))))

(test an-action-registered-after-cancel-runs-at-once
  (let ((token (miao:make-cancel-token))
        (runs 0))
    (miao:cancel token)
    (miao::on-cancel token (lambda () (incf runs)))
    (is (= 1 runs))))

;;; --- the exchange's deadline -------------------------------------------------

(test an-exchange-runs-on-the-calling-thread
  (is (equal (list (bt:current-thread) nil)
             (multiple-value-list
              (miao::call-with-deadline 1000 (lambda (connect)
                                               (declare (ignore connect))
                                               (bt:current-thread)))))))

(test the-deadline-unwinds-an-exchange-that-does-not-return
  (let ((start (get-internal-real-time)))
    (is (equal '(nil :timeout)
               (multiple-value-list
                (miao::call-with-deadline 200 (lambda (connect)
                                                (declare (ignore connect))
                                                (sleep 10))))))
    (is (< (elapsed-since start) 2))))

(test a-cancel-unwinds-an-exchange-that-does-not-return
  (let ((token (miao:make-cancel-token))
        (start (get-internal-real-time)))
    (bt:make-thread (lambda () (sleep 0.2) (miao:cancel token)))
    (is (equal '(nil :cancelled)
               (multiple-value-list
                (miao::call-with-deadline 10000 (lambda (connect)
                                                  (declare (ignore connect))
                                                  (sleep 10))
                                          :cancel token))))
    (is (< (elapsed-since start) 2))))

(test a-finished-exchange-ignores-a-later-cancel-and-deadline
  (let ((token (miao:make-cancel-token)))
    (is (equal '(:done nil)
               (multiple-value-list
                (miao::call-with-deadline 300 (lambda (connect)
                                                (declare (ignore connect))
                                                :done)
                                          :cancel token))))
    (miao:cancel token)
    ;; Past the deadline too: neither may reach this thread.
    (sleep 0.5)
    (is (equal '(:next nil)
               (multiple-value-list
                (miao::call-with-deadline 1000 (lambda (connect)
                                                 (declare (ignore connect))
                                                 (sleep 0.3)
                                                 :next)))))))
