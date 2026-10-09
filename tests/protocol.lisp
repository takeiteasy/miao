(in-package #:miao/tests)
(in-suite :miao)

;;; The completion path exercised through the real registry and service
;;; stack, against an echo backend that implements the contract and nothing
;;; else: discovery by registration props, pre-flight checking on both the
;;; COMPLETE and the bare M:CALL paths, content normalisation, and the
;;; streaming vocabulary.

(defclass echo-backend (ci:protocol-backend) ()
  (:default-initargs
   :name :protocol-echo
   :summary "Echo the last user message"
   :params '((:temperature number :doc "sampling temperature"))))

(defun echo-hold (request)
  "Park until REQUEST's cancel token fires."
  (loop until (miao:cancelled-p (getf request :cancel)) do (sleep 0.01))
  (miao:fail :cancelled))

(defvar *echo-runs* 0 "How many completions the echo protocol has begun.")

(defmethod ci:backend-complete ((backend echo-backend) request)
  (incf *echo-runs*)
  (when (getf request :delay) (sleep (getf request :delay)))
  (when (getf request :stall)
    (sb-sys:without-interrupts (sleep (getf request :stall))))
  (when (getf request :boom) (error "boom"))
  (if (getf request :hold)
      (echo-hold request)
      (let* ((ref (getf request :ref))
             (sink (getf request :stream))
             (text (miao:content-text
                    (getf (car (last (getf request :messages))) :content))))
        (when sink
          (funcall sink (miao:text-delta ref text))
          (funcall sink (miao:text-delta ref "!")))
        (list :ok (list :role :assistant
                        :content (miao:normalize-content
                                  (concatenate 'string text "!"))
                        :tool-calls nil
                        :done t
                        :meta (list :finish-reason :stop
                                    :echoed (length (getf request :messages))
                                    :timeout (getf request :timeout)))))))

(ci:register-backend (make-instance 'echo-backend))

(defvar *protocol-context* nil)

(defun call-with-protocol (body &rest mount-args)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :protocols)
                                   :registry registry)))
    (setf *protocol-context* context)
    (unwind-protect
         (progn (apply #'m:mount context 'miao:backend-service :name :protocol-echo mount-args)
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
  (let ((reason (miao:result-error result)))
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

(test a-null-sink-drops-events
  (is (equal '(:type :done :ref nil :reason nil)
             (miao:deliver-event nil (miao:done nil)))))


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
      (is (eq :cancelled (miao:result-error (bt:join-thread thread)))))))

(test a-worker-that-signals-answers-its-caller
  (with-protocol
    (let ((start (get-internal-real-time))
          (result (apply #'miao:complete :protocol-echo (hello :boom t))))
      (is (miao:result-error-p result))
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
      (is (eq :cancelled (miao:result-error (bt:join-thread thread))))
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
          (is (eq :cancelled (miao:result-error (bt:join-thread queued))))
          (is (< (elapsed-since start) 0.5)))
        (is (= runs *echo-runs*))
        (miao:cancel held-token)
        (is (eq :cancelled (miao:result-error (bt:join-thread held))))))))

(test queued-time-counts-against-the-timeout
  (with-capped-protocol (1)
    (let ((busy (in-thread (lambda ()
                             (apply #'miao:complete :protocol-echo (hello :delay 0.6))))))
      (sleep 0.1)
      (let ((start (get-internal-real-time))
            (result (apply #'miao:complete :protocol-echo (hello :timeout 200))))
        (is (eq :timeout (miao:result-error result)))
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
      (is (eq :cancelled (miao:result-error (bt:join-thread held))))
      (is (eq :cancelled (miao:result-error (bt:join-thread queued))))
      (is (< (elapsed-since start) 3)))))

(test a-queued-job-that-signals-still-answers
  (with-capped-protocol (1)
    (let ((busy (in-thread (lambda ()
                             (apply #'miao:complete :protocol-echo (hello :delay 0.2)))))
          (start (get-internal-real-time)))
      (sleep 0.05)
      (let ((result (apply #'miao:complete :protocol-echo (hello :boom t))))
        (is (equal '(:error "boom") (miao:result-error result)))
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
