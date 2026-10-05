(in-package #:miao/tests)
(in-suite :miao)

(defun count-matches (needle haystack)
  (loop for start = 0 then (+ found (length needle))
        for found = (search needle haystack :start2 start)
        while found count t))

;;; Checkpoints and rollback (~takeiteasy/miao#11): SNAPSHOT/RESTORE across
;;; the shared convention, the generation file on disk, drift reporting, the
;;; agent's own state trimming, and TOOL-CHECKPOINT.

;;; A stub service with real declared state, driven only through messages --
;;; the mechanism under test never touches another process's slots directly.

(m:defservice stateful-thing () ((value :initform 0 :accessor thing-value))
  (:name :stateful-thing))

(defmethod miao:snapshot ((service stateful-thing))
  (list :value (thing-value service)))

(defmethod miao:restore ((service stateful-thing) state)
  (setf (thing-value service) (getf state :value))
  t)

(defmethod m:handle ((service stateful-thing) message)
  (case (first message)
    (:set (setf (thing-value service) (second message)))
    (:get (thing-value service))
    (:snapshot (miao:snapshot service))
    (:restore (miao:restore service (second message)))
    (t nil)))

;;; --- the harness --------------------------------------------------------

(defvar *ckpt-context* nil "The running context, bound by WITH-CHECKPOINTS.")

(defun make-generations-directory ()
  (format nil "~amiao-generations-test-~36r/"
          (namestring (uiop:temporary-directory))
          (random (expt 2 64) (make-random-state t))))

(defmacro with-generations-directory ((dir) &body body)
  `(let ((,dir (make-generations-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,dir)
                                   :validate t :if-does-not-exist :ignore))))

(defun call-with-checkpoints (dir body)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :tools)
                                   :registry registry)))
    (setf *ckpt-context* context)
    (unwind-protect
         (progn
           (m:mount context 'stateful-thing)
           (m:mount context 'miao:tool-fs :root (make-sandbox-directory))
           (m:mount context 'miao:tool-checkpoint :dir dir)
           (funcall body))
      (m:stop context))))

(defmacro with-checkpoints ((dir) &body body)
  `(with-generations-directory (,dir) (call-with-checkpoints ,dir (lambda () ,@body))))

(defun thing () (m:call (m:lookup :stateful-thing) '(:get)))
(defun set-thing (value) (m:call (m:lookup :stateful-thing) (list :set value)))

;;; --- the generation file --------------------------------------------------

(test generation-file-round-trips
  (with-generations-directory (dir)
    (ensure-directories-exist dir)
    (let ((path (merge-pathnames "x.generation" dir))
          (form (list :miao-generation 1 :created "2026-01-01T00:00:00Z"
                      :label "t"
                      :services (list (list :name :a :class "a" :state (list :x 1))))))
      (miao::%write-generation path form)
      (is (equal form (miao::%read-generation path))))))

(test a-tool-call-keeps-its-schema-through-a-generation-file
  (with-tools
   (with-generations-directory (dir)
    (ensure-directories-exist dir)
    (dolist (name '(:tool-http :tool-fs))
      (let* ((path (merge-pathnames (format nil "~(~a~).generation" name) dir))
             (schema (miao:tool-schema (miao:describe-tool name)))
             (call (list :id "c1" :name name :arguments nil :schema schema))
             (form (list :miao-generation 1 :messages
                         (list (list :role :assistant :tool-calls (list call))))))
        (miao::%write-generation path form)
        (is (equal schema
                   (getf (first (getf (first (getf (miao::%read-generation path) :messages))
                                      :tool-calls))
                         :schema))))))))

(test a-schema-two-calls-share-is-written-once-and-read-back-shared
  (with-generations-directory (dir)
    (ensure-directories-exist dir)
    (let* ((path (merge-pathnames "shared.generation" dir))
           (schema (list (list :cmd 'string :required t :doc "run it")))
           (calls (loop for id in '("c1" "c2")
                        collect (list :id id :name :tool-http :arguments nil :schema schema)))
           (form (list :miao-generation 1 :messages
                       (list (list :role :assistant :tool-calls calls)))))
      (miao::%write-generation path form)
      (is (= 1 (count-matches "run it" (uiop:read-file-string path))))
      (let ((read (getf (first (getf (miao::%read-generation path) :messages)) :tool-calls)))
        (is (equal calls read))
        (is (eq (getf (first read) :schema) (getf (second read) :schema)))))))

(test reading-a-generation-never-evaluates
  ;; *READ-EVAL* is nil around the read, the same guard tool-eval's worker
  ;; applies to a submitted form.
  (with-generations-directory (dir)
    (ensure-directories-exist dir)
    (let ((path (merge-pathnames "evil.generation" dir)))
      (with-open-file (stream path :direction :output)
        (write-string "(:miao-generation 1 :services (#.(error \"read-eval ran\")))"
                      stream))
      (signals error (miao::%read-generation path)))))

;;; --- checkpoint / rollback ------------------------------------------------

(test checkpoint-and-rollback-round-trip-a-services-own-state
  (with-checkpoints (dir)
    (set-thing 42)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (set-thing 0)
      (is (eql 0 (thing)))
      (miao:rollback *ckpt-context* path)
      (is (eql 42 (thing))))))

(test a-delegated-sub-agent-is-left-out-of-a-checkpoint
  (with-checkpoints (dir)
    (m:with-process (runner)
      (m:delegate *ckpt-context* 'miao:agent :name :boss/1 :parent-name :boss)
      (is (find :boss/1 (m:children *ckpt-context*) :key (lambda (c) (getf c :name))))
      (let* ((path (miao:checkpoint *ckpt-context* :dir dir))
             (generation (find path (miao:generations :dir dir)
                               :key (lambda (g) (getf g :path))
                               :test (lambda (a b) (equal (namestring a) b)))))
        (is-true generation)
        (is (not (member :boss/1 (getf generation :services))))
        (is (member :tool-fs (getf generation :services)))))))

(test a-service-with-no-snapshot-method-restores-cleanly
  ;; tool-fs takes the default NIL SNAPSHOT/RESTORE; the round trip must not
  ;; error just because there is nothing to carry.
  (with-checkpoints (dir)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (is (member :tool-fs (getf (second (miao:rollback *ckpt-context* path)) :restored))))))

(test rollback-reports-a-service-missing-since-the-checkpoint
  (with-checkpoints (dir)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (m:unmount *ckpt-context* :stateful-thing)
      (is (equal '(:stateful-thing)
                 (getf (second (miao:rollback *ckpt-context* path :remount nil)) :missing))))))

(test rollback-reports-a-class-mismatch-and-does-not-restore-it
  (with-checkpoints (dir)
    (set-thing 9)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (m:unmount *ckpt-context* :stateful-thing)
      ;; A different class mounted under the same name now.
      (m:mount *ckpt-context* 'miao:tool-fs :name :stateful-thing :root (make-sandbox-directory))
      (let ((mismatched (getf (second (miao:rollback *ckpt-context* path)) :mismatched)))
        (is (eql 1 (length mismatched)))
        (is (equal :stateful-thing (getf (first mismatched) :name)))))))

(test checkpoint-keep-prunes-the-oldest-generations
  (with-checkpoints (dir)
    (dotimes (n 3)
      (miao:checkpoint *ckpt-context* :dir dir :label (princ-to-string n)))
    (miao:checkpoint *ckpt-context* :dir dir :keep 2 :label "3")
    (is (equal '("3" "2") (mapcar (lambda (g) (getf g :label)) (miao:generations :dir dir))))))

(test checkpoint-keep-keeps-the-generation-just-written
  (with-checkpoints (dir)
    (dotimes (n 20)
      (let ((path (miao:checkpoint *ckpt-context* :dir dir :keep 1)))
        (is (equal (list (namestring path))
                   (mapcar (lambda (g) (getf g :path)) (miao:generations :dir dir))))))))

;;; --- the agent's own snapshot ---------------------------------------------

(defun agent-snapshot (name)
  (m:call (m:lookup name) '(:snapshot)))

(defun wait-for-agent-turns (name n &optional (deadline 3.0))
  (loop repeat (ceiling deadline 0.05)
        for snap = (agent-snapshot name)
        when (>= (getf snap :turns 0) n) return snap
        do (sleep 0.05)
        finally (return (agent-snapshot name))))

(test agent-checkpoint-restores-mid-run-state-and-lands-not-running
  ;; A snapshot is only interesting taken mid-run. The backend is slowed
  ;; down to hold that window open, and the whole context is checkpointed,
  ;; then rolled back, while :running-p is still t on the very same process.
  ;; protocol-openai is blocked for the run's own 0.3s, so this also shows
  ;; CHECKPOINT does not queue behind it.
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :agents)
                                   :registry registry))
         (server (start-fake-http (lambda (&rest r) (declare (ignore r))
                                    (sleep 0.3) (json-response +hello-reply+)))))
    (unwind-protect
         (with-generations-directory (dir)
           (m:mount context 'miao:protocol-openai)
           (apply #'m:mount context (first (keyed)) :base-url (fake-http-url server) (rest (keyed)))
           (m:mount context 'miao:agent :name :assistant :model :provider-test-keyed)
           (m:cast (m:lookup :assistant) (list :run :messages '((:role :user :content "hi"))))
           (let ((mid-run (wait-for-agent-turns :assistant 1 1.0)))
             (is (eql 1 (getf mid-run :turns)))
             (is (search "hi" (prin1-to-string (getf mid-run :messages))))
             (is (equal '(:turn 1 :tool-calls nil) (getf mid-run :in-flight)))
             (multiple-value-bind (path interrupted) (miao:checkpoint context :dir dir)
               (is (equal '(:assistant) interrupted))
               (is (equal '(:assistant) (getf (first (miao:generations :dir dir)) :interrupted)))
               (is (equal '(:assistant)
                          (getf (second (miao:rollback context path)) :interrupted)))
               (let ((restored (agent-snapshot :assistant)))
                 (is (equal (getf mid-run :messages) (getf restored :messages)))
                 (is (eql 1 (getf restored :turns)))
                 (is (null (getf restored :in-flight))))
               ;; RESTORE always lands a not-running agent: a further :run
               ;; is accepted at once rather than refused as already
               ;; running. START-RUN answers synchronously, before the
               ;; backend is even asked, so this does not wait out the
               ;; 0.3s delay.
               (is (eq :ok (m:call (m:lookup :assistant)
                                   (list :run :messages '((:role :user :content "second")))))))))
      (m:stop context)
      (stop-fake-http server))))

(test agent-snapshot-mid-tool-call-closes-pending-calls
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (context (m:start-service (make-instance 'm:context :name :agents)
                                   :registry registry))
         (server (start-fake-http (lambda (&rest r) (declare (ignore r))
                                    (tool-call-reply "c1" "tool-hold" "{}")))))
    (unwind-protect
         (progn
           (m:mount context 'miao:protocol-openai)
           (apply #'m:mount context (first (keyed)) :base-url (fake-http-url server) (rest (keyed)))
           (m:mount context 'tool-hold)
           (m:mount context 'miao:agent :name :assistant :model :provider-test-keyed
                                        :tools '(:tool-hold))
           (m:cast (m:lookup :assistant) (list :run :messages '((:role :user :content "go"))))
           (let ((snap (loop repeat 100
                             for s = (agent-snapshot :assistant)
                             when (getf (getf s :in-flight) :tool-calls) return s
                             do (sleep 0.02))))
             (is (equal '("c1") (getf (getf snap :in-flight) :tool-calls)))
             (let ((last-message (car (last (getf snap :messages)))))
               (is (eq :tool (getf last-message :role)))
               (is (equal "c1" (getf last-message :tool-call-id)))
               (is (search "interrupted"
                           (miao:content-text (getf last-message :content)))))))
      (m:stop context)
      (stop-fake-http server))))

;;; --- unavailable services -------------------------------------------------

(m:defservice slow-thing () ((value :initform 5 :accessor slow-value))
  (:name :slow-thing))

(defmethod m:handle ((service slow-thing) message)
  (case (first message)
    (:snapshot (sleep 1) (list :value (slow-value service)))
    (:restore (setf (slow-value service) (getf (second message) :value)) t)
    (:get (slow-value service))
    (:set (setf (slow-value service) (second message)))
    (t nil)))

(test a-service-that-misses-the-deadline-is-unavailable-and-left-alone
  (with-checkpoints (dir)
    (m:mount *ckpt-context* 'slow-thing)
    (set-thing 3)
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (path interrupted unavailable)
          (miao:checkpoint *ckpt-context* :dir dir :timeout 0.3)
        (is (< (- (get-internal-real-time) start) (* 0.8 internal-time-units-per-second)))
        (is (null interrupted))
        (is (equal '(:slow-thing) unavailable))
        (is (equal '(:slow-thing) (getf (first (miao:generations :dir dir)) :unavailable)))
        (m:call (m:lookup :slow-thing) '(:set 9))
        (set-thing 0)
        (let ((result (second (miao:rollback *ckpt-context* path))))
          (is (equal '(:slow-thing) (getf result :unavailable)))
          (is (not (member :slow-thing (getf result :restored)))))
        (is (eql 3 (thing)) "the service that answered is restored")
        (is (eql 9 (m:call (m:lookup :slow-thing) '(:get))) "the unavailable one is untouched")))))

(test a-slow-service-does-not-delay-the-others
  (with-checkpoints (dir)
    (m:mount *ckpt-context* 'slow-thing)
    (set-thing 3)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir :timeout 0.3)))
      (let ((entries (getf (miao::%read-generation path) :services)))
        (is (equal '(:value 3)
                   (getf (find :stateful-thing entries
                               :key (lambda (e) (getf e :name)))
                         :state)))))))

(m:defservice slow-restore-thing () ()
  (:name :slow-restore-thing))

(defmethod m:handle ((service slow-restore-thing) message)
  (case (first message)
    (:restore (sleep 1) t)
    (t nil)))

(test a-slow-restore-does-not-delay-the-others
  (with-checkpoints (dir)
    (m:mount *ckpt-context* 'slow-restore-thing)
    (set-thing 3)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (set-thing 0)
      (let* ((start (get-internal-real-time))
             (result (second (miao:rollback *ckpt-context* path :timeout 0.3))))
        (is (< (- (get-internal-real-time) start) (* 0.8 internal-time-units-per-second)))
        (is (equal '(:slow-restore-thing) (getf result :failed)))
        (is (equal '((:slow-restore-thing :timeout)) (getf result :failures)))
        (is (member :stateful-thing (getf result :restored)))
        (is (eql 3 (thing)))))))

;;; --- tool-checkpoint --------------------------------------------------

(test tool-checkpoint-is-operator-trusted
  (with-checkpoints (dir)
    (is (eq :operator (miao:tool-trust (miao:describe-tool :tool-checkpoint))))))

(test tool-checkpoint-saves-lists-and-restores
  (with-checkpoints (dir)
    (set-thing 7)
    (let* ((save (miao:invoke-tool :tool-checkpoint :op :save :label "seven"))
           (path (getf (second save) :path)))
      (is (eq :ok (first save)))
      (let ((listed (getf (second (miao:invoke-tool :tool-checkpoint :op :list)) :generations)))
        (is (find path listed :key (lambda (g) (getf g :path)) :test #'equal))
        (is (equal "seven" (getf (find path listed :key (lambda (g) (getf g :path))
                                       :test #'equal)
                                 :label))))
      (set-thing 0)
      (miao:invoke-tool :tool-checkpoint :op :restore :path path)
      (is (eql 7 (thing))))))

(test tool-checkpoint-save-marks-itself-unavailable-rather-than-hanging
  ;; :save runs inside the tool's own process, so snapshotting it is a
  ;; deadlock M:CALL-ALL refuses at once.
  (with-checkpoints (dir)
    (let* ((start (get-internal-real-time))
           (save (miao:invoke-tool :tool-checkpoint :op :save)))
      (is (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second)))
      (is (eq :ok (first save)))
      (is (equal '(:tool-checkpoint) (getf (second save) :unavailable)))
      (is (null (getf (second save) :interrupted))))))

(test tool-checkpoint-restore-requires-a-path
  (with-checkpoints (dir)
    (is (equal :bad-request
               (first (miao:tool-error (miao:invoke-tool :tool-checkpoint :op :restore)))))))

(test tool-checkpoint-restore-rejects-an-unknown-path
  (with-checkpoints (dir)
    (is (equal :bad-request
               (first (miao:tool-error
                       (miao:invoke-tool :tool-checkpoint :op :restore
                                        :path "/nonexistent/x.generation")))))))

;;; --- per-path log locks (~takeiteasy/miao#65) -----------------------------

(test one-file-under-two-spellings-shares-a-lock
  (with-generations-directory (dir)
    (ensure-directories-exist (format nil "~asub/" dir))
    (let ((a (format nil "~ax.log" dir))
          (b (format nil "~asub/../x.log" dir)))
      (is (eq (miao::%log-lock a) (miao::%log-lock b)))
      (is (not (eq (miao::%log-lock a) (miao::%log-lock (format nil "~ay.log" dir))))))))

(test appending-to-one-log-does-not-wait-on-another
  (with-generations-directory (dir)
    (let* ((a (format nil "~aa.log" dir))
           (b (format nil "~ab.log" dir))
           (lock (miao::%log-lock a))
           (done nil))
      (bt:with-lock-held (lock)
        (bt:make-thread (lambda ()
                          (miao::%append-log b '(:kind :x))
                          (setf done t)))
        (loop repeat 100 until done do (sleep 0.05))
        (is-true done))
      (is (equal '((:kind :x)) (miao::%read-log b))))))

(test read-log-reports-a-torn-tail
  (with-generations-directory (dir)
    (let ((path (format nil "~aa.log" dir)))
      (miao::%append-log path '(:kind :x))
      (is (eq t (nth-value 1 (miao::%read-log path))))
      (with-open-file (s path :direction :output :if-exists :append)
        (write-string "(broken" s))
      (is (equal '((:kind :x)) (miao::%read-log path)))
      (is (null (nth-value 1 (miao::%read-log path)))))))

;;; --- cross-process log lock (~takeiteasy/miao#84) --------------------------

(defmacro with-foreign-log-flock ((path) &body body)
  "BODY run while another file description holds PATH's sidecar flock, as
another process would."
  `(let ((fd (sb-posix:open (format nil "~a.lock" (miao::%log-key ,path))
                            (logior sb-posix:o-creat sb-posix:o-rdwr) #o644)))
     (unwind-protect
          (progn (miao::%flock-exclusive fd) ,@body)
       (sb-posix:close fd))))

(defun blocks-on-foreign-flock-p (path thunk)
  "True when THUNK, run on a thread, waits for PATH's flock and then finishes
once it is released."
  (let (thread finished-early)
    (with-foreign-log-flock (path)
      (setf thread (bt:make-thread thunk))
      (sleep 0.3)
      (setf finished-early (not (bt:thread-alive-p thread))))
    (bt:join-thread thread)
    (not finished-early)))

(test appending-waits-for-a-lock-held-by-another-process
  (with-generations-directory (dir)
    (let ((path (format nil "~aa.log" dir)))
      (miao::%append-log path '(:kind :first))
      (is-true (blocks-on-foreign-flock-p path (lambda () (miao::%append-log path '(:kind :x)))))
      (is (equal '((:kind :first) (:kind :x)) (miao::%read-log path))))))

(test the-lock-sidecar-sits-beside-the-log
  (with-generations-directory (dir)
    (let ((path (format nil "~aa.log" dir)))
      (miao::%append-log path '(:kind :x))
      (is-true (probe-file (format nil "~a.lock" (miao::%log-key path)))))))

;;; --- remounting (~takeiteasy/miao#49) -------------------------------------

(defun rolled-back (path &rest args)
  (second (apply #'miao:rollback *ckpt-context* path args)))

(defun generation-text (path) (uiop:read-file-string path))

(defun service-entry (path name)
  (find name (getf (miao::%read-generation path) :services)
        :key (lambda (entry) (getf entry :name))))

(test rollback-mounts-a-service-unmounted-since-the-checkpoint-again
  (with-checkpoints (dir)
    (set-thing 9)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (m:unmount *ckpt-context* :stateful-thing)
      (let ((result (rolled-back path)))
        (is (equal '(:stateful-thing) (getf result :remounted)))
        (is (null (getf result :missing)))
        (is (null (getf result :unremounted)))
        (is (member :stateful-thing (getf result :restored))))
      (is (= 9 (thing)) "the remounted service has its state back"))))

(test rollback-can-be-told-not-to-remount
  (with-checkpoints (dir)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (m:unmount *ckpt-context* :stateful-thing)
      (let ((result (rolled-back path :remount nil)))
        (is (null (getf result :remounted)))
        (is (equal '(:stateful-thing) (getf result :missing)))
        (is (null (m:lookup :stateful-thing)))))))

(test a-generation-records-how-a-service-was-mounted
  (with-checkpoints (dir)
    (m:unmount *ckpt-context* :tool-fs)
    (m:mount *ckpt-context* 'miao:tool-fs :root "/tmp/x/" :restart :permanent :shutdown 9)
    (let ((entry (service-entry (miao:checkpoint *ckpt-context* :dir dir) :tool-fs)))
      (is (equal "MIAO" (getf entry :package)))
      (is (equal "TOOL-FS" (getf entry :symbol)))
      (is (eq :permanent (getf entry :restart)))
      (is (= 9 (getf entry :shutdown)))
      (is (null (getf entry :parent)))
      (is (search "/tmp/x/" (getf entry :initargs))))))

(test a-remounted-service-keeps-its-mount-options-and-initargs
  (with-checkpoints (dir)
    (m:unmount *ckpt-context* :tool-fs)
    (m:mount *ckpt-context* 'miao:tool-fs :root (make-sandbox-directory) :restart :permanent)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (m:unmount *ckpt-context* :tool-fs)
      (is (equal '(:tool-fs) (getf (rolled-back path) :remounted)))
      (let ((child (find :tool-fs (m:children *ckpt-context*)
                         :key (lambda (c) (getf c :name)))))
        (is (eq :permanent (getf child :restart)))))))

(test a-nested-service-is-mounted-again-under-its-context-and-declared-children-once
  (with-checkpoints (dir)
    (let ((inner (m:mount *ckpt-context* 'm:context :name :inner
                          :children '((stateful-thing :name :declared)))))
      (m:mount inner 'stateful-thing :name :hand)
      (m:call (m:lookup :hand) '(:set 5))
      (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
        (is (equal :inner (getf (service-entry path :hand) :parent)))
        (m:unmount *ckpt-context* :inner)
        (let ((result (rolled-back path)))
          (is (equal '(:inner :hand) (getf result :remounted)))
          (is (null (getf result :unremounted)) "the declared child is not mounted twice")
          (is (null (getf result :missing)))
          (is (member :declared (getf result :restored))))
        (is (= 5 (m:call (m:lookup :hand) '(:get))))))))

(defun mount-keyed-provider (&rest initargs)
  (unless (m:lookup :protocol-openai) (m:mount *ckpt-context* 'miao:protocol-openai))
  (apply #'m:mount *ckpt-context* 'miao/tests::provider-test-keyed
         :model "test-model" :base-url "http://127.0.0.1:1" initargs))

(defun provider-key ()
  (miao::provider-api-key (m:service-of (m:lookup :provider-test-keyed))))

(test a-generation-never-holds-a-credential
  (with-checkpoints (dir)
    (mount-keyed-provider :api-key "sk-very-secret")
    (let* ((path (miao:checkpoint *ckpt-context* :dir dir))
           (entry (service-entry path :provider-test-keyed)))
      (is (null (search "sk-very-secret" (generation-text path))))
      (is (equal '(:api-key) (getf entry :withheld)))
      (is (search ":model" (getf entry :initargs))))))

(test a-provider-remounted-without-its-key-takes-it-back-from-rollback
  (with-checkpoints (dir)
    (mount-keyed-provider :api-key "sk-very-secret")
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (m:unmount *ckpt-context* :provider-test-keyed)
      (is (equal '(:provider-test-keyed) (getf (rolled-back path) :remounted)))
      (is (null (provider-key)))
      (is (equal "test-model" (miao::provider-model
                               (m:service-of (m:lookup :provider-test-keyed)))))
      (m:unmount *ckpt-context* :provider-test-keyed)
      (rolled-back path :initargs '((:provider-test-keyed :api-key "sk-again")))
      (is (equal "sk-again" (provider-key))))))

(test a-credential-inside-a-declared-child-is-left-out-too
  (multiple-value-bind (kept withheld)
      (miao::%persistable-initargs
       'm:context '(:name :c :children ((miao/tests::provider-test-keyed
                                         :api-key "k" :model "m"))))
    (is (equal '(:name :c :children ((miao/tests::provider-test-keyed :model "m"))) kept))
    (is (equal '("provider-test-keyed :api-key") withheld))))

(test a-value-that-cannot-be-read-back-is-left-out
  (with-checkpoints (dir)
    (m:mount *ckpt-context* 'miao:agent :name :a1 :model :nothing :sink (lambda (e) e))
    (let ((entry (service-entry (miao:checkpoint *ckpt-context* :dir dir) :a1)))
      (is (equal '(:sink) (getf entry :withheld)))
      (is (null (search "sink" (getf entry :initargs)))))))

(test an-entry-that-cannot-be-mounted-again-is-reported-and-the-rest-carry-on
  (with-checkpoints (dir)
    (ensure-directories-exist dir)
    (set-thing 4)
    (let ((path (merge-pathnames "ghost.generation" dir)))
      (miao::%write-generation
       path
       (list :miao-generation 2 :created "2026-01-01T00:00:00Z" :label nil
             :services (list (list :name :ghost :class "ghost" :package "NO-SUCH-PACKAGE"
                                   :symbol "GHOST" :parent nil :initargs "nil" :state nil)
                             (list :name :orphan :class "stateful-thing" :package "MIAO/TESTS"
                                   :symbol "STATEFUL-THING" :parent :nowhere :initargs "nil"
                                   :state nil)
                             (list :name :stateful-thing :class "stateful-thing"
                                   :state (list :value 8)))))
      (let ((result (rolled-back path)))
        (is (equal '(:ghost :orphan) (mapcar #'first (getf result :unremounted))))
        (is (search "NO-SUCH-PACKAGE" (second (first (getf result :unremounted)))))
        (is (search "nowhere" (second (second (getf result :unremounted)))))
        (is (equal '(:ghost :orphan) (getf result :missing)))
        (is (equal '(:stateful-thing) (getf result :restored)))
        (is (= 8 (thing)))))))

(test a-version-1-generation-still-rolls-back
  (with-checkpoints (dir)
    (ensure-directories-exist dir)
    (let ((path (merge-pathnames "old.generation" dir)))
      (miao::%write-generation
       path
       (list :miao-generation 1 :created "2026-01-01T00:00:00Z" :label nil
             :services (list (list :name :old-thing :class "stateful-thing" :state nil)
                             (list :name :stateful-thing :class "stateful-thing"
                                   :state (list :value 3)))))
      (let ((result (rolled-back path)))
        (is (equal '(:old-thing) (getf result :missing)))
        (is (null (getf result :remounted)))
        (is (null (getf result :unremounted)))
        (is (= 3 (thing)))))))

(defun mount-inner-with-a-keyed-child ()
  (unless (m:lookup :protocol-openai) (m:mount *ckpt-context* 'miao:protocol-openai))
  (m:mount *ckpt-context* 'm:context :name :inner
           :children '((miao/tests::provider-test-keyed
                        :model "test-model" :base-url "http://127.0.0.1:1"
                        :api-key "sk-nested"))))

(test a-credential-in-a-declared-child-is-never-written
  (with-checkpoints (dir)
    (mount-inner-with-a-keyed-child)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (is (null (search "sk-nested" (generation-text path))))
      (is (equal '("provider-test-keyed :api-key")
                 (getf (service-entry path :inner) :withheld))))))

(test rollback-initargs-reach-a-child-a-remounted-context-declares
  (with-checkpoints (dir)
    (mount-inner-with-a-keyed-child)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (m:unmount *ckpt-context* :inner)
      (let ((result (rolled-back path :initargs '((:provider-test-keyed :api-key "sk-back")))))
        (is (equal '(:inner) (getf result :remounted)))
        (is (equal '(:provider-test-keyed) (getf result :updated)))
        (is (null (getf result :unremounted))))
      (is (equal "sk-back" (provider-key)))
      (is (equal "test-model" (miao::provider-model
                               (m:service-of (m:lookup :provider-test-keyed))))
          "the rest of its spec is kept"))))

(test a-declared-child-is-left-alone-without-an-override-or-a-remounted-context
  (with-checkpoints (dir)
    (mount-inner-with-a-keyed-child)
    (let ((path (miao:checkpoint *ckpt-context* :dir dir)))
      (is (null (getf (rolled-back path :initargs '((:provider-test-keyed :api-key "sk-x")))
                      :updated))
          "the context was never gone, so its child is not rollback's to update")
      (m:unmount *ckpt-context* :inner)
      (let ((result (rolled-back path)))
        (is (null (getf result :updated)))
        (is (null (provider-key)))))))
