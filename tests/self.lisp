(in-package #:miao/tests)
(in-suite :miao)

;;; TOOL-SELF (~takeiteasy/miao#12): the write ops refused unless :ENABLE
;;; names them, the checkpoint-then-log sequence around each write, and
;;; :EVAL, :DEFINE and :RELOAD themselves.
;;;
;;; Definitions made through :EVAL/:DEFINE land in MIAO-SELF-TEST, a scratch
;;; package created before and deleted after every test -- nothing here
;;; ever redefines a MIAO:: symbol. STATEFUL-THING, THING and SET-THING are
;;; tests/checkpoint.lisp's.

(defvar *self-context* nil)
(defvar *self-log-path* nil)
(defvar *self-dir* nil)

(defun make-self-log-path ()
  (format nil "~amiao-self-test-~36r.log"
          (namestring (uiop:temporary-directory))
          (random (expt 2 64) (make-random-state t))))

(defun call-with-self (enable body)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (log (make-self-log-path)))
    (setf *self-log-path* log)
    (or (find-package "MIAO-SELF-TEST") (make-package "MIAO-SELF-TEST" :use '("CL")))
    (unwind-protect
         (with-generations-directory (dir)
           (setf *self-dir* dir)
           (let ((context (m:start-service (make-instance 'm:context :name :self-tools)
                                           :registry registry)))
             (setf *self-context* context)
             (unwind-protect
                  (progn
                    (m:mount context 'stateful-thing)
                    (m:mount context 'miao:tool-self :enable enable :dir dir :log log)
                    (funcall body))
               (m:stop context))))
      (delete-package "MIAO-SELF-TEST")
      (ignore-errors (delete-file log))
      (ignore-errors (delete-file (format nil "~a.lock" log))))))

(defmacro with-self ((&optional (enable ''(:eval :define :reload))) &body body)
  `(call-with-self ,enable (lambda () ,@body)))

(defun self (op &rest args)
  (apply #'miao:invoke-tool :tool-self :op op args))

;;; --- trust and gating --------------------------------------------------

(test tool-self-is-operator-trusted
  (with-self ()
    (is (eq :operator (miao:tool-trust (miao:describe-tool :tool-self))))))

(test tool-self-write-ops-refused-when-not-enabled
  (with-self (nil)
    (dolist (call (list (list :eval :form "1")
                        (list :define :form "(defun miao-self-test::f () 1)")
                        (list :reload :name "stateful-thing")))
      (let ((result (apply #'self call)))
        (is (equal :forbidden (first (miao:tool-error result))))))))

(test tool-self-log-always-answers-even-disabled
  (with-self (nil)
    (is (eq :ok (first (self :log))))))

;;; --- :eval ---------------------------------------------------------

(test self-eval-returns-value-and-output
  (with-self ()
    (let ((result (self :eval :form "(progn (princ \"hi\") (+ 1 2))")))
      (is (eq :ok (first result)))
      (is (equal "3" (getf (second result) :value)))
      (is (equal '("3") (getf (second result) :values)))
      (is (equal "hi" (getf (second result) :out)))
      (is (null (getf (second result) :elided))))))

(test self-eval-carries-every-value-a-form-returns
  (with-self ()
    (let ((result (self :eval :form "(floor 7 2)")))
      (is (equal "3" (getf (second result) :value)))
      (is (equal '("3" "1") (getf (second result) :values))))))

(test self-eval-with-no-values-reports-nil
  (with-self ()
    (let ((result (self :eval :form "(values)")))
      (is (equal "NIL" (getf (second result) :value)))
      (is (null (getf (second result) :values))))))

(test self-eval-elides-a-large-value
  (with-self ()
    (is (eq t (getf (second (self :eval :form "(make-list 200)")) :elided)))))

(test self-eval-a-signalling-form-is-an-error-not-a-crash
  (with-self ()
    (let ((result (self :eval :form "(error \"boom\")")))
      (is (eq :error (first result))))
    ;; the service is still answering afterwards
    (is (eq :ok (first (self :eval :form "1"))))))

(test self-eval-unreadable-source-is-a-bad-request-with-no-checkpoint
  (with-self ()
    (let ((before (length (miao:generations :dir *self-dir*)))
          (result (self :eval :form "(+ 1")))
      (is (equal :bad-request (first (miao:tool-error result))))
      (is (= before (length (miao:generations :dir *self-dir*)))))))

(test self-eval-past-its-timeout-times-out-and-recovers
  (with-self ()
    (let ((result (self :eval :form "(sleep 1)" :timeout 50)))
      (is (equal :timeout (miao:tool-error result))))
    (is (eq :ok (first (self :eval :form "1"))))))

;;; --- :define -------------------------------------------------------

(test self-define-defines-a-function-in-the-named-package
  (with-self ()
    (let ((result (self :define :form "(defun greet () :hi)" :package "MIAO-SELF-TEST")))
      (is (eq :ok (first result))))
    (is (eq :hi (funcall (find-symbol "GREET" "MIAO-SELF-TEST"))))))

(test self-define-refuses-a-non-definition-form
  (with-self ()
    (let ((result (self :define :form "(+ 1 2)")))
      (is (equal :bad-request (first (miao:tool-error result)))))))

(test self-define-redefines-a-class-and-updates-a-live-instance
  (with-self ()
    (eval (read-from-string
           "(defclass miao-self-test::thing ()
              ((x :initform 1 :accessor miao-self-test::thing-x)))"))
    (let ((instance (make-instance (find-symbol "THING" "MIAO-SELF-TEST"))))
      (self :define :package "MIAO-SELF-TEST"
            :form "(defclass thing () ((x :initform 1 :accessor thing-x) (y :initform 2 :accessor thing-y)))")
      (is (= 2 (funcall (find-symbol "THING-Y" "MIAO-SELF-TEST") instance))))))

;;; --- the CLOS mutation latch (~takeiteasy/miao#79, #81) --------------------
;;;
;;; RUN-IN-HOST defers a lapsed deadline only while one of SBCL's own
;;; loaders is mid-mutation, so a wedged :eql specializer or slow compile
;;; is killed whether or not the form mutated something earlier.

(defun no-miao-self-eval-thread-p ()
  "T once every RUN-IN-HOST worker has actually exited -- polled rather
than asserted immediately after :TIMEOUT, since ABANDON-SELF-EVAL's
interrupt still has to land and unwind."
  (eventually (lambda () (notany (lambda (thread) (equal "miao-self-eval" (sb-thread:thread-name thread)))
                                 (sb-thread:list-all-threads)))))

(test clos-mutation-hooks-are-installed
  (is-true (miao::clos-mutation-hooks-installed-p)))

(test self-define-a-wedged-eql-specializer-is-killed-not-leaked
  (with-self ()
    (self :define :package "MIAO-SELF-TEST" :form "(defgeneric self-test-wedged-eql (x))")
    (let ((result (self :define :package "MIAO-SELF-TEST" :timeout 50
                        :form "(defmethod self-test-wedged-eql ((x (eql (progn (sleep 5) 1)))) :done)")))
      (is (equal :timeout (miao:tool-error result))))
    ;; the :eql specializer's own form runs ahead of SBCL's method loader,
    ;; so the interrupt still lands pre-emptively: no method landed, and
    ;; the worker thread that was running it is gone, not leaked
    (is-true (no-miao-self-eval-thread-p))
    (signals error (funcall (find-symbol "SELF-TEST-WEDGED-EQL" "MIAO-SELF-TEST") 1))))

(test self-define-a-slow-method-compile-is-killed-not-leaked
  (with-self ()
    (self :define :package "MIAO-SELF-TEST" :form "(defgeneric self-test-slow-compile (x))")
    (let ((result (self :define :package "MIAO-SELF-TEST" :timeout 50
                        :form "(defmethod self-test-slow-compile ((x integer)) (macrolet ((slow () (sleep 5) 1)) (slow)))")))
      (is (equal :timeout (miao:tool-error result))))
    ;; compiling the method's own lambda -- SLOW's macroexpansion runs
    ;; here -- also runs ahead of the method loader that would latch, so
    ;; this is killed the same way
    (is-true (no-miao-self-eval-thread-p))))

(test self-define-non-clos-form-still-interruptible-past-its-timeout
  (with-self ()
    (let ((result (self :define :package "MIAO-SELF-TEST" :timeout 50
                        :form "(defparameter *self-test-wedged* (progn (sleep 1) :never))")))
      (is (equal :timeout (miao:tool-error result))))
    ;; a non-CLOS head (defun, defmacro, defparameter, defvar) never
    ;; latches, so the interrupt still lands mid-eval and the definition
    ;; never completes
    (sleep 1.2)
    (is (not (boundp (find-symbol "*SELF-TEST-WEDGED*" "MIAO-SELF-TEST"))))))

(defun late-outcome-entry ()
  (find :late-outcome (getf (second (self :log)) :entries) :key (lambda (e) (getf e :kind))))

(defun class-named (name)
  (find-class (find-symbol name "MIAO-SELF-TEST") nil))

(test self-eval-is-killed-between-clos-mutations
  (with-self ()
    (let ((result (self :eval :timeout 50 :package "MIAO-SELF-TEST"
                        :form "(progn (defclass self-test-latched-a () ()) (sleep 0.3) (defclass self-test-latched-b () ()))")))
      (is (equal :timeout (miao:tool-error result))))
    ;; the deadline lands after the first DEFCLASS finished: it stays, the
    ;; rest of the form is killed, and the log says so
    (is-true (no-miao-self-eval-thread-p))
    (is-true (class-named "SELF-TEST-LATCHED-A"))
    (is-false (class-named "SELF-TEST-LATCHED-B"))
    (is-true (eventually #'late-outcome-entry))
    (is (equal '(:error :abandoned) (getf (late-outcome-entry) :outcome)))))

(test self-define-a-wedge-after-a-first-mutation-is-killed-not-leaked
  (with-self ()
    (self :define :package "MIAO-SELF-TEST" :form "(defgeneric self-test-second-wedge (x))")
    (let ((result (self :eval :timeout 50 :package "MIAO-SELF-TEST"
                        :form "(progn (defclass self-test-first-mutation () ())
                                      (defmethod self-test-second-wedge ((x (eql (progn (sleep 5) 1)))) :done))")))
      (is (equal :timeout (miao:tool-error result))))
    (is-true (no-miao-self-eval-thread-p))
    (is-true (class-named "SELF-TEST-FIRST-MUTATION"))
    (signals error (funcall (find-symbol "SELF-TEST-SECOND-WEDGE" "MIAO-SELF-TEST") 1))))

(test self-eval-a-wedge-in-a-defgeneric-method-option-is-killed
  (with-self ()
    (self :eval :timeout 50 :package "MIAO-SELF-TEST"
                :form "(defgeneric self-test-option-wedge (x) (:method ((x (eql (progn (sleep 5) 1)))) :done))")
    (is-true (no-miao-self-eval-thread-p))))

(test self-eval-a-struct-is-never-torn-by-its-deadline
  ;; Weak: nothing here can reliably land the deadline inside the struct's
  ;; span, so a run may pass without exercising the deferral at all. It only
  ;; asserts the outcome is never a half-defined struct.
  (with-self ()
    (self :eval :timeout 20 :package "MIAO-SELF-TEST"
                :form "(defstruct self-test-struct-a a b c d e f g h)")
    (is-true (no-miao-self-eval-thread-p))
    (when (class-named "SELF-TEST-STRUCT-A")
      (is-true (fboundp (find-symbol "MAKE-SELF-TEST-STRUCT-A" "MIAO-SELF-TEST")))
      (is-true (fboundp (find-symbol "SELF-TEST-STRUCT-A-H" "MIAO-SELF-TEST"))))))

(test self-eval-interns-macroexpansion-symbols-in-the-requested-package
  (with-self ()
    (self :eval :package "MIAO-SELF-TEST" :form "(defstruct self-test-struct-b a)")
    (is-true (fboundp (find-symbol "MAKE-SELF-TEST-STRUCT-B" "MIAO-SELF-TEST")))
    (is-true (fboundp (find-symbol "SELF-TEST-STRUCT-B-A" "MIAO-SELF-TEST")))))

(defun call-with-grace (ms body)
  (let ((old miao::*clos-mutation-grace-ms*))
    (setf miao::*clos-mutation-grace-ms* ms)
    (unwind-protect (funcall body)
      (setf miao::*clos-mutation-grace-ms* old))))

(defun define-slow-metaclass (name seconds)
  "Defines metaclass NAME in MIAO-SELF-TEST whose class initialisation sleeps SECONDS."
  (self :define :package "MIAO-SELF-TEST"
                :form (format nil "(defclass ~a (standard-class) ())" name))
  (self :define :package "MIAO-SELF-TEST"
                :form (format nil "(defmethod sb-mop:validate-superclass ((c ~a) (s standard-class)) t)" name))
  (self :define :package "MIAO-SELF-TEST"
                :form (format nil "(defmethod shared-initialize :after ((c ~a) slots &key) (sleep ~a))" name seconds)))

(test self-eval-a-wedge-in-a-defstruct-span-is-torn-not-leaked
  (with-self ()
    (self :define :package "MIAO-SELF-TEST"
                  :form "(defmacro self-test-slow-macro () (sleep 5) 1)")
    (call-with-grace
     100
     (lambda ()
       (let ((result (self :eval :timeout 50 :package "MIAO-SELF-TEST"
                           :form "(defstruct (self-test-wedged-struct (:constructor make-self-test-wedged-struct (&aux (a (self-test-slow-macro))))) a)")))
         (is (equal :timeout (miao:tool-error result))))
       (is-true (no-miao-self-eval-thread-p))
       (is-true (eventually #'late-outcome-entry))
       (is (equal '(:error :torn) (getf (late-outcome-entry) :outcome)))))))

(test self-eval-a-wedge-in-a-mop-method-is-torn-not-leaked
  (with-self ()
    (define-slow-metaclass "SELF-TEST-WEDGED-META" 5)
    (call-with-grace
     100
     (lambda ()
       (let ((result (self :eval :timeout 50 :package "MIAO-SELF-TEST"
                           :form "(defclass self-test-wedged-instance () () (:metaclass self-test-wedged-meta))")))
         (is (equal :timeout (miao:tool-error result))))
       (is-true (no-miao-self-eval-thread-p))
       (is-true (eventually #'late-outcome-entry))
       (is (equal '(:error :torn) (getf (late-outcome-entry) :outcome)))))))

(test self-eval-a-wedge-in-remove-method-during-a-redefinition-is-torn-not-leaked
  (with-self ()
    (self :define :package "MIAO-SELF-TEST"
                  :form "(defclass self-test-slow-gf (standard-generic-function) () (:metaclass sb-mop:funcallable-standard-class))")
    (self :define :package "MIAO-SELF-TEST"
                  :form "(defgeneric self-test-redefined (x) (:generic-function-class self-test-slow-gf) (:method ((x integer)) 1))")
    (self :define :package "MIAO-SELF-TEST"
                  :form "(defmethod remove-method :after ((gf self-test-slow-gf) method) (sleep 5))")
    (call-with-grace
     100
     (lambda ()
       (let ((result (self :eval :timeout 50 :package "MIAO-SELF-TEST"
                           :form "(defgeneric self-test-redefined (x) (:generic-function-class self-test-slow-gf) (:method ((x string)) 2))")))
         (is (equal :timeout (miao:tool-error result))))
       (is-true (no-miao-self-eval-thread-p))))))

(test self-eval-a-slow-span-inside-its-grace-still-completes
  (with-self ()
    (define-slow-metaclass "SELF-TEST-SLOW-META" 0.3)
    (call-with-grace
     2000
     (lambda ()
       (self :eval :timeout 50 :package "MIAO-SELF-TEST"
                   :form "(defclass self-test-slow-instance () () (:metaclass self-test-slow-meta))")
       (is-true (no-miao-self-eval-thread-p))
       (is-true (class-named "SELF-TEST-SLOW-INSTANCE"))
       (is-true (eventually #'late-outcome-entry))
       (is (equal '(:error :abandoned) (getf (late-outcome-entry) :outcome)))))))

(test self-write-abandoned-after-its-timeout-logs-a-late-outcome
  (with-self ()
    (self :eval :timeout 50 :package "MIAO-SELF-TEST"
                :form "(progn (defclass self-test-late-a () ()) (sleep 0.3))")
    (is-true (eventually #'late-outcome-entry))
    (let* ((entries (getf (second (self :log)) :entries))
           (late (late-outcome-entry))
           (intent (find :intent entries :key (lambda (e) (getf e :kind)) :from-end t)))
      (is (equal '(:error :abandoned) (getf late :outcome)))
      (is (equal (getf intent :checkpoint) (getf late :checkpoint)))
      (is (equal '(:intent :outcome :late-outcome)
                 (mapcar (lambda (e) (getf e :kind)) (last entries 3)))))))

(test self-write-killed-at-its-timeout-logs-no-late-outcome
  (with-self ()
    (self :eval :timeout 50 :form "(sleep 1)")
    (sleep 0.3)
    (is (null (late-outcome-entry)))))

;;; A cancel lands as a lapsed deadline does, once the form is running --
;;; not while the checkpoint ahead of it is still being taken.

(defvar *self-eval-started* nil)

(defun cancel-self-once (started op &rest args)
  "Invoke OP with ARGS on a thread of its own, cancel it once STARTED answers
true, and return its result."
  (let* ((token (miao:make-cancel-token))
         (registry m:*registry*)
         (thread (bt:make-thread
                  (lambda ()
                    (let ((m:*registry* registry))
                      (apply #'self op :timeout 30000 :cancel token args))))))
    (is-true (eventually started 5))
    (miao:cancel token)
    (bt:join-thread thread)))

(defun last-entry (kind)
  (find kind (getf (second (self :log)) :entries)
        :key (lambda (e) (getf e :kind)) :from-end t))

(test self-eval-cancelled-mid-run-stops-at-once
  (with-self ()
    (setf *self-eval-started* nil)
    (let* ((started (get-internal-real-time))
           (result (cancel-self-once
                    (lambda () *self-eval-started*)
                    :eval :package "MIAO-SELF-TEST"
                    :form "(progn (setf miao/tests::*self-eval-started* t) (sleep 5))")))
      (is (equal :cancelled (miao:tool-error result)))
      (is (< (/ (- (get-internal-real-time) started) internal-time-units-per-second) 4))
      (is-true (no-miao-self-eval-thread-p))
      (is (equal '(:error :cancelled) (getf (last-entry :outcome) :outcome))))))

(test self-eval-cancelled-after-a-mutation-logs-it-abandoned
  (with-self ()
    (let ((result (cancel-self-once
                   (lambda () (class-named "SELF-TEST-CANCEL-A"))
                   :eval :package "MIAO-SELF-TEST"
                   :form "(progn (defclass self-test-cancel-a () ()) (sleep 5))")))
      (is (equal :cancelled (miao:tool-error result)))
      (is-true (eventually #'late-outcome-entry))
      (is (equal '(:error :abandoned) (getf (late-outcome-entry) :outcome))))))

;;; --- :reload ---------------------------------------------------------

(test self-reload-restarts-a-named-child
  (with-self ()
    (set-thing 9)
    (let ((result (self :reload :name "stateful-thing")))
      (is (eq :ok (first result))))
    ;; reload keeps the instance's own slots (meow's reload.md): still
    ;; answering, and still holding the value set before the reload.
    (is (eql 9 (thing)))))

;;; A child a second in stopping, so a reload is still running when its
;;; call's deadline lapses or its token is cancelled.

(defvar *slow-stopper-stopping* nil)

(m:defservice slow-stopper () () (:name :slow-stopper))

(defmethod m:dispose ((service slow-stopper) reason)
  (declare (ignore reason))
  (setf *slow-stopper-stopping* t)
  (sleep 1))

(test self-reload-past-its-timeout-stops-waiting-and-logs-the-late-outcome
  (with-self ()
    (m:mount *self-context* 'slow-stopper)
    (let ((result (self :reload :name "slow-stopper" :timeout 100)))
      (is (equal :timeout (miao:tool-error result)))
      (is-true (eventually #'late-outcome-entry 5))
      (is (eq :ok (getf (late-outcome-entry) :outcome)))
      (is-true (m:lookup :slow-stopper)))))

(test self-reload-cancelled-mid-run-stops-waiting-and-logs-the-late-outcome
  (with-self ()
    (setf *slow-stopper-stopping* nil)
    (m:mount *self-context* 'slow-stopper)
    (let ((result (cancel-self-once (lambda () *slow-stopper-stopping*)
                                    :reload :name "slow-stopper")))
      (is (equal :cancelled (miao:tool-error result)))
      (is (equal '(:error :cancelled) (getf (last-entry :outcome) :outcome)))
      (is-true (eventually #'late-outcome-entry 5))
      (is (eq :ok (getf (late-outcome-entry) :outcome))))))

(test self-reload-an-unknown-name-is-a-bad-request
  (with-self ()
    (let ((result (self :reload :name "no-such-child")))
      (is (equal :bad-request (first (miao:tool-error result)))))))

(test self-reload-of-itself-is-an-error-not-a-deadlock
  (with-self ()
    (let ((result (self :reload :name "tool-self")))
      (is (eq :error (first result))))))

;;; --- checkpoint + log around every write --------------------------

(test self-write-checkpoints-and-logs-intent-and-outcome
  (with-self ()
    (let ((before (length (miao:generations :dir *self-dir*))))
      (self :eval :form "1" :label "probe")
      (is (= (1+ before) (length (miao:generations :dir *self-dir*))))
      (let* ((entries (getf (second (self :log)) :entries))
             (last-two (last entries 2)))
        (is (eq :intent (getf (first last-two) :kind)))
        (is (equal "probe" (getf (first last-two) :label)))
        (is (eq :outcome (getf (second last-two) :kind)))
        (is (eq :ok (getf (second last-two) :outcome)))))))

(test self-log-reading-never-evaluates
  (with-self ()
    (self :eval :form "1")
    (with-open-file (stream *self-log-path* :direction :output
                            :if-exists :append :if-does-not-exist :create)
      (write-string "(:kind :evil :form #.(error \"read-eval ran\"))" stream)
      (terpri stream))
    (is (eq :ok (first (self :log))))))

;;; --- kept off the plan gate ------------------------------------------

(test tool-plan-refuses-tool-self
  (with-self ()
    (m:mount *self-context* 'miao:tool-plan :allow '(:tool-self))
    (let ((result (miao:invoke-tool :tool-plan
                                    :steps (list (list :tool "tool-self"
                                                        :args (list :op :eval :form "1"))))))
      (is (equal :bad-request (first (miao:tool-error result))))
      (is (search "agent-trusted" (second (miao:tool-error result)))))))
