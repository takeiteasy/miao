(in-package #:miao)

;;; Self-modification (~takeiteasy/miao#12): evaluate in the host image,
;;; redefine functions and classes, and reload a mounted child -- the three
;;; things tool-eval, tool-repl and a worker can never reach, since a worker
;;; loads nothing and shares no state with the running harness.
;;;
;;; Every write op (:eval, :define, :reload) is off by default. :ENABLE, a
;;; mount option, names the ops a caller may reach; :LOG alone is always
;;; answered, since it only reads back what a write already did. This
;;; records operator intent and gives an audit trail -- it is not a sandbox.
;;; A :form can still redefine TOOL-TRUST or RESOLVE-TOOLS, since it runs in
;;; the host image with no DSL between it and CL:EVAL; the plan gate (#6)
;;; constrains a *model's* reach, not an operator's.
;;;
;;; A write takes a checkpoint first (checkpoint.lisp) and logs an intent
;;; entry naming it before the op runs, then an outcome entry after -- so a
;;; crash mid-eval still leaves a trace pointing at the generation to roll
;;; back to. The log is a second, append-only s-expression file alongside
;;; the generation directory, read the same guarded way (checkpoint.lisp's
;;; %READ-GENERATION): *READ-EVAL* nil, so a submitted form can never run
;;; merely by the log being read back. Each entry also reaches meow's
;;; logger, when one is mounted, for live visibility; the file is the
;;; durable copy.

(defvar *self-log* nil
  "Default log path for TOOL-SELF: ~/.miao/self.log, resolved lazily so
loading this file never touches the filesystem or the user's home.")

(defun %default-self-log ()
  (or *self-log*
      (setf *self-log* (merge-pathnames ".miao/self.log" (user-homedir-pathname)))))

(defvar *last-image* nil
  "SAVE-IMAGE's (image-generation.lisp, loaded after this file) most
recent core pathname, or nil if none has been taken. Set there; read here
for :REQUIRE-IMAGE and logged on every write, so this file only ever
depends on a var, never a forward reference to a function in a file that
loads after it.")

(defvar *self-dirty* nil
  "T once any tool-self write has happened since *LAST-IMAGE* was taken.
Cleared by SAVE-IMAGE and SELF-DEFINE; set by every write this file
performs, regardless of :op.")

(a:define-constant +definition-heads+
    '(defun defmacro defgeneric defmethod defclass defstruct defparameter
      defvar m:defservice miao:define-tool)
  :test #'equal
  :documentation "Heads :DEFINE accepts. Anything else -- PROGN, LET, a bare
call -- is :EVAL's job instead, so :DEFINE's checkpoint label and
:previous-source logging always describe an actual definition.")

(define-tool :tool-self
    (:trust :operator
     :summary "Evaluate, redefine and reload in the host image; every write checkpointed and logged"
     :slots ((enable :initarg :enable :initform nil :reader self-enable
                     :documentation "Ops this mount answers as writes, e.g.
'(:define :reload). :EVAL, :DEFINE and :RELOAD are refused unless named
here; :LOG always answers.")
             (dir :initarg :dir :initform *generations-directory* :reader self-dir)
             (keep :initarg :keep :initform nil :reader self-keep)
             (log-path :initarg :log :initform nil :reader self-log-path)
             (require-image :initarg :require-image :initform nil :reader self-require-image-p
                            :documentation "T refuses :eval and :define
unless an image generation (~takeiteasy/miao#48) has been taken and
nothing has written since (*LAST-IMAGE*, *SELF-DIRTY*) -- SELF-DEFINE is
then the only way an operator can still redefine anything, and every
:define stays code-exact, undoable by relaunching that image."))
     :params ((:op (member :eval :define :reload :log) :required t
               :doc "operation to perform")
              (:form string :required-when (:op (:eval :define)) :doc "source text of one form")
              (:package string :default "CL-USER"
               :doc "package the form reads in, for :eval and :define")
              (:name string :required-when (:op :reload) :doc "child name to reload")
              (:label string :doc "a note for this write's checkpoint and log entry")
              (:limit (integer 1 1000) :default 50 :doc "entries to answer, for :log")
              (:timeout (integer 1) :default +default-tool-timeout+
               :doc "stop waiting on a write after this many milliseconds")))
  (:invoke (op form package name label limit timeout)
    (case op
      (:log (op-self-log (self-log-file service) limit))
      (t (cond
           ((not (member op (self-enable service)))
            (fail (list :forbidden (format nil "~(~a~) is not enabled" op))))
           ((and (member op '(:eval :define)) (self-require-image-p service)
                 (%image-required-refusal))
            (bad-request "~a" (%image-required-refusal)))
           (t (op-self-write service op form package name label timeout cancel-token)))))))

(defun %image-required-refusal ()
  "Why :REQUIRE-IMAGE refuses right now, or nil if it wouldn't. A generation
taken before any tool-self write, but with a write since, is stale: it
would roll back to before the write tool-self is about to make, not to
before this one."
  (cond ((null *last-image*) "take an image generation first (~takeiteasy/miao#48)")
        (*self-dirty* "the last image generation is stale -- take another first")))

(defun self-log-file (service)
  (or (self-log-path service) (%default-self-log)))

;;; --- dispatch, ahead of any checkpoint --------------------------------

(defun op-self-write (service op form package name label timeout cancel)
  (multiple-value-bind (parsed problem)
      (case op
        (:eval (parse-self-form form package))
        (:define (parse-define-form form package))
        (:reload (parse-reload-name name)))
    (if problem
        (bad-request "~a" problem)
        (run-checkpointed-write service op parsed label timeout package cancel))))

(defun parse-self-form (form package)
  (%read-one-form form package))

(defun parse-define-form (form package)
  (multiple-value-bind (parsed problem) (parse-self-form form package)
    (cond
      (problem (values nil problem))
      ((not (member (first parsed) +definition-heads+ :test #'eq))
       (values nil (format nil "~(~a~) is not a definition form" (first parsed))))
      (t (values parsed nil)))))

(defun parse-reload-name (name)
  (let ((symbol (find-symbol (string-upcase name) "KEYWORD")))
    (if symbol (values symbol nil) (values nil (format nil "no such name ~a" name)))))

(defun %read-one-form (text package-name)
  "TEXT read as exactly one form, in PACKAGE-NAME, with *READ-EVAL* nil, or
(values nil problem). FIND-PACKAGE only -- a package that does not already
exist is a problem, never created on the operator's behalf."
  (let ((package (find-package (string-upcase package-name))))
    (if (null package)
        (values nil (format nil "no package named ~a" package-name))
        (let ((*read-eval* nil) (*package* package))
          (handler-case
              (with-input-from-string (stream text)
                (let ((form (read stream)))
                  (if (eq (read stream nil :eof) :eof)
                      (values form nil)
                      (values nil "form must be exactly one expression"))))
            (error (e) (values nil (princ-to-string e))))))))

;;; --- checkpoint, log, then the op --------------------------------------

(defun run-checkpointed-write (service op parsed label timeout package cancel)
  (let ((context (m:service-context service)))
    (if (null context)
        (fail (list :error "not mounted under a context"))
        (handler-case
            (let* ((checkpoint-path
                     (namestring (checkpoint (m:service-process context)
                                             :dir (self-dir service) :keep (self-keep service)
                                             :label (or label (format nil "tool-self ~(~a~)" op)))))
                   (previous (self-write-previous-source op parsed)))
              (setf *self-dirty* t)
              (log-self-entry service :intent op parsed label checkpoint-path previous)
              (let* ((outcome-logged (bt:make-semaphore))
                     (result (perform-self-write
                              service op parsed timeout package cancel
                              (lambda (late-result)
                                (bt:wait-on-semaphore outcome-logged)
                                (log-self-outcome service op parsed late-result
                                                  :kind :late-outcome :checkpoint checkpoint-path)))))
                (unwind-protect (log-self-outcome service op parsed result)
                  (bt:signal-semaphore outcome-logged))
                result))
          (file-error (e) (fail (list :error (princ-to-string e))))))))

(defun self-write-previous-source (op parsed)
  "PARSED's defined name's current SYMBOL-SOURCE (tools/image.lisp), for
:DEFINE only, so a log entry that redefines something still points at where
it used to live. Declared-state ROLLBACK never restores code
(~takeiteasy/miao#48); an ordinary tool-self :define therefore has no way
back but this pointer -- SELF-DEFINE (image-generation.lisp,
~takeiteasy/miao#63) is the code-exact one, an image generation taken
immediately before the write."
  (when (and (eq op :define) (second parsed) (symbolp (second parsed)))
    (symbol-source (second parsed))))

(defun perform-self-write (service op parsed timeout package cancel on-late)
  (ecase op
    ((:eval :define) (run-in-host parsed timeout :package package :cancel cancel
                                                 :on-late on-late))
    (:reload (op-self-reload service parsed timeout cancel on-late))))

;;; --- reload, waited on off the tool's process ---------------------------

;;; The context stops and restarts the child on its own process, and a
;;; reload it has begun cannot be undone half way. So a cancel or a lapsed
;;; :TIMEOUT only stops the wait: the reload runs to its end, and its result
;;; reaches ON-LATE. A reload not yet begun is never started. STATE is the
;;; same hand-off RUN-IN-HOST uses.

(defun op-self-reload (service name timeout cancel on-late)
  (let ((context (m:service-process (m:service-context service))))
    ;; Refused here: waited on from another thread, the context would stop
    ;; this process while its HANDLE still waits on the reload.
    (if (eq (m:lookup name :registry (m:service-registry service))
            (m:service-process service))
        (fail (list :error "tool-self cannot reload itself"))
        (let* ((result nil)
               (state (list :running))
               (wake (bt:make-semaphore)))
          (bt:make-thread
           (lambda ()
             (when (eq :running (car state))
               (setf result (reload-child context name))
               (unless (atomics:cas (car state) :running :done)
                 (ignore-errors (funcall on-late result))))
             (bt:signal-semaphore wake))
           :name "miao-self-reload")
          (when cancel
            (on-cancel cancel (lambda () (bt:signal-semaphore wake))))
          (bt:wait-on-semaphore wake :timeout (/ timeout 1000))
          (if (atomics:cas (car state) :running :stopped)
              (fail (if (and cancel (cancelled-p cancel)) :cancelled :timeout))
              result)))))

(defun reload-child (context name)
  (handler-case
      (a:if-let (process (m:reload context name))
        (ok :name (string-downcase (symbol-name name)) :process (princ-to-string process))
        (bad-request "no child named ~(~a~)" name))
    (error (e) (fail (list :error (princ-to-string e))))))

;;; --- CLOS mutation latch (~takeiteasy/miao#79, #81) --------------------
;;;
;;; SBCL's own PCL/DEFSTRUCT loaders are hooked to say exactly when a form's
;;; evaluation is inside a class, method, generic-function or struct
;;; mutation, so :DEFINE only defers a lapsed deadline for the duration of
;;; one mutation instead of across the whole form.
;;;
;;; *CLOS-MUTATION-LATCH*, bound fresh by RUN-IN-HOST for each evaluation,
;;; counts the mutations in flight. Outside one the interrupt lands
;;; pre-emptively, so a wedged :eql specializer or slow compile is killed,
;;; not leaked; inside one it only records itself as pending, and the
;;; mutation's own exit throws once the count returns to zero. A form killed
;;; after landing a mutation is partially applied and reports :ABANDONED.
;;;
;;; A mutation gets *CLOS-MUTATION-GRACE-MS* to finish. If user code inside
;;; it (a MOP method, a defstruct constructor macro) outlasts that, the
;;; interrupt throws out of it anyway and the form reports :TORN: that one
;;; definition may be half applied.
;;;
;;; The hooks are installed with SB-INT:ENCAPSULATE, the primitive TRACE and
;;; PROFILE use, so a rename breaks loudly. They are gated on the latch
;;; being bound, so a DEFMETHOD elsewhere in the image is unaffected.
;;;
;;; A DEFGENERIC redefinition removes the old initial methods under a system
;;; lock with interrupts off, where a user REMOVE-METHOD method could wedge
;;; uninterruptibly; %HOLD-DEFGENERIC removes them first, outside it.

(defstruct clos-latch
  (depth 0 :type fixnum)
  (pending nil)
  (mutated nil))

(defvar *clos-mutation-latch* nil
  "Nil outside RUN-IN-HOST; bound there to a CLOS-LATCH for the form being
evaluated.")

(defvar *clos-mutation-grace-ms* 2000
  "How long a lapsed deadline waits for one CLOS mutation before tearing it.")

#+sbcl (a:define-constant +clos-held-hooks+
    '(sb-pcl::load-defclass sb-pcl::load-defmethod
      sb-pcl::set-initial-methods sb-pcl::compile-or-load-defgeneric)
  :test #'equal
  :documentation "SBCL internals each of which is one whole mutation:
the interrupt is deferred for exactly the call.")

#+sbcl (a:define-constant +clos-span-hooks+
    '((sb-kernel::%defstruct . :begin) (sb-kernel::%target-defstruct . :end))
  :test #'equal
  :documentation "A defstruct is several steps between these two; the
interrupt is deferred across the span so a struct is never torn.")

(defun %finish-clos-mutation (latch)
  (setf (clos-latch-mutated latch) t)
  (when (and (zerop (clos-latch-depth latch)) (clos-latch-pending latch))
    (throw 'self-abandoned (fail :abandoned))))

(defun %hold-clos-mutation (next &rest args)
  (let ((latch *clos-mutation-latch*))
    (if (null latch)
        (apply next args)
        (progn
          (incf (clos-latch-depth latch))
          (multiple-value-prog1 (unwind-protect (apply next args)
                                  (decf (clos-latch-depth latch)))
            (%finish-clos-mutation latch))))))

(defun %begin-clos-mutation (next &rest args)
  (when *clos-mutation-latch* (incf (clos-latch-depth *clos-mutation-latch*)))
  (apply next args))

(defun %end-clos-mutation (next &rest args)
  (let ((latch *clos-mutation-latch*))
    (if (null latch)
        (apply next args)
        (multiple-value-prog1 (apply next args)
          (decf (clos-latch-depth latch))
          (%finish-clos-mutation latch)))))

#+sbcl (defun %remove-initial-methods (name)
  "Removes the initial methods of the generic function NAME that a
redefinition would remove itself, but under a system lock with interrupts
off, where a user REMOVE-METHOD method could not be interrupted."
  (when (fboundp name)
    (let ((function (fdefinition name)))
      (when (typep function 'generic-function)
        (dolist (method (copy-list (sb-pcl::generic-function-initial-methods function)))
          (remove-method function method))
        (setf (sb-pcl::generic-function-initial-methods function) '())))))

#+sbcl (defun %hold-defgeneric (next &rest args)
  (if (null *clos-mutation-latch*)
      (apply next args)
      (apply #'%hold-clos-mutation
             (lambda (&rest args)
               (%remove-initial-methods (first args))
               (apply next args))
             args)))

#+sbcl (defun %clos-hook-alist ()
  (append (mapcar (lambda (name) (cons name #'%hold-clos-mutation)) +clos-held-hooks+)
          (list (cons 'sb-pcl::load-defgeneric #'%hold-defgeneric))
          (mapcar (lambda (entry)
                    (cons (car entry) (if (eq :begin (cdr entry))
                                          #'%begin-clos-mutation
                                          #'%end-clos-mutation)))
                  +clos-span-hooks+)))

#+sbcl (defun %install-clos-mutation-hooks ()
  "Idempotent: UNENCAPSULATEs first, so reloading this file never stacks a
second copy of the same hook."
  (loop for (name . hook) in (%clos-hook-alist)
        do (sb-int:unencapsulate name 'miao-self)
           (sb-int:encapsulate name 'miao-self hook)))

#-sbcl
(defun clos-mutation-hooks-installed-p () nil)

#+sbcl
(defun clos-mutation-hooks-installed-p ()
  "T if every hook %INSTALL-CLOS-MUTATION-HOOKS installs is still in
place -- checked by a test, so a future SBCL rename of one of these
internals fails loudly instead of silently reverting :DEFINE to
pre-emptive-only."
  (loop for (name) in (%clos-hook-alist)
        always (sb-int:encapsulated-p name 'miao-self)))

#+sbcl (eval-when (:load-toplevel :execute) (%install-clos-mutation-hooks))

;;; --- host eval, bounded and interruptible ------------------------------

;;; Same shape as TOOLS/HTTP.LISP's ABANDON-REQUEST: a worker thread, a
;;; semaphore for the caller's wait, and an IN-REGION box guarding the
;;; interrupt so it only ever throws to a catch tag that is actually there.
;;;
;;; A deadline's or a cancel's interrupt lands pre-emptively (THROW
;;; straight to SELF-ABANDONED) except inside a CLOS mutation, where it waits
;;; for that mutation to finish. The caller has already been told :TIMEOUT
;;; or :CANCELLED by then, so a form abandoned after mutating reports
;;; :ABANDONED through ON-LATE.
;;;
;;; STATE is the hand-off between the two threads: whichever of the worker
;;; (:DONE) and the caller (:STOPPED) claims it first by COMPARE-AND-SWAP
;;; decides who reports the result. The worker checks it again once inside
;;; the region, so an interrupt that landed before it got there is not lost.

(defun run-in-host (form timeout-ms &key package cancel on-late)
  "FORM evaluated on its own thread, with *PACKAGE* bound to the package
named PACKAGE, interrupted at TIMEOUT-MS or when CANCEL, a cancel token or
NIL, is cancelled. The interrupt lands between CLOS mutations, or inside one
only after the grace deadline (see above). A form killed after mutating
reports (fail :abandoned), or (fail :torn) if killed inside a mutation, to
ON-LATE, if given, after the caller has already received :TIMEOUT or
:CANCELLED."
  (let* ((result nil)
         (in-region (list nil))
         (state (list :running))
         (latch (make-clos-latch))
         (done (bt:make-semaphore))
         (wake (bt:make-semaphore))
         (registry m:*registry*)
         (worker (bt:make-thread
                  (lambda ()
                    (let ((m:*registry* registry)
                          (*clos-mutation-latch* latch))
                      (unwind-protect
                           (progn
                             (setf result
                                   (catch 'self-abandoned
                                     (setf (car in-region) t)
                                     (unless (eq :running (car state))
                                       (throw 'self-abandoned nil))
                                     (unwind-protect (eval-in-host form package)
                                       (setf (car in-region) nil))))
                             (when (and (not (atomics:cas (car state) :running :done))
                                        result on-late)
                               (ignore-errors (funcall on-late result))))
                        (bt:signal-semaphore done)
                        (bt:signal-semaphore wake))))
                  :name "miao-self-eval")))
    ;; A cancel only wakes the wait: DONE stays the worker's, which
    ;; TEAR-AFTER-GRACE waits on.
    (when cancel
      (on-cancel cancel (lambda () (bt:signal-semaphore wake))))
    (bt:wait-on-semaphore wake :timeout (/ timeout-ms 1000))
    (if (not (atomics:cas (car state) :running :stopped))
        result
        (progn (abandon-self-eval worker in-region latch done)
               (fail (if (and cancel (cancelled-p cancel)) :cancelled :timeout))))))

(defun abandon-self-eval (worker in-region latch done)
  (ignore-errors
   (when (bt:thread-alive-p worker)
     (bt:interrupt-thread
      worker (lambda ()
               (when (car in-region)
                 (if (plusp (clos-latch-depth latch))
                     (setf (clos-latch-pending latch) t)
                     (throw 'self-abandoned
                       (and (clos-latch-mutated latch) (fail :abandoned)))))))
     (tear-after-grace worker in-region latch done (/ *clos-mutation-grace-ms* 1000)))))

(defun tear-after-grace (worker in-region latch done grace)
  "Once GRACE seconds pass without WORKER exiting, throws it out of the
mutation it is still in. GRACE is read by the caller: a binding is not
visible on the helper thread."
  (bt:make-thread
   (lambda ()
     (unless (bt:wait-on-semaphore done :timeout grace)
       (ignore-errors
        (when (bt:thread-alive-p worker)
          (bt:interrupt-thread
           worker (lambda ()
                    (when (car in-region)
                      (throw 'self-abandoned
                        (cond ((plusp (clos-latch-depth latch)) (fail :torn))
                              ((clos-latch-mutated latch) (fail :abandoned)))))))))))
   :name "miao-self-grace"))

(defun eval-in-host (form package)
  (let ((out (make-string-output-stream)))
    (handler-case
        (let ((values (let ((*standard-output* out) (*error-output* out)
                            (*package* (or (and package (find-package (string-upcase package)))
                                           *package*)))
                        (multiple-value-list (eval form)))))
          (multiple-value-bind (rendered elided) (render-self-values values)
            (ok :value (or (first rendered) "NIL") :values rendered
                :out (get-output-stream-string out) :elided elided)))
      (error (e) (fail (list :error (princ-to-string e)))))))

(defun render-self-value (value)
  "VALUE printed under the same caps and elision check
WORKER-PROGRAM.LISP's RENDER applies to a worker value, so a large or
circular host value cannot flood the reply the way an uncapped one could
(~takeiteasy/miao#26), and a caller sees when it did. Elided when the
character cap cut the string outright, or when printing one step wider
would print more of it -- that second pass only runs when the first
output looks cut, so a value under both limits prints once."
  (flet ((render-under (length level)
           (let ((*print-length* length) (*print-level* level)
                 (*print-readably* nil) (*print-circle* t))
             (prin1-to-string value))))
    (let* ((s (render-under 100 8))
           (capped (> (length s) 4000)))
      (values (if capped (concatenate 'string (subseq s 0 4000) " ...") s)
              (or capped
                  (and (or (find #\# s) (search "..." s))
                       (string/= s (render-under 101 9))))))))

(defun render-self-values (values)
  "Every one of VALUES rendered under RENDER-SELF-VALUE's own cap; more
than 100 values, or a combined printed form past 4000 characters, drops
the remainder and sets ELIDED -- the same guard
~takeiteasy/miao#105 gave the worker side's own RENDER-VALUES."
  (let* ((many (> (length values) 100))
         (values (if many (subseq values 0 100) values))
         (elided many) (total 0) (rendered '()))
    (dolist (v values)
      (if (> total 4000)
          (setf elided t)
          (multiple-value-bind (s e) (render-self-value v)
            (push s rendered)
            (incf total (length s))
            (when e (setf elided t)))))
    (values (nreverse rendered) elided)))

;;; --- the log -----------------------------------------------------------

(defun log-self-entry (service kind op parsed label checkpoint-path previous)
  (let ((entry (list :at (%now-iso8601) :kind kind :op op
                     :form (and (member op '(:eval :define)) (prin1-to-string parsed))
                     :name (and (eq op :reload) parsed)
                     :label label :checkpoint checkpoint-path :previous-source previous
                     :image *last-image*)))
    (%append-log (self-log-file service) entry)
    (m:log-info service "tool-self ~(~a~) ~(~a~), checkpoint ~a" kind op checkpoint-path)))

(defun log-self-outcome (service op parsed result &key (kind :outcome) checkpoint)
  "KIND :LATE-OUTCOME, with the write's CHECKPOINT to tie it to its intent
entry, records a write that finished after its caller was told :TIMEOUT."
  (let ((entry (list :at (%now-iso8601) :kind kind :op op
                     :name (and (eq op :reload) parsed)
                     :checkpoint checkpoint
                     :outcome (if (tool-error-p result) (list :error (tool-error result)) :ok))))
    (%append-log (self-log-file service) entry)
    (cond ((eq kind :late-outcome)
           (m:log-warn service "tool-self ~(~a~) finished after its timeout: ~s" op (getf entry :outcome)))
          ((tool-error-p result)
           (m:log-warn service "tool-self ~(~a~) failed: ~s" op (tool-error result)))
          (t (m:log-info service "tool-self ~(~a~) ok" op)))))

(defun op-self-log (path limit)
  (let ((entries (%read-log path)))
    (ok :entries (last entries limit) :total (length entries))))
