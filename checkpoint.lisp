(in-package #:miao)

(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

;;; Generations (~takeiteasy/miao#11). A checkpoint is one s-expression file
;;; recording every named service's own declared state, taken through the
;;; SNAPSHOT/RESTORE convention (tool.lisp) shared by every tool, the agent
;;; and a provider. Rollback restores that state onto the services mounted
;;; now, first remounting one unmounted since the checkpoint from what the
;;; generation recorded of how it was mounted (~takeiteasy/miao#49).
;;;
;;; That record is where a provider's :api-key would otherwise end up on disk
;;; (providers.md's credentials line, held the same way in tools/image.lisp),
;;; so a class names its credential initargs through SECRET-INITARGS and they
;;; are left out, as is any value that cannot be printed and read back, such
;;; as an agent's :sink. What was left out is recorded by name, as :WITHHELD,
;;; and a remount takes it back from ROLLBACK's :INITARGS.
;;;
;;; Image generations -- SAVE-LISP-AND-DIE, relaunch-and-restore, an
;;; install-time recovery image (~takeiteasy/miao#48) -- live in
;;; image-generation.lisp instead, layered on this file's declared-state
;;; generation and M:SUSPEND/M:RESUME (meow#64): SAVE-IMAGE takes one of
;;; these first, then writes a sibling .core alongside it.
;;;
;;; The agent's own SNAPSHOT/RESTORE methods live at the end of agent.lisp,
;;; alongside the slots they read and write.

(defgeneric secret-initargs (service)
  (:documentation "The mount initargs of SERVICE's class that hold a credential,
which a generation never writes to disk. NIL by default. A class is asked
through its prototype, so the method reads nothing but the class.")
  (:method ((service m:service)) nil))

(defvar *generations-directory* (miao/launcher:generations-directory)
  "Default directory CHECKPOINT writes to and GENERATIONS lists from.")

;;; --- walking the mount tree ---------------------------------------------

(defun %delegated-p (child)
  "Whether CHILD, an M:CHILDREN entry, is an agent M:DELEGATE mounted, which
runs once and is never mounted again."
  (and (eq :temporary (getf child :restart))
       (subtypep (getf child :class) 'agent)))

(defun %context-entries (context-process &key specs parent)
  "Every named child under CONTEXT-PROCESS, recursively and parent first, as a
flat list of (:name :class :process). A delegated sub-agent (agent.lisp), named
or not, is skipped, as is its own subtree.
SPECS adds each child's :SPEC, how to mount it again (%ENTRY-SPEC), PARENT
being the name of the context CONTEXT-PROCESS is."
  (loop for child in (m:children context-process)
        for name = (getf child :name)
        for class = (getf child :class)
        for process = (getf child :process)
        when (and name (not (%delegated-p child)))
          collect (append (list :name name :class (string-downcase (symbol-name class))
                                :process process)
                          (and specs (%entry-spec context-process name class parent)))
        when (and name (not (%delegated-p child)) (subtypep class 'm:context) process)
          append (%context-entries process :specs specs :parent name)))

;;; --- how a service was mounted ---------------------------------------------

(defun %secret-initargs (class)
  "CLASS's SECRET-INITARGS, or :ALL when it cannot be asked, which fails
closed: a class that cannot say which initargs are credentials records none."
  (handler-case
      (let ((class (find-class class)))
        (sb-mop:finalize-inheritance class)
        (secret-initargs (sb-mop:class-prototype class)))
    (error () :all)))

(defun %readable-p (value)
  "Whether VALUE prints and reads back as it would be remounted: SBCL prints
a function readably as a #. form, which the guarded read then refuses."
  (handler-case (let ((*print-readably* t))
                  (%read-initargs (%print-initargs value))
                  t)
    (error () nil)))

(defun %check-readable-defaults (name params)
  "Signal unless every :DEFAULT in PARAMS, a schema with its defaults
evaluated, prints and reads back: a call carries its tool's schema into a
generation file."
  (dolist (param params)
    (let ((default (getf (param-options param) :default *absent*)))
      (unless (or (eq default *absent*) (%readable-p default))
        (error "~s's :default for ~s does not print and read back: ~s"
               name (param-name param) default)))))

(defun %persistable-initargs (class initargs)
  "INITARGS of a CLASS mount as a generation may record them, then the ones
left out: those CLASS's SECRET-INITARGS name, any value that does not print
readably, and, inside a :CHILDREN spec list, the same for each spec's own
class. A left-out key is a keyword, or a string \"class key\" from a child spec."
  (let ((secret (%secret-initargs class)) kept withheld)
    (if (eq secret :all)
        (loop for (key) on initargs by #'cddr do (push key withheld))
        (loop for (key value) on initargs by #'cddr
              do (cond ((member key secret) (push key withheld))
                       ((and (eq key :children) (a:proper-list-p value))
                        (multiple-value-bind (specs gone) (%persistable-children value)
                          (setf kept (append kept (list key specs))
                                withheld (append (reverse gone) withheld))))
                       ((%readable-p value) (setf kept (append kept (list key value))))
                       (t (push key withheld)))))
    (values kept (nreverse withheld))))

(defun %persistable-children (specs)
  "SPECS, a :CHILDREN list of (class . initargs), as %PERSISTABLE-INITARGS
records each, then what it left out. A spec whose class is not defined cannot
be asked, so it is left out whole."
  (let (kept withheld)
    (dolist (spec specs)
      (let ((class (and (consp spec) (symbolp (car spec)) (find-class (car spec) nil))))
        (cond (class
               (multiple-value-bind (args gone) (%persistable-initargs (car spec) (cdr spec))
                 (push (cons (car spec) args) kept)
                 (dolist (key gone)
                   (push (format nil "~(~a~) ~(~s~)" (car spec) key) withheld))))
              (t (push (format nil "~(~a~) :all" (if (consp spec) (car spec) spec)) withheld)))))
    (values (nreverse kept) (nreverse withheld))))

(defun %print-initargs (initargs)
  "INITARGS as text, so that reading a generation never depends on every
package they name being loaded: an entry whose package is gone fails alone,
when it is remounted, rather than the whole file failing to read."
  (let ((*package* (find-package "KEYWORD")) (*print-case* :downcase))
    (prin1-to-string initargs)))

(defun %read-initargs (text)
  (let ((*read-eval* nil) (*package* (find-package "KEYWORD")))
    (read-from-string text)))

(defun %entry-spec (context-process name class parent)
  "(:SPEC plist) of what a generation records to mount NAME again, or nil
when it cannot be: its class is not a symbol in a package, or CONTEXT-PROCESS
no longer has it."
  (let ((spec (m:child-spec context-process name)))
    (when (and spec (symbolp class) (symbol-package class))
      (multiple-value-bind (initargs withheld)
          (%persistable-initargs class (getf spec :initargs))
        (list :spec (list :parent parent
                          :package (package-name (symbol-package class))
                          :symbol (symbol-name class)
                          :restart (getf spec :restart)
                          :shutdown (getf spec :shutdown)
                          :backoff (getf spec :backoff)
                          :backoff-max (getf spec :backoff-max)
                          :initargs (%print-initargs initargs)
                          :withheld withheld))))))

;;; --- snapshot / restore across a process boundary ------------------------

;;; A reply of (:error ...) -- a service outside these three conventions,
;;; still answering the shared UNKNOWN-MESSAGE fallback -- is recorded as no
;;; state. A transport failure (M:CALL's second value) is recorded as
;;; :UNAVAILABLE instead, which ROLLBACK skips rather than restoring nil over
;;; whatever the service holds.

(defun %unavailable-reason (status)
  "STATUS, M:CALL's second value, as a keyword safe to print into a generation."
  (if (consp status) (first status) status))

(defun %interrupted-p (state)
  "True when STATE is a plist carrying a non-nil :IN-FLIGHT -- work a restore
cannot bring back."
  (and (consp state) (a:proper-list-p state) (evenp (length state))
       (getf state :in-flight)
       t))

;;; --- the generation file --------------------------------------------------

(defun generation-id ()
  "A name that sorts by the time it was made, to the microsecond, then a random
tail so two processes in the same microsecond do not collide."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time (+ seconds (encode-universal-time 0 0 0 1 1 1970 0)))
      (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~6,'0d-~3,'0d"
              year month day hour min sec microseconds (random 1000)))))

(defun %generation-filename ()
  (format nil "~a.generation" (generation-id)))

(defun %now-iso8601 (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (sec min hour day month year) (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour min sec)))

(defun %write-generation (path form)
  "FORM written to PATH through a temporary file in the same directory, then
renamed in, so a torn write never replaces a good generation."
  (uiop:with-temporary-file (:pathname tmp :directory (uiop:pathname-directory-pathname path)
                             :type "tmp" :keep t)
    ;; Circle detection writes a list the form shares, such as a tool schema
    ;; several calls carry, once.
    (let ((*package* (find-package "KEYWORD")) (*print-case* :downcase)
          (*print-circle* t))
      (a:write-string-into-file (prin1-to-string form) tmp :if-exists :supersede))
    (uiop:rename-file-overwriting-target tmp path)))

(defun %read-generation (path)
  "The form stored at PATH, read with *READ-EVAL* NIL so a generation can
never run code merely by being read back in -- the same guard tool-eval's
worker applies to a submitted form."
  (let ((*read-eval* nil) (*package* (find-package "KEYWORD")))
    (with-input-from-string (stream (a:read-file-into-string path))
      (read stream))))

;;; --- shared append-only logs --------------------------------------------

;;; TOOL-SELF's log (tools/self.lisp) and the vault (vault.lisp,
;;; ~takeiteasy/miao#14) are both one append-only s-expression file, read
;;; back the same guarded way a generation is. Shared here rather than
;;; duplicated.
;;;
;;; One lock per log file, keyed by its canonical name, so two specs naming
;;; the same file serialise and different files never queue behind each other.
;;; WITH-LOG-LOCK adds an flock(2) on a sidecar "<log>.lock" file, so other
;;; processes appending to or compacting the same log serialise too. The
;;; sidecar is never renamed, unlike the log a compaction replaces.

(defvar *log-locks* (make-hash-table :test 'equal)
  "Canonical log namestring -> its lock.")

(defvar *log-locks-lock* (bt:make-lock :name "miao-log-registry")
  "Guards *LOG-LOCKS*.")

(defun %log-key (path)
  "PATH's canonical namestring. Its directory must exist."
  (or (a:when-let ((file (probe-file path))) (namestring file))
      (namestring (merge-pathnames (file-namestring path)
                                   (truename (uiop:pathname-directory-pathname path))))))

(defun %log-lock (path)
  (ensure-directories-exist path)
  (let ((key (%log-key path)))
    (bt:with-lock-held (*log-locks-lock*)
      (or (gethash key *log-locks*)
          (setf (gethash key *log-locks*) (bt:make-lock :name key))))))

(defconstant +lock-ex+ 2)

(defun %flock-exclusive (fd)
  (loop until (zerop (sb-alien:alien-funcall
                      (sb-alien:extern-alien "flock" (function sb-alien:int sb-alien:int sb-alien:int))
                      fd +lock-ex+))
        unless (= (sb-alien:get-errno) sb-posix:eintr)
          do (error "flock failed, errno ~d" (sb-alien:get-errno))))

(defun call-with-log-lock (path thunk)
  (bt:with-lock-held ((%log-lock path))
    (let ((fd (sb-posix:open (concatenate 'string (%log-key path) ".lock")
                             (logior sb-posix:o-creat sb-posix:o-rdwr) #o644)))
      (unwind-protect
           (progn (sb-posix:fcntl fd sb-posix:f-setfd 1) ; FD_CLOEXEC
                  (%flock-exclusive fd)
                  (funcall thunk))
        (sb-posix:close fd)))))

(defmacro with-log-lock ((path) &body body)
  "Run BODY holding PATH's in-process lock and its cross-process flock."
  `(call-with-log-lock ,path (lambda () ,@body)))

(defun %append-log-locked (path entry)
  "%APPEND-LOG's write, for a caller already holding PATH's WITH-LOG-LOCK."
  (let ((*package* (find-package "KEYWORD")) (*print-case* :downcase))
    (with-open-file (stream path :direction :output :if-exists :append
                                  :if-does-not-exist :create)
      (prin1 entry stream)
      (terpri stream))))

(defun %append-log-lines-locked (path lines)
  "Append LINES, entries already printed, one open for all, for a caller already
holding PATH's WITH-LOG-LOCK."
  (with-open-file (stream path :direction :output :if-exists :append
                                :if-does-not-exist :create)
    (dolist (line lines)
      (write-line line stream))))

(defun %append-log (path entry)
  "Append ENTRY, a plist, to PATH as one printed form per line. *PRINT-CASE*
downcase and the keyword package, so the file reads back the same way
regardless of the caller's own *PACKAGE*."
  (with-log-lock (path)
    (%append-log-locked path entry)))

(defun %read-log (path)
  "Every entry in PATH, oldest first, read with *READ-EVAL* nil -- the same
guard %READ-GENERATION applies -- so a log can never run code merely by
being read back. A malformed form ends the read; the second value is nil
then, T when the whole file read cleanly."
  (if (not (probe-file path))
      (values nil t)
      (let ((*read-eval* nil) (*package* (find-package "KEYWORD")) (clean t))
        (with-open-file (stream path)
          (values (loop for form = (handler-case (read stream nil :eof)
                                     (error () (setf clean nil) :eof))
                        until (eq form :eof)
                        collect form)
                  clean)))))

(defun %write-log (path entries)
  "ENTRIES written to PATH, one form per line, through a temporary file in
the same directory and renamed in. The caller holds PATH's WITH-LOG-LOCK."
  (uiop:with-temporary-file (:pathname tmp :directory (uiop:pathname-directory-pathname path)
                             :type "tmp" :keep t)
    (let ((*package* (find-package "KEYWORD")) (*print-case* :downcase))
      (with-open-file (stream tmp :direction :output :if-exists :supersede)
        (dolist (entry entries)
          (prin1 entry stream)
          (terpri stream))))
    (uiop:rename-file-overwriting-target tmp path)))

;;; --- the API -------------------------------------------------------------

(defun %canonical-path (path)
  "PATH resolved to its TRUENAME when possible. A symlinked TMPDIR (macOS's
/var, say) resolves differently under a plain MERGE-PATHNAMES than under a
directory listing, and the two implementations disagree on which side does
the resolving -- so CHECKPOINT's return and GENERATIONS' own :path both go
through this, and always compare equal to each other."
  (or (ignore-errors (truename path)) path))

(defun %snapshot-entry (entry outcome)
  (destructuring-bind (reply status) outcome
    (let ((base (append (list :name (getf entry :name) :class (getf entry :class))
                        (getf entry :spec))))
      (cond (status (append base (list :unavailable (%unavailable-reason status))))
            ((result-error-p reply) (append base (list :state nil)))
            (t (append base (list :state reply)))))))

(defun %entry-names (services key)
  (loop for entry in services
        when (ecase key
               (:interrupted (%interrupted-p (getf entry :state)))
               (:unavailable (getf entry :unavailable)))
          collect (getf entry :name)))

(defun checkpoint (context &key (dir *generations-directory*) label keep (timeout 30))
  "Snapshot every named service under CONTEXT, a mounted context's process,
recursively, and write it as a generation file under DIR. Every service is
asked at once and given TIMEOUT seconds; one that does not answer is
recorded :UNAVAILABLE. KEEP, given, prunes DIR to its KEEP newest
generations afterwards. Returns the generation's pathname, then the names
of the services snapshotted mid-work and of those unavailable."
  (let* ((entries (%context-entries context :specs t))
         (outcomes (m:call-all (mapcar (lambda (entry) (getf entry :process)) entries)
                               '(:snapshot) :timeout timeout))
         (services (mapcar #'%snapshot-entry entries outcomes))
         (directory (uiop:ensure-directory-pathname dir))
         (path (merge-pathnames (%generation-filename) directory)))
    (ensure-directories-exist directory)
    (%write-generation path
                       (list :miao-generation 2 :created (%now-iso8601)
                             :label label :services services))
    (when keep (%prune-generations directory keep))
    (values (%canonical-path path)
            (%entry-names services :interrupted)
            (%entry-names services :unavailable))))

(defun %generation-image (path)
  "PATH's sibling .core (~takeiteasy/miao#48's SAVE-IMAGE writes one
alongside its generation, same basename), or nil."
  (let ((core (make-pathname :type "core" :defaults path)))
    (and (probe-file core) (namestring (%canonical-path core)))))

(defun generations (&key (dir *generations-directory*))
  "Every generation under DIR, newest first, as (:path :created :label
:services :interrupted :unavailable :image), :services naming the services
it covers rather than their state, :interrupted and :unavailable the ones
snapshotted mid-work or not at all. :IMAGE is the generation's sibling .core, or nil if none was
taken (SAVE-IMAGE, ~takeiteasy/miao#48)."
  (sort (loop for path in (ignore-errors
                            (uiop:directory-files (uiop:ensure-directory-pathname dir)
                                                  "*.generation"))
              for generation = (ignore-errors (%read-generation path))
              when generation
                collect (list :path (namestring (%canonical-path path))
                              :created (getf generation :created)
                              :label (getf generation :label)
                              :services (mapcar (lambda (entry) (getf entry :name))
                                                (getf generation :services))
                              :interrupted (%entry-names (getf generation :services) :interrupted)
                              :unavailable (%entry-names (getf generation :services) :unavailable)
                              :image (%generation-image path)))
        ;; The filename sorts by the microsecond it was made, so sorting by it
        ;; (rather than :CREATED, which two generations in the same second
        ;; share) is newest-first and keeps the one CHECKPOINT just wrote
        ;; out of PRUNE-GENERATIONS' reach.
        #'string> :key (lambda (g) (getf g :path))))

(defun %prune-generations (dir keep)
  (dolist (stale (nthcdr keep (generations :dir dir)))
    (ignore-errors (delete-file (getf stale :path)))
    (a:when-let ((image (getf stale :image)))
      (ignore-errors (delete-file image)))))

(defun %remount-entry (context entries entry overrides)
  "Mount ENTRY, a generation entry, again, under the context it was under
among ENTRIES -- CONTEXT's current entries -- or CONTEXT itself. OVERRIDES is
ROLLBACK's :INITARGS. Signals an error saying why it cannot."
  (let* ((parent-name (getf entry :parent))
         (parent (if parent-name
                     (getf (find parent-name entries :key (lambda (e) (getf e :name)))
                           :process)
                     context))
         (package (find-package (getf entry :package)))
         (class (and package (find-symbol (getf entry :symbol) package))))
    (unless parent (error "its parent ~(~a~) is not mounted" parent-name))
    (unless (and class (find-class class nil))
      (error "its class ~a::~a is not defined" (getf entry :package) (getf entry :symbol)))
    (apply #'m:mount parent class
           (append (cdr (assoc (getf entry :name) overrides))
                   (%read-initargs (getf entry :initargs))
                   (loop for key in '(:restart :shutdown :backoff :backoff-max)
                         when (getf entry key) append (list key (getf entry key)))))))

(defun %update-declared (context entries entry overrides)
  "Give ENTRY, a generation entry mounted now by a context that ROLLBACK just
mounted again, its OVERRIDES: the context mounted it itself, from a spec with
its credentials left out, so it is updated in place instead."
  (let ((parent (getf (find (getf entry :parent) entries :key (lambda (e) (getf e :name)))
                      :process)))
    (unless parent (error "its parent ~(~a~) is not mounted" (getf entry :parent)))
    (apply #'m:update parent (getf entry :name)
           (cdr (assoc (getf entry :name) overrides)))))

(defun %remount (context recorded overrides)
  "Mount again each of RECORDED, generation entries, that has no service of
its name now and was recorded with how it was mounted. Parent first, as the
generation lists them, looking again after each so that a context's declared
children, which it mounts itself, are not mounted twice. Each of those, at any
depth, that OVERRIDES names is updated with them. Returns the names remounted,
then those updated, then (name reason) for each that could not be."
  (let (remounted updated failed brought)
    (dolist (entry recorded)
      (let* ((name (getf entry :name)) (entries (%context-entries context))
             (present (find name entries :key (lambda (e) (getf e :name)))))
        (handler-case
            (cond ((and (not present) (getf entry :symbol))
                   (%remount-entry context entries entry overrides)
                   (push name remounted)
                   (push name brought))
                  ((and present (getf entry :parent) (member (getf entry :parent) brought))
                   (push name brought)
                   (when (assoc name overrides)
                     (%update-declared context entries entry overrides)
                     (push name updated))))
          (error (e) (push (list name (princ-to-string e)) failed)))))
    (values (nreverse remounted) (nreverse updated) (nreverse failed))))

(defun rollback (context path &key (timeout 30) (remount t) initargs)
  "Restore the generation at PATH onto CONTEXT's named services now. Every
restore is sent at once and given TIMEOUT seconds. Unless REMOUNT is nil, a
service unmounted since the checkpoint is first mounted again from what the
generation recorded of it, and restored like the rest. INITARGS, an alist of
(name . initargs), is added to a remounted service's own -- the way to give
back a credential the generation left out, which a provider otherwise takes
from its environment variable -- or, for a child a remounted context declares
and mounted itself, applied to it with M:UPDATE.
Returns (:ok (:restored names :failed names :failures entries :interrupted names
:unavailable names :remounted names :updated names :unremounted entries :missing names
:mismatched entries :extra names)). FAILED names a
restore that got no answer and FAILURES lists each as (name reason); INTERRUPTED a restored service that was
snapshotted mid-work, whose in-flight work is gone; UNAVAILABLE an entry the
checkpoint could not snapshot, left as it is. REMOUNTED names a service
mounted again, UPDATED a declared child of one that INITARGS was applied to,
UNREMOUNTED lists each that could not be as (name reason), and
MISSING names a generation entry with no service mounted under that name
now, one from a version 1 generation included; MISMATCHED one mounted
under a different class, which is reported rather than restored; EXTRA a
service mounted now the generation does not name. None of these fails the
call -- the caller decides what drift means."
  (let* ((generation (%read-generation path))
         (recorded (getf generation :services))
         (remounted '()) (updated '()) (unremounted '())
         (current '())
         (targets '())
         (unavailable '()) (missing '()) (mismatched '()))
    (when remount
      (setf (values remounted updated unremounted) (%remount context recorded initargs)))
    (setf current (%context-entries context))
    (dolist (entry recorded)
      (let* ((name (getf entry :name))
             (found (find name current :key (lambda (e) (getf e :name)))))
        (cond
          ((null found) (push name missing))
          ((not (string= (getf entry :class) (getf found :class)))
           (push (list :name name :expected (getf entry :class)
                       :actual (getf found :class))
                 mismatched))
          ((getf entry :unavailable) (push name unavailable))
          (t (push (cons entry found) targets)))))
    (setf targets (nreverse targets))
    (let ((outcomes (m:call-each
                     (mapcar (lambda (target) (getf (cdr target) :process)) targets)
                     (mapcar (lambda (target) (list :restore (getf (car target) :state)))
                             targets)
                     :timeout timeout))
          (restored '()) (failed '()) (failures '()) (interrupted '()))
      (loop for (entry . nil) in targets
            for (nil status) in outcomes
            for name = (getf entry :name)
            do (cond (status (push name failed)
                            (push (list name (%unavailable-reason status)) failures))
                     (t (push name restored)
                        (when (%interrupted-p (getf entry :state))
                          (push name interrupted)))))
      (ok :restored (nreverse restored) :failed (nreverse failed)
          :failures (nreverse failures) :interrupted (nreverse interrupted) :unavailable (nreverse unavailable)
          :remounted remounted :updated updated :unremounted unremounted
          :missing (nreverse missing) :mismatched (nreverse mismatched)
          :extra (set-difference (mapcar (lambda (e) (getf e :name)) current)
                                 (mapcar (lambda (e) (getf e :name)) recorded))))))
