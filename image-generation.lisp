(in-package #:miao)

(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

;;; Image generations (~takeiteasy/miao#48): a generation (checkpoint.lisp)
;;; that also carries the running image itself, so a rollback -- unlike a
;;; declared-state-only one -- can undo code, not just state. SAVE-IMAGE
;;; forks, suspends nothing on the calling thread's own account (M:SUSPEND,
;;; meow#64) so every other thread is gone, then has the child
;;; SAVE-LISP-AND-DIE while the parent resumes and carries on. RELAUNCH
;;; re-execs into a saved core; miao.ros is the launcher and
;;; recovery side.

;;; --- refusals -----------------------------------------------------------

(defun %require-main-thread ()
  (unless (sb-thread:main-thread-p)
    (error "SAVE-IMAGE must run on the main thread, not ~a." (sb-thread:thread-name sb-thread:*current-thread*))))

(defun %require-no-credentials (context)
  "Refuses if any entry under CONTEXT is a PROVIDER holding an :API-KEY --
a core file is a copy of the whole heap, the same line providers.md and
tool-image already hold for published state and metadata -- or if an
entry can't be inspected at all. A provider M:SERVICE-OF can't reach
before its call times out is exactly the case that must not be assumed
credential-free: failing open here would be the one place this refusal
doesn't actually hold."
  (dolist (entry (%context-entries context))
    (multiple-value-bind (service problem) (ignore-errors (m:service-of (getf entry :process)))
      (cond
        (problem
         (error "could not check ~(~a~) for an :api-key before taking an image (~a); refusing rather than risk writing one to disk"
                (getf entry :name) problem))
        ((and (typep service 'provider) (provider-api-key service))
         (error "~(~a~) is mounted with :api-key; SAVE-IMAGE refuses to write a credential to disk. Unmount it, or mount from the environment variable instead."
                (getf entry :name)))))))

;;; --- saving ---------------------------------------------------------------

(defun forget-workers ()
  "Start a new process image's worker bookkeeping: every worker held so far
reads as stale, and none is signalled. SBCL's own list of child processes
survives a save too; its entries for these workers go, so a pid a new child
reuses is never reaped through them."
  (let ((pids (mapcar (lambda (w) (uiop:process-info-pid (worker-process w)))
                      *live-workers*)))
    (sb-thread:with-recursive-lock (sb-impl::*active-processes-lock*)
      (setf sb-impl::*active-processes*
            (remove-if (lambda (p) (member (sb-ext:process-pid p) pids))
                       sb-impl::*active-processes*))))
  (setf *boot* (list :boot))
  (bt:with-lock-held (*live-workers-lock*) (setf *live-workers* '())))

(pushnew '%forget-vault-token sb-ext:*save-hooks*)

(defun %image-path (generation-path)
  (make-pathname :type "core" :defaults generation-path))

(defun %make-image-toplevel (suspension)
  "SUSPENSION closed over from before the fork -- the same process and
service instances the saved core's heap already holds, so RESUME on load
just respawns threads over them, no re-discovery needed. FORGET-WORKERS
and FORGET-POOLS first: the workers and pooled threads that heap holds
belong to the process that saved it, and
so do the vault claims on its agents' queued steers, which are claimed again
as this image (RECLAIM-STEER-CLAIMS). MIAO_IMAGE_PROBE
set skips all of that: the launcher and the integration tests use it to check
a core loads without actually reviving its services."
  (lambda ()
    (cond
      ((uiop:getenv "MIAO_IMAGE_PROBE") (sb-ext:exit :code 0 :abort t))
      (t (forget-workers)
         (forget-pools)
         ;; TODO: reads meow's internal suspension-entries; upgrade path is
         ;; a public accessor. Tracked in ~takeiteasy/meow#69.
         (dolist (entry (meow::suspension-entries suspension))
           (when (typep (second entry) 'agent)
             (reclaim-steer-claims (second entry))))
         (cl+ssl:reload)
         (m:resume suspension)
         (sb-impl::toplevel-init)))))

(defun %save-error-path (core-path)
  (make-pathname :type "save-error" :defaults core-path))

(defun %save-and-die (suspension core-path)
  "Runs in the forked child, which SAVE-IMAGE has already confirmed is
down to its own single thread. Never returns: SAVE-LISP-AND-DIE exits the
process on success (SAVE-IMAGE reads that exit status, not this return,
since the child's own further execution is moot either way). On failure
this writes the condition to a sibling .save-error file -- the child's
own return value can't otherwise reach the parent past WAITPID's exit
code -- then EXITs :code 1, so a save that raised partway through,
leaving a truncated core, is never mistaken for one that finished."
  (handler-case
      (sb-ext:save-lisp-and-die (namestring core-path)
                                :toplevel (%make-image-toplevel suspension))
    (error (e)
      (ignore-errors
       (a:write-string-into-file (princ-to-string e) (%save-error-path core-path)
                                 :if-exists :supersede))))
  (sb-ext:exit :code 1 :abort t))

(defun %await-lone-thread (deadline)
  "Wait until this thread is the only one running, or DEADLINE (an
internal-real-time reading) passes. FORK's own check backs this up --
newborn threads it can see that LIST-ALL-THREADS still hides -- but this
turns the common case (a just-stopped context's thread still mid-unwind,
~takeiteasy/miao#72) into a bounded wait instead of an outright refusal.
Signals, naming every other thread by name, if any are still running once
DEADLINE passes."
  (loop for others = (remove sb-thread:*current-thread* (sb-thread:list-all-threads))
        while others
        do (if (> (get-internal-real-time) deadline)
               (error "~d thread~:p other than the main thread still running, SAVE-IMAGE needs this one alone: ~{~a~^, ~}"
                      (length others) (mapcar #'sb-thread:thread-name others))
               (sleep 0.01))))

(defun %require-clean-save (pid core-path)
  "PID's WAITPID status, checked against SAVE-LISP-AND-DIE's own contract
(a clean exit :code 0): anything else -- a nonzero code, a signal -- means
CORE-PATH is truncated or absent, not a generation to hand back as if it
were whole. Deletes it and signals rather than returning a broken path,
naming the underlying condition (%SAVE-ERROR-PATH) if the child left one."
  (multiple-value-bind (reported-pid status) (sb-posix:waitpid pid 0)
    (declare (ignore reported-pid))
    (unless (and (sb-posix:wifexited status) (zerop (sb-posix:wexitstatus status)))
      (ignore-errors (delete-file core-path))
      (let ((error-path (%save-error-path core-path)))
        (unwind-protect
             (error "save-lisp-and-die did not finish cleanly (status ~a)~@[: ~a~]; ~a was not written"
                    status (and (probe-file error-path) (uiop:read-file-string error-path)) core-path)
          (ignore-errors (delete-file error-path)))))))

(defun save-image (context &key (dir *generations-directory*) label keep (timeout 5))
  "Fork and SAVE-LISP-AND-DIE a full image of CONTEXT's tree, alongside a
declared-state generation (checkpoint.lisp) of the same label. Must run on
the main thread, the only one still standing once M:SUSPEND has parked
every other. Refuses -- before suspending anything -- if a mounted
PROVIDER holds an :API-KEY.

TIMEOUT (seconds, default 5) bounds both M:SUSPEND and the wait for any
thread outside CONTEXT's tree to exit on its own -- a just-stopped
context's thread still mid-unwind (~takeiteasy/miao#72), say. Past that,
SAVE-IMAGE refuses rather than let FORK's own single-threaded check do it
less informatively.

Returns (values core-path generation-path). The calling process is
unaffected: every service is suspended for the fork and resumed again
before this returns, whether the fork succeeded, failed, or never ran
because of a stray thread."
  (%require-main-thread)
  (%require-no-credentials context)
  (let* ((generation-path (checkpoint context :dir dir :label label :keep keep))
         (core-path (%image-path generation-path))
         (suspension (m:suspend context :timeout timeout)))
    (unwind-protect
         (progn
           ;; Pooled threads sit outside every context, so M:SUSPEND
           ;; leaves them be.
           (retire-idle-workers :timeout timeout)
           (journal-retire-writers :timeout timeout)
           (%await-lone-thread (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))
           (let ((pid (sb-posix:fork)))
             (if (zerop pid)
                 (%save-and-die suspension core-path)
                 (%require-clean-save pid core-path))))
      (m:resume suspension))
    (when keep (%prune-generations (uiop:pathname-directory-pathname generation-path) keep))
    (setf *last-image* (%canonical-path core-path) *self-dirty* nil)
    (values *last-image* (%canonical-path generation-path))))

;;; --- SELF-DEFINE (~takeiteasy/miao#63) ---------------------------------
;;;
;;; tool-self's :define, even checkpointed, can only be undone back to
;;; declared state -- never the redefinition itself. SELF-DEFINE closes
;;; that gap for an operator by taking an image generation immediately
;;; before the write, so RELAUNCHing that core undoes the code too. It is
;;; a REPL entry, not a tool op: an agent's turn can't reach it (SAVE-IMAGE
;;; needs the main thread), and the operator interrupts it the ordinary
;;; way -- there is no worker thread or :TIMEOUT here to abandon.

(defun self-define (context form &key (package "CL-USER") label (log (%default-self-log)))
  "Redefine FORM (read in PACKAGE), with an image generation taken
immediately before it as the rollback path. Returns the definition's
result and the image's path. Signals as SAVE-IMAGE does for its own
refusals (off the main thread, a credentialed provider); a malformed or
non-definition FORM is a plain error, same shape PARSE-DEFINE-FORM
reports through tool-self."
  (multiple-value-bind (parsed problem) (parse-define-form form package)
    (if problem
        (error "~a" problem)
        (let ((previous (self-write-previous-source :define parsed)))
          (multiple-value-bind (image-path checkpoint-path)
              (save-image context :label (or label (format nil "self-define ~(~a~)" (first parsed))))
            (%log-self-define-entry log :intent parsed label checkpoint-path previous image-path)
            (let ((result (eval-in-host parsed package)))
              ;; SAVE-IMAGE cleared *SELF-DIRTY* for the image it just
              ;; took, before this write -- the write itself still counts,
              ;; the same as any other tool-self write, so :REQUIRE-IMAGE
              ;; never treats an image as covering a redefinition that
              ;; happened after it.
              (setf *self-dirty* t)
              (%log-self-define-entry log :outcome parsed label checkpoint-path previous image-path result)
              (values result image-path)))))))

(defun %log-self-define-entry (log kind parsed label checkpoint-path previous image-path &optional result)
  (let ((entry (if (eq kind :intent)
                    (list :at (%now-iso8601) :kind :intent :op :define
                          :form (prin1-to-string parsed) :label label
                          :checkpoint (namestring checkpoint-path) :previous-source previous
                          :image image-path)
                    (list :at (%now-iso8601) :kind :outcome :op :define
                          :outcome (if (tool-error-p result) (list :error (tool-error result)) :ok)))))
    (%append-log log entry)))

;;; --- launching --------------------------------------------------------

;;; No Lisp-level execv wrapper exists in this SBCL build (sb-posix,
;;; sb-ext and sb-unix were all checked by hand against the running
;;; implementation) -- bound straight to libc's.
(sb-alien:define-alien-routine "execv" sb-alien:int
  (path sb-alien:c-string)
  (argv (* sb-alien:c-string)))

(defun %execv (path args)
  "Replace the current process image with PATH, ARGS as argv (PATH itself
is not implicitly argv[0]; callers pass it). Never returns on success --
signals an error on failure, execv's usual contract."
  (let* ((n (length args))
         (argv (sb-alien:make-alien sb-alien:c-string (1+ n))))
    (unwind-protect
         (progn
           (loop for i from 0 for a in args do (setf (sb-alien:deref argv i) a))
           (setf (sb-alien:deref argv n) nil)
           (execv path argv)
           (error "execv ~a failed: ~a" path (sb-int:strerror (sb-unix::get-errno))))
      (sb-alien:free-alien argv))))

(defun relaunch (core)
  "Replace the running SBCL process with CORE (EXECV), after confirming it
loads (MIAO/LAUNCHER:PROBE-CORE), killing this process's workers and running shell
commands first so none outlives it as an orphan. A generation's core is code-exact -- unlike
declared-state ROLLBACK, this is the manual way tool-self's :DEFINE
writes can actually be undone (~takeiteasy/miao#63) until an operator
does it. Never returns on success."
  (unless (probe-file core) (error "no such core: ~a" core))
  (unless (miao/launcher:probe-core core) (error "~a did not load cleanly; refusing to relaunch into it" core))
  (kill-live-workers)
  (kill-live-commands)
  (finish-output) (finish-output *error-output*)
  (let ((argv (miao/launcher:launch-argv core nil)))
    (%execv (first argv) argv)))

(defun save-recovery-image (path)
  "A plain image with no services mounted, for `miao install`: the
image the launcher falls back to when a generation's core fails
MIAO/LAUNCHER:PROBE-CORE. Must run on the main thread, same as SAVE-IMAGE."
  (%require-main-thread)
  (ensure-directories-exist path)
  (sb-ext:save-lisp-and-die
   (namestring path)
   :toplevel (lambda ()
               (unless (uiop:getenv "MIAO_IMAGE_PROBE")
                 (cl+ssl:reload))
               (if (uiop:getenv "MIAO_IMAGE_PROBE")
                   (sb-ext:exit :code 0 :abort t)
                   (sb-impl::toplevel-init)))))
