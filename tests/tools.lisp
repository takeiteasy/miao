(in-package #:miao/tests)
(in-suite :miao)

;;; The tool convention exercised through the real registry and service
;;; stack: discovery by registration props, the describe/invoke protocol,
;;; per-tool behaviour, sandbox enforcement, and the property that a hung
;;; command never wedges the tool service itself.

(defvar *sandbox* nil "The fs tool's sandbox root for the running test.")
(defvar *context* nil "The context the running test's tools are mounted in.")

(defun call-with-tools (body)
  (let* ((registry (make-instance 'm:registry))
         (m:*registry* registry)
         (root (make-sandbox-directory))
         (context (m:start-service (make-instance 'm:context :name :tools)
                                   :registry registry)))
    (setf *sandbox* root
          *context* context)
    (unwind-protect
         (progn
           (m:mount context 'miao:tool-fs :root root)
           (m:mount context 'miao:tool-shell)
           (m:mount context 'miao:tool-http)
           (m:mount context 'miao:tool-eval)
           (m:mount context 'miao:tool-gated-eval)
           (m:mount context 'miao:tool-repl)
           (m:mount context 'miao:tool-plan :allow '(:tool-fs))
           (m:mount context 'miao:tool-image)
           (m:mount context 'miao:tool-services)
           (funcall body))
      (m:stop context)
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname root)
                                  :validate t :if-does-not-exist :ignore))))

(defmacro with-tools (&body body)
  `(call-with-tools (lambda () ,@body)))

(defun make-sandbox-directory ()
  (let ((path (format nil "~amiao-tool-test-~36r"
                      (namestring (uiop:temporary-directory))
                      (random (expt 2 64) (make-random-state t)))))
    (ensure-directories-exist (concatenate 'string path "/"))
    ;; The tool truenames its root the same way (/tmp is itself a symlink
    ;; on macOS); match it so assertions about escaping compare like with
    ;; like.
    (string-right-trim "/" (uiop:native-namestring (uiop:truename* path)))))

(defun tool (name &rest args)
  (apply #'miao:invoke-tool name args))

(defun result-value (result key)
  (getf (second result) key))

(defmacro tool-thread (&body body)
  ;; A plain BT:MAKE-THREAD does not inherit dynamic bindings, and
  ;; M:*REGISTRY* is one WITH-TOOLS establishes with a LET -- so a thread
  ;; that calls TOOL needs it carried across explicitly.
  `(let ((registry m:*registry*))
     (bt:make-thread (lambda () (let ((m:*registry* registry)) ,@body)))))

(defparameter +getpid-form+
  ;; A worker runs the host implementation, so the bare image it starts has
  ;; exactly the internals this one does.
  #+sbcl "(sb-unix:unix-getpid)"
  #+ecl "(ext:getpid)")

(defun unix-process-alive-p (pid)
  (zerop (nth-value 2 (uiop:run-program (list "kill" "-0" (princ-to-string pid))
                                        :ignore-error-status t))))

(defun wait-for-exit (pid &optional (deadline 2.0))
  "True once PID is gone. A kill is not documented to block until the
process has actually exited, so the check polls rather than assume."
  (loop repeat (ceiling deadline 0.05)
        while (unix-process-alive-p pid)
        do (sleep 0.05))
  (not (unix-process-alive-p pid)))

(defun unix-pgid (pid)
  "PID's process group id, or NIL if it is already gone."
  (let ((out (with-output-to-string (s)
              (uiop:run-program (list "ps" "-o" "pgid=" "-p" (princ-to-string pid))
                                :output s :ignore-error-status t))))
    (parse-integer out :junk-allowed t)))

(defun process-group-containment-available-p ()
  "True unless the host has fallen all the way back to :TREE -- the racy
last resort with no dedicated OS mechanism behind it."
  (not (eq miao::*process-group-strategy* :tree)))

(defun poll-until (predicate &optional (deadline 5.0) (interval 0.1))
  "True once PREDICATE is false, polled rather than assumed instant."
  (loop repeat (ceiling deadline interval)
        while (funcall predicate)
        do (sleep interval))
  (not (funcall predicate)))

;;; --- M:CALL failures folded into the result vocabulary (~takeiteasy/miao#106)

(test call-result-passes-through-a-real-reply
  (is (equal '(:ok (:value "1")) (miao::%call-result '(:ok (:value "1")) nil))))

(test call-result-folds-a-timeout
  (is (equal '(:error :timeout) (miao::%call-result nil :timeout))))

(test call-result-folds-a-down-status-to-unavailable
  (is (equal '(:error :unavailable) (miao::%call-result nil (list :down :shutdown)))))

(test call-result-stringifies-a-deadlock-status
  (let ((result (miao::%call-result nil (list :deadlock (list :some-process)))))
    (is (eq :error (first result)))
    (is (eq :error (first (second result))))
    (is (stringp (second (second result))))))

;;; --- the convention --------------------------------------------------

(test tools-are-discoverable-via-props
  (with-tools
    ;; kind=tool in the registration props, found through names + lookup:
    ;; the context and meow's own entries must not appear.
    (is (equal '(:tool-eval :tool-fs :tool-gated-eval :tool-http :tool-image :tool-plan
                 :tool-repl :tool-services :tool-shell)
               (miao:tools)))))

(test metadata-carries-a-trust-level
  (with-tools
    (is (eq :operator (miao:tool-trust (miao:describe-tool :tool-shell))))
    (is (eq :agent (miao:tool-trust (miao:describe-tool :tool-fs))))
    (is (eq :agent (miao:tool-trust (miao:describe-tool :tool-plan))))
    (is (eq :agent (miao:tool-trust (miao:describe-tool :tool-gated-eval))))
    (is (eq :agent (miao:tool-trust (miao:describe-tool :tool-image))))
    (is (eq :agent (miao:tool-trust (miao:describe-tool :tool-services))))
    ;; A tool that names none is an agent tool.
    (is (eq :agent (miao:tool-trust '(:kind :tool))))))

(test describe-returns-convention-metadata
  (with-tools
    (let ((metadata (miao:describe-tool :tool-shell)))
      (is (eq :tool (getf metadata :kind)))
      (is (stringp (getf metadata :summary)))
      ;; The schema renders, so a protocol can put it in a tools array.
      (is (miao:schema->json-schema (miao:tool-schema metadata))))))

(test arguments-are-coerced-against-the-schema
  (with-tools
    ;; A model supplies strings whatever the declared type.
    (is (equal (format nil "ok~%")
               (result-value (tool :tool-shell :cmd "echo ok" :timeout "5000")
                             :out)))
    ;; :op is a member, so the string and the keyword name the same op.
    (tool :tool-fs :op "write" :path "m.txt" :data "x")
    (is (equal "x" (result-value (tool :tool-fs :op :read :path "m.txt") :data)))))

(test an-unknown-parameter-is-a-bad-request
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error
                       (tool :tool-shell :cmd "echo hi" :colour t)))))))

(test a-bare-call-is-coerced-too
  ;; INVOKE-TOOL is not the only way in, so the handler coerces as well.
  (with-tools
    (is (eql 0 (result-value (m:call (m:lookup :tool-shell)
                                     '(:invoke :cmd "true" :timeout "5000")
                                     :timeout 10)
                             :exit)))))

(test a-string-timeout-outlives-the-default-call-timeout
  ;; %CALLER-TIMEOUT reads the coerced arguments: a model-supplied "15000"
  ;; must extend the caller's wait the same way 15000 does.
  (with-tools
    (is (eql 0 (result-value (tool :tool-shell :cmd "sleep 8" :timeout "15000")
                             :exit)))))

(test unknown-message-is-a-bad-request
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error
                       (m:call (m:lookup :tool-shell) '(:nonsense))))))))

;;; --- fs ---------------------------------------------------------------

(test fs-round-trips-inside-the-root
  (with-tools
    (is (eq :ok (first (tool :tool-fs :op :write :path "a/b.txt"
                                     :data "hello fs"))))
    (is (equal "hello fs"
               (result-value (tool :tool-fs :op :read :path "a/b.txt") :data)))
    (is (member "a" (result-value (tool :tool-fs :op :list :path ".") :files)
                :test #'string=))
    (is (eq :ok (first (tool :tool-fs :op :mkdir :path "c/d"))))
    (is (member "c" (result-value (tool :tool-fs :op :list :path ".") :files)
                :test #'string=))
    (is (eq :ok (first (tool :tool-fs :op :delete :path "a/b.txt"))))))

(test fs-refuses-to-delete-a-directory
  (with-tools
    (tool :tool-fs :op :mkdir :path "keep")
    (is (equal :bad-request
               (first (miao:tool-error
                       (tool :tool-fs :op :delete :path "keep")))))))

(test fs-rejects-path-escapes
  (with-tools
    (dolist (path '("../outside.txt" "/etc/passwd" "../sandbox-root-evil"
                    "a/../../outside.txt"))
      (is (equal '(:forbidden "path escapes sandbox root")
                 (miao:tool-error (tool :tool-fs :op :read :path path))))))

  ;; The sibling-prefix trick specifically: a directory whose name merely
  ;; starts with the root's name is not inside it.
  (with-tools
    (let ((sibling (concatenate 'string *sandbox* "-evil")))
      (is (equal '(:forbidden "path escapes sandbox root")
                 (miao:tool-error (tool :tool-fs :op :list :path sibling)))))))

(test fs-requires-a-path
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error (tool :tool-fs :op :read)))))))

(test fs-write-without-data-is-refused-before-touching-the-filesystem
  (with-tools
    (let ((result (miao:tool-error (tool :tool-fs :op :write :path "new/dir/f.txt"))))
      (is (eq :bad-request (first result)))
      (is (search ":data is required when :op is write" (second result))))
    (is (not (member "new" (result-value (tool :tool-fs :op :list :path ".") :files)
                     :test #'string=)))))

(test a-missing-conditional-parameter-is-refused-by-every-tool
  (with-tools
    (m:mount *context* 'miao:tool-vault)
    (m:mount *context* 'miao:tool-self)
    (dolist (case '((:tool-services (:op :describe) ":name")
                    (:tool-image (:op :describe) ":symbol")
                    (:tool-image (:op :apropos) ":pattern")
                    (:tool-vault (:op :restore) ":id")
                    (:tool-vault (:op :discard) ":id")
                    (:tool-self (:op :eval) ":form")
                    (:tool-self (:op :define) ":form")
                    (:tool-self (:op :reload) ":name")))
      (destructuring-bind (name args parameter) case
        (let ((result (miao:tool-error (apply #'tool name args))))
          (is (eq :bad-request (first result)))
          (is (search parameter (second result))))))))

;;; --- fs: symlinks (~takeiteasy/miao#15) --------------------------------

(defun make-symlink (target link)
  (uiop:run-program (list "ln" "-s" target link) :output nil :error-output nil))

(defun write-file (path contents)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents s)))

(test fs-refuses-a-symlink-to-a-file-outside-the-root
  (with-tools
    (let ((outside (format nil "~a-outside.txt" *sandbox*)))
      (write-file outside "secret")
      (unwind-protect
           (progn
             (make-symlink outside (concatenate 'string *sandbox* "/link.txt"))
             (is (equal '(:forbidden "path escapes sandbox root")
                        (miao:tool-error (tool :tool-fs :op :read :path "link.txt")))))
        (delete-file outside)))))

(test fs-refuses-traversal-through-a-symlinked-directory
  (with-tools
    (let ((outside (format nil "~a-outside" *sandbox*)))
      (ensure-directories-exist (concatenate 'string outside "/"))
      (write-file (concatenate 'string outside "/x.txt") "secret")
      (unwind-protect
           (progn
             (make-symlink outside (concatenate 'string *sandbox* "/linkdir"))
             (is (equal '(:forbidden "path escapes sandbox root")
                        (miao:tool-error (tool :tool-fs :op :read :path "linkdir/x.txt")))))
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname outside)
                                    :validate t :if-does-not-exist :ignore)))))

(test fs-refuses-a-dangling-symlink
  (with-tools
    (let ((target (format nil "~a-created-by-attack.txt" *sandbox*)))
      (make-symlink target (concatenate 'string *sandbox* "/dangle"))
      (is (equal '(:forbidden "path escapes sandbox root")
                 (miao:tool-error (tool :tool-fs :op :write :path "dangle" :data "x"))))
      (is (not (uiop:file-exists-p target))))))

(test fs-refuses-any-symlink-below-the-root
  ;; ~takeiteasy/miao#52: the atomic walk refuses every symlink below the
  ;; root outright, rather than resolving an in-root one and re-checking
  ;; it -- there is no path-based re-check left to race.
  (with-tools
    (tool :tool-fs :op :write :path "real.txt" :data "hello")
    (make-symlink (concatenate 'string *sandbox* "/real.txt")
                  (concatenate 'string *sandbox* "/alias.txt"))
    (is (equal '(:forbidden "path escapes sandbox root")
               (miao:tool-error (tool :tool-fs :op :read :path "alias.txt"))))))

(test fs-rmdir-removes-an-empty-directory
  (with-tools
    (tool :tool-fs :op :mkdir :path "gone/inner")
    (is (eq :ok (first (tool :tool-fs :op :rmdir :path "gone/inner"))))
    (is (null (result-value (tool :tool-fs :op :list :path "gone") :files)))
    (is (eq :ok (first (tool :tool-fs :op :rmdir :path "gone"))))
    (is (not (member "gone" (result-value (tool :tool-fs :op :list :path ".") :files)
                     :test #'string=)))))

(test fs-rmdir-refuses-a-non-empty-directory
  (with-tools
    (tool :tool-fs :op :write :path "full/a.txt" :data "x")
    (is (equal '(:bad-request "directory not empty")
               (miao:tool-error (tool :tool-fs :op :rmdir :path "full"))))
    (is (equal "x" (result-value (tool :tool-fs :op :read :path "full/a.txt") :data)))))

(test fs-rmdir-refuses-a-file
  (with-tools
    (tool :tool-fs :op :write :path "f.txt" :data "x")
    (is (equal '(:bad-request "not a directory")
               (miao:tool-error (tool :tool-fs :op :rmdir :path "f.txt"))))
    (is (equal "x" (result-value (tool :tool-fs :op :read :path "f.txt") :data)))))

(test fs-rmdir-refuses-a-symlink-leaf
  (with-tools
    (tool :tool-fs :op :mkdir :path "real")
    (make-symlink (concatenate 'string *sandbox* "/real")
                  (concatenate 'string *sandbox* "/alias"))
    (is (equal '(:forbidden "path escapes sandbox root")
               (miao:tool-error (tool :tool-fs :op :rmdir :path "alias"))))
    (is (eq :ok (first (tool :tool-fs :op :list :path "real"))))))

(test fs-rmdir-refuses-the-root
  (with-tools
    (is (equal :bad-request (first (miao:tool-error (tool :tool-fs :op :rmdir :path ".")))))
    (is (eq :ok (first (tool :tool-fs :op :list :path "."))))))

(test fs-rmdir-reports-a-missing-path
  (with-tools
    (is (equal '(:error "no such file")
               (miao:tool-error (tool :tool-fs :op :rmdir :path "nope"))))))

(test fs-refuses-to-delete-a-symlink-leaf
  (with-tools
    (tool :tool-fs :op :write :path "real.txt" :data "hello")
    (make-symlink (concatenate 'string *sandbox* "/real.txt")
                  (concatenate 'string *sandbox* "/alias.txt"))
    (is (equal '(:forbidden "path escapes sandbox root")
               (miao:tool-error (tool :tool-fs :op :delete :path "alias.txt"))))
    (is (equal "hello" (result-value (tool :tool-fs :op :read :path "real.txt") :data)))))

;;; --- define-tool: readable defaults (~takeiteasy/miao#134) ---------------

(defun define-tool-with-defaults (&rest default-forms)
  (eval `(miao:define-tool :tool-default-probe
             (:summary "probe"
              :params ,(loop for form in default-forms
                             for i from 0
                             collect `(,(alexandria:make-keyword (format nil "P~d" i))
                                       string :default ,form)))
           (:invoke () (miao::ok)))))

(test define-tool-refuses-a-default-that-does-not-read-back
  (signals error (define-tool-with-defaults 1 '#'identity))
  (signals error (define-tool-with-defaults '(make-hash-table))))

(test define-tool-accepts-a-default-that-reads-back
  (finishes (define-tool-with-defaults 3 :a "s" t 'miao::+default-tool-timeout+)))

;;; A tool built without define-tool is checked when it is mounted (#156).

(defun define-hand-written-tool (default-form)
  (eval `(progn
           (m:defservice tool-hand-probe () () (:name :tool-hand-probe))
           (defmethod m:metadata ((service tool-hand-probe))
             (list :kind :tool :name :tool-hand-probe :trust :agent :summary "probe"
                   :params (list (list :p 'string :default ,default-form))))
           (miao::define-tool-handler tool-hand-probe (service args)
             (miao::ok)))))

(test a-hand-written-tool-with-an-unreadable-default-fails-its-mount
  (define-hand-written-tool '#'identity)
  (with-tools
    (is (search "does not print and read back"
                (handler-case (m:mount *context* 'tool-hand-probe)
                  (error (e) (princ-to-string e)))))
    (is (null (m:lookup :tool-hand-probe)))))

(test a-hand-written-tool-with-a-readable-default-mounts
  (define-hand-written-tool 3)
  (with-tools
    (m:mount *context* 'tool-hand-probe)
    (is (equal '(:default 3)
               (miao::param-options (first (miao:tool-schema
                                            (miao:describe-tool :tool-hand-probe))))))))

;;; --- fs: fd-relative walk (~takeiteasy/miao#59) -------------------------

(test fs-walk-never-changes-the-process-cwd
  (with-tools
    (let* ((stop nil)
           (changed nil)
           (start (namestring (uiop:getcwd)))
           (watcher (bt:make-thread
                     (lambda ()
                       (loop until stop
                             do (unless (string= start (namestring (uiop:getcwd)))
                                  (setf changed t)))))))
      (unwind-protect
           (dotimes (i 200)
             (let ((path (format nil "d~d/e/f~d.txt" (mod i 5) i)))
               (tool :tool-fs :op :write :path path :data "x")
               (tool :tool-fs :op :read :path path)
               (tool :tool-fs :op :list :path (format nil "d~d/e" (mod i 5)))
               (tool :tool-fs :op :mkdir :path (format nil "m~d/n" i))
               (tool :tool-fs :op :delete :path path)))
        (setf stop t)
        (bt:join-thread watcher))
      (is (not changed))
      (is (string= start (namestring (uiop:getcwd)))))))

(test fs-write-creates-a-file-with-the-usual-mode
  (with-tools
    (let ((old (miao::posix-umask #o022)))
      (unwind-protect (tool :tool-fs :op :write :path "m.txt" :data "x")
        (miao::posix-umask old)))
    (is (= #o644 (miao::file-mode (concatenate 'string *sandbox* "/m.txt"))))))

(defun listing-fixture ()
  (write-file (concatenate 'string *sandbox* "/.hidden") "h")
  (write-file (concatenate 'string *sandbox* "/real.txt") "r")
  (ensure-directories-exist (concatenate 'string *sandbox* "/sub/"))
  (make-symlink "real.txt" (concatenate 'string *sandbox* "/live"))
  (make-symlink "nowhere" (concatenate 'string *sandbox* "/dangling")))

(test fs-list-shows-every-entry-by-default
  (with-tools
    (listing-fixture)
    (is (equal '(".hidden" "dangling" "live" "real.txt" "sub")
               (result-value (tool :tool-fs :op :list :path ".") :files)))))

(test fs-list-hides-symlinks-when-asked
  (with-tools
    (listing-fixture)
    (is (equal '(".hidden" "real.txt" "sub")
               (result-value (miao::apply-fs-op :list *sandbox* *sandbox* nil :text t) :files)))))

;;; --- shell -------------------------------------------------------------

(test shell-captures-merged-output-and-exit-status
  (with-tools
    (let ((result (tool :tool-shell :cmd "echo out; echo err 1>&2")))
      (is (eql 0 (result-value result :exit)))
      (is (equal "out
err
" (result-value result :out))))
    (is (eql 3 (result-value (tool :tool-shell :cmd "exit 3") :exit)))))

(test shell-enforces-its-timeout-and-stays-alive
  (with-tools
    (let ((started (get-internal-real-time)))
      (is (eq :timeout (miao:tool-error
                        (tool :tool-shell :cmd "sleep 30" :timeout 300))))
      (is (< (/ (- (get-internal-real-time) started)
                internal-time-units-per-second)
             5)))
    ;; The service must still answer afterwards.
    (is (equal "still-here
" (result-value (tool :tool-shell :cmd "echo still-here") :out)))))

(test shell-registers-a-command-only-while-it-runs
  (miao::run-command "true" 5000)
  (is (null miao::*live-commands*))
  (miao::run-command "sleep 30" 200)
  (is (null miao::*live-commands*)))

(test kill-live-commands-kills-a-running-command
  (let* ((result nil)
         (thread (bt:make-thread (lambda () (setf result (miao::run-command "sleep 30" 30000))))))
    (is-true (eventually (lambda () miao::*live-commands*)))
    (miao::kill-live-commands)
    (is-true (eventually (lambda () (not (bt:thread-alive-p thread)))))
    (is (not (eql 0 (getf (second result) :exit))))
    (is (null miao::*live-commands*))))

(test shell-rejects-a-missing-command
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error (tool :tool-shell :timeout 100)))))))

(test invoke-outlives-the-default-call-timeout
  ;; M:CALL defaults to 5s. A tool given a longer deadline must not be cut
  ;; short at the caller while its own work is still within bounds.
  (with-tools
    (is (eql 0 (result-value (tool :tool-shell :cmd "sleep 8" :timeout 15000)
                             :exit)))))

(test shell-kills-a-backgrounded-descendant-on-timeout
  ;; ~takeiteasy/miao#16: the deadline used to signal the direct `sh` child
  ;; only, so a backgrounded grandchild outlived it.
  (if (not (process-group-containment-available-p))
      (skip "no process-group mechanism on this host")
      (with-tools
        (let ((pidfile (format nil "~a/miao-shell-pgid-test.pid"
                               (uiop:native-namestring (uiop:temporary-directory)))))
          (unwind-protect
               (progn
                 (is (eq :timeout
                        (miao:tool-error
                         (tool :tool-shell
                               :cmd (format nil "sleep 30 & echo $! > ~a; wait" pidfile)
                               :timeout 500))))
                 (let ((grandchild (with-open-file (s pidfile) (parse-integer (read-line s)))))
                   (is (wait-for-exit grandchild))))
            (ignore-errors (delete-file pidfile)))))))

(test shell-kills-a-backgrounded-descendant-under-the-tree-fallback
  ;; ~takeiteasy/miao#54: with no OS grouping mechanism at all, containment
  ;; falls back to walking and killing the descendant tree by hand.
  (let ((miao::*process-group-strategy* :tree))
    (with-tools
      (let ((pidfile (format nil "~a/miao-shell-tree-test.pid"
                             (uiop:native-namestring (uiop:temporary-directory)))))
        (unwind-protect
             (progn
               (is (eq :timeout
                      (miao:tool-error
                       (tool :tool-shell
                             :cmd (format nil "sleep 30 & echo $! > ~a; wait" pidfile)
                             :timeout 500))))
               (let ((grandchild (with-open-file (s pidfile) (parse-integer (read-line s)))))
                 (is (wait-for-exit grandchild))))
          (ignore-errors (delete-file pidfile)))))))

;;; --- http ---------------------------------------------------------------

(defvar *stall* nil
  "Set around a request to /stall to hold the fake server's response, and
cleared again so STOP-FAKE-HTTP's join does not wait on it.")

(defun echo-handler (&key method path headers body)
  (declare (ignore method))
  (cond
    ((string= path "/echo") (list 200 '("Content-Type" "text/plain") body))
    ((string= path "/slow") (sleep 1.5) (list 200 '() ""))
    ((string= path "/stall") (loop while *stall* do (sleep 0.02)) (list 200 '() ""))
    ((string= path "/moved") (list 302 '("Location" "/echo") ""))
    ((string= path "/boom") (list 500 '() "kaboom"))
    ((string= path "/crash") (error "the handler crashed"))
    ((string= path "/seen") (list 200 '() (or (getf-string headers "x-tag") "")))
    (t (list 404 '("Content-Type" "application/json") "{\"error\":\"nf\"}"))))

(defmacro with-fake-http ((url) &body body)
  `(let ((server (start-fake-http #'echo-handler)))
     (unwind-protect (let ((,url (fake-http-url server))) ,@body)
       (stop-fake-http server))))

(test http-passes-statuses-through-untouched
  (with-tools
    (with-fake-http (url)
      (is (eql 200 (result-value (tool :tool-http :url (format nil "~a/echo" url))
                                 :status)))
      (is (eql 404 (result-value (tool :tool-http :url (format nil "~a/nope" url))
                                 :status)))
      (is (eql 500 (result-value (tool :tool-http :url (format nil "~a/boom" url))
                                 :status))))))

(test fake-http-keeps-serving-after-a-connection-fails
  ;; Serving a client that has gone -- a cancelled exchange, where Linux
  ;; fails the write -- must not take the server down for the next request.
  (with-tools
    (with-fake-http (url)
      (is (miao:tool-error-p (tool :tool-http :url (format nil "~a/crash" url)
                                   :timeout 2000)))
      (is (eql 200 (result-value (tool :tool-http :url (format nil "~a/echo" url)
                                       :timeout 2000)
                                 :status))))))

(test http-does-not-follow-redirects
  (with-tools
    (with-fake-http (url)
      (is (eql 302 (result-value
                    (tool :tool-http :url (format nil "~a/moved" url))
                    :status))))))

(test http-lower-cases-response-headers
  (with-tools
    (with-fake-http (url)
      (let ((headers (result-value
                      (tool :tool-http :url (format nil "~a/echo" url))
                      :headers)))
        (is (equal "text/plain" (getf-string headers "content-type")))))))

(test http-sends-body-and-extra-headers
  (with-tools
    (with-fake-http (url)
      (is (equal "payload=1"
                 (result-value (tool :tool-http :url (format nil "~a/echo" url)
                                               :method "POST"
                                               :body "payload=1")
                               :body)))
      (is (equal "abc"
                 (result-value (tool :tool-http :url (format nil "~a/seen" url)
                                               :method "POST"
                                               :headers '(:x-tag "abc")
                                               :body "x")
                               :body))))))

(test http-sends-a-non-ascii-body-as-utf-8
  (with-tools
    (let ((server (start-fake-http #'echo-handler)))
      (unwind-protect
           (progn
             (tool :tool-http :url (format nil "~a/echo" (fake-http-url server))
                              :method "POST"
                              :body +non-ascii-text+)
             (is (equal +non-ascii-text+
                        (getf (first (fake-http-requests server)) :body))))
        (stop-fake-http server)))))

(defun http-body-of (answer)
  "The :body tool-http returns for a server that answers ANSWER."
  (let ((server (start-fake-http (lambda (&rest request)
                                   (declare (ignore request))
                                   answer))))
    (unwind-protect
         (result-value (tool :tool-http :url (format nil "~a/any" (fake-http-url server)))
                       :body)
      (stop-fake-http server))))

(defun octets (&rest bytes)
  (coerce bytes '(vector (unsigned-byte 8))))

(defun utf-8-octets (text)
  (flexi-streams:string-to-octets text :external-format :utf-8))

(test http-decodes-a-text-response-with-no-charset-as-utf-8
  (with-tools
    (is (equal +non-ascii-text+
               (http-body-of (list :octets 200 '("Content-Type" "text/plain")
                                   (utf-8-octets +non-ascii-text+)))))))

(test http-decodes-a-response-in-its-declared-charset
  (with-tools
    (is (equal (format nil "h~cllo" (code-char #xe9))
               (http-body-of (list :octets 200
                                   '("Content-Type" "text/plain; charset=ISO-8859-1")
                                   (octets 104 233 108 108 111)))))))

(test http-falls-back-to-latin-1-when-a-body-is-not-utf-8
  (with-tools
    (is (equal (format nil "h~cllo" (code-char #xe9))
               (http-body-of (list :octets 200 '("Content-Type" "text/plain")
                                   (octets 104 233 108 108 111)))))))

(test http-ignores-a-charset-it-does-not-know
  (with-tools
    (is (equal +non-ascii-text+
               (http-body-of (list :octets 200
                                   '("Content-Type" "text/plain; charset=x-no-such")
                                   (utf-8-octets +non-ascii-text+)))))))

(test http-decodes-an-empty-body-to-an-empty-string
  (with-tools
    (is (equal "" (http-body-of (list 204 '() ""))))))

(defun http-result-of (answer)
  "The whole (:ok plist) tool-http returns for a server that answers ANSWER."
  (let ((server (start-fake-http (lambda (&rest request)
                                   (declare (ignore request))
                                   answer))))
    (unwind-protect
         (tool :tool-http :url (format nil "~a/any" (fake-http-url server)))
      (stop-fake-http server))))

(defparameter +png-octets+ (octets #x89 80 78 71 13 10 26 10 0 255 254))

(test http-returns-a-non-text-body-as-base64
  (with-tools
    (let ((result (http-result-of (list :octets 200 '("Content-Type" "image/png")
                                        +png-octets+))))
      (is (equal "base64" (result-value result :body-encoding)))
      (is (equalp +png-octets+
                  (cl-base64:base64-string-to-usb8-array (result-value result :body)))))))

(test http-labels-a-text-body-as-text
  (with-tools
    (is (equal "text" (result-value (http-result-of (list 200 '("Content-Type" "text/plain") "hi"))
                                    :body-encoding)))))

(test http-treats-a-structured-suffix-type-as-text
  (with-tools
    (let ((result (http-result-of (list :octets 200
                                        '("Content-Type" "application/vnd.x+json; charset=utf-8")
                                        (utf-8-octets "{}")))))
      (is (equal "{}" (result-value result :body)))
      (is (equal "text" (result-value result :body-encoding))))))

(test http-decodes-utf-8-with-no-content-type-as-text
  (with-tools
    (let ((result (http-result-of (list :octets 200 '() (utf-8-octets +non-ascii-text+)))))
      (is (equal +non-ascii-text+ (result-value result :body)))
      (is (equal "text" (result-value result :body-encoding))))))

(test http-returns-invalid-utf-8-with-no-content-type-as-base64
  (with-tools
    (is (equal "base64"
               (result-value (http-result-of (list :octets 200 '() (octets 104 233 108)))
                             :body-encoding)))))

(test http-returns-an-empty-non-text-body-as-an-empty-string
  (with-tools
    (is (equal "" (http-body-of (list :octets 200 '("Content-Type" "image/png") (octets)))))))

;;; --- fs: base64 bytes (~takeiteasy/miao#155) -----------------------------

(defun base64-of (octets)
  (cl-base64:usb8-array-to-base64-string octets))

(test fs-round-trips-bytes-as-base64
  (with-tools
    (is (eq :ok (first (tool :tool-fs :op :write :path "b.bin" :encoding :base64
                                      :data (base64-of +png-octets+)))))
    (is (equalp +png-octets+
                (alexandria:read-file-into-byte-vector
                 (format nil "~a/b.bin" *sandbox*))))
    (is (equal (base64-of +png-octets+)
               (result-value (tool :tool-fs :op :read :path "b.bin" :encoding :base64)
                             :data)))))

(test fs-writes-an-empty-base64-body-as-an-empty-file
  (with-tools
    (tool :tool-fs :op :write :path "e.bin" :encoding :base64 :data "")
    (is (zerop (length (alexandria:read-file-into-byte-vector
                        (format nil "~a/e.bin" *sandbox*)))))))

(test fs-refuses-invalid-base64-and-keeps-the-file
  (with-tools
    (tool :tool-fs :op :write :path "k.txt" :data "keep")
    (is (equal :bad-request
               (first (miao:tool-error (tool :tool-fs :op :write :path "k.txt"
                                                 :encoding :base64 :data "a$b!")))))
    (is (equal "keep" (result-value (tool :tool-fs :op :read :path "k.txt") :data)))))

(test fs-text-read-of-non-utf-8-is-a-bad-request
  (with-tools
    (tool :tool-fs :op :write :path "n.bin" :encoding :base64 :data (base64-of +png-octets+))
    (is (eq :bad-request (first (miao:tool-error (tool :tool-fs :op :read :path "n.bin")))))
    (is (equal (base64-of +png-octets+)
               (result-value (tool :tool-fs :op :read :path "n.bin" :encoding :base64) :data)))))

(test fs-saves-a-tool-http-body
  (with-tools
    (let ((result (http-result-of (list :octets 200 '("Content-Type" "image/png")
                                        +png-octets+))))
      (tool :tool-fs :op :write :path "saved.png" :encoding (result-value result :body-encoding)
                     :data (result-value result :body))
      (is (equalp +png-octets+
                  (alexandria:read-file-into-byte-vector
                   (format nil "~a/saved.png" *sandbox*)))))))

(test declared-charset-reads-the-charset-parameter
  (is (eq :utf-8 (miao::declared-charset "text/plain; charset=UTF-8")))
  (is (eq :utf-8 (miao::declared-charset "text/plain; charset=\"utf-8\"; format=flowed")))
  (is (eq :iso-8859-1 (miao::declared-charset "text/html;charset=iso-8859-1")))
  (is (null (miao::declared-charset "text/plain")))
  (is (null (miao::declared-charset nil)))
  (is (null (miao::declared-charset "text/plain; charset=x-no-such"))))

(test http-honours-a-caller-supplied-content-type
  (with-tools
    (let ((server (start-fake-http #'echo-handler)))
      (unwind-protect
           (progn
             (tool :tool-http :url (format nil "~a/echo" (fake-http-url server))
                              :method "POST"
                              :headers '("Content-Type" "application/json")
                              :body "{\"a\":1}")
             (let ((headers (getf (first (fake-http-requests server)) :headers)))
               ;; Sent once, as the caller asked -- not clobbered to
               ;; application/octet-stream, and not duplicated.
               (is (equal "application/json"
                          (getf-string headers "content-type")))
               (is (eql 1 (count "content-type"
                                 (loop for (name) on headers by #'cddr
                                       collect name)
                                 :test #'string=)))))
        (stop-fake-http server)))))

(test http-rejects-malformed-headers
  (with-tools
    ;; An alist has an even length too; it must not pass as one empty header.
    (dolist (headers '((("X-Tag" . "a") ("Y" . "b")) ("X-Tag") (:x-tag 7)))
      (is (equal :bad-request
                 (first (miao:tool-error
                         (tool :tool-http :url "http://127.0.0.1:1/x"
                                          :headers headers))))))))

(test http-rejects-a-missing-url-without-hitting-the-wire
  (with-tools
    (let ((server (start-fake-http #'echo-handler)))
      (unwind-protect
           (progn
             (is (equal :bad-request
                        (first (miao:tool-error (tool :tool-http :method "GET")))))
             (is (null (fake-http-requests server))))
        (stop-fake-http server)))))

(test http-timeout-is-bounded-and-surfaces-as-an-error
  (with-tools
    (with-fake-http (url)
      (is (eq :timeout
              (miao:tool-error (tool :tool-http :url (format nil "~a/slow" url)
                                                :timeout 300)))))))

(test http-unreachable-host-is-unavailable
  (with-tools
    (let* ((server (start-fake-http #'echo-handler))
           (url (fake-http-url server)))
      (stop-fake-http server)
      (is (eq :unavailable
              (miao:tool-error (tool :tool-http :url (format nil "~a/echo" url)
                                                :timeout 5000)))))))

(defun http-threads ()
  (remove-if-not (lambda (name) (and name (search "miao-http" name)))
                 (mapcar #'bt:thread-name (bt:all-threads))))

(test an-http-exchange-runs-on-no-thread-of-its-own
  ;; *STALL* holds the fake server's response, so the exchange is in flight
  ;; until the deadline ends it. SETF rather than LET, since the handler runs
  ;; on the fake server's own thread, which does not see a dynamic binding
  ;; made on this one.
  (with-tools
    (with-fake-http (url)
      (setf *stall* t)
      (unwind-protect
           (let ((call (in-thread (lambda ()
                                    (tool :tool-http :url (format nil "~a/stall" url)
                                                     :timeout 600)))))
             (sleep 0.3)
             (is (null (http-threads)))
             (is (eq :timeout (miao:tool-error (bt:join-thread call))))
             (is (null (http-threads))))
        (setf *stall* nil)))))

(test a-timed-out-exchange-does-not-disturb-the-next-call
  (with-tools
    (with-fake-http (url)
      (setf *stall* t)
      (unwind-protect
           (dotimes (i 2)
             (is (eq :timeout
                     (miao:tool-error (tool :tool-http :url (format nil "~a/stall" url)
                                                       :timeout 300)))))
        (setf *stall* nil))
      (is (eql 200 (result-value (tool :tool-http :url (format nil "~a/echo" url)) :status))))))

(test http-https-round-trip
  ;; Off by default: CI must not depend on the network. Exercises the
  ;; SSL-wrapped stream PERFORM-REQUEST builds for :STREAM
  ;; (~takeiteasy/miao#17), which drakma never attaches on its own.
  (if (uiop:getenv "MIAO_LIVE_HTTP")
      (with-tools
        (is (eql 404 (result-value
                      (tool :tool-http :url "https://httpbingo.org/status/404")
                      :status))))
      (skip "set MIAO_LIVE_HTTP to run live HTTP tests")))

;;; --- eval --------------------------------------------------------------

(test eval-returns-the-value-and-the-output
  (with-tools
    (let ((result (tool :tool-eval :form "(progn (princ \"printed\") (+ 1 2))")))
      (is (equal "3" (result-value result :value)))
      (is (equal "printed" (result-value result :out))))))

(test eval-reports-a-reader-error-as-a-bad-request
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error (tool :tool-eval :form "(+ 1")))))))

(test eval-reports-a-signalled-form-as-an-error
  (with-tools
    (let ((reason (miao:tool-error (tool :tool-eval :form "(error \"boom\")"))))
      (is (eq :error (first reason)))
      (is (search "boom" (second reason))))))

(test eval-does-not-evaluate-at-read-time
  ;; *READ-EVAL* is nil in the worker, so #. never runs.
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error
                       (tool :tool-eval :form "'#.(error \"read-eval ran\")")))))))

(test eval-keeps-no-state-between-calls
  (with-tools
    (is (eq :ok (first (tool :tool-eval :form "(defparameter *x* 1)"))))
    (let ((reason (miao:tool-error (tool :tool-eval :form "*x*"))))
      (is (eq :error (first reason))))))

(test eval-enforces-its-timeout-and-stays-alive
  (with-tools
    (is (eq :timeout (miao:tool-error (tool :tool-eval :form "(loop)"
                                                       :timeout 500))))
    (is (equal "4" (result-value (tool :tool-eval :form "(+ 2 2)") :value)))))

(test eval-requires-a-form
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error (tool :tool-eval :timeout 100)))))))

;;; --- gated eval --------------------------------------------------------

(test gated-eval-returns-the-value-and-the-output
  (with-tools
    (let ((result (tool :tool-gated-eval :form "(progn (princ \"printed\") (+ 1 2))")))
      (is (equal "3" (result-value result :value)))
      (is (equal "printed" (result-value result :out))))))

(test gated-eval-runs-loop-labels-and-hash-tables
  (with-tools
    (is (equal "(1 4 9)"
               (result-value (tool :tool-gated-eval
                                   :form "(loop for x in '(1 2 3) collect (* x x))")
                             :value)))
    (is (equal "120"
               (result-value (tool :tool-gated-eval
                                   :form "(labels ((f (n) (if (< n 2) 1 (* n (f (1- n)))))) (f 5))")
                             :value)))
    (is (equal "2"
               (result-value (tool :tool-gated-eval
                                   :form "(let ((h (make-hash-table))) (setf (gethash 1 h) 2) (gethash 1 h))")
                             :value)))
    (is (equal "\"1.5d0 X\""
               (result-value (tool :tool-gated-eval :form "(format nil \"~a ~a\" 1.5 'x)")
                             :value)))))

(test gated-eval-refuses-a-form-as-a-bad-request-before-any-worker-starts
  (with-tools
    (let ((before miao::*live-workers*))
      (is (eq :bad-request (first (miao:tool-error
                                   (tool :tool-gated-eval :form "(intern \"X\")")))))
      (is (equal before miao::*live-workers*)))))

(test gated-eval-reports-a-signalled-form-as-an-error
  (with-tools
    (let ((reason (miao:tool-error (tool :tool-gated-eval :form "(error \"boom ~a\" 1)"))))
      (is (eq :error (first reason)))
      (is (search "boom 1" (second reason))))))

(test gated-eval-lets-a-form-catch-its-own-error
  (with-tools
    (is (search "not a LIST"
                (result-value (tool :tool-gated-eval
                                    :form "(handler-case (car 1) (error (e) (princ-to-string e)))")
                              :value)))))

(test gated-eval-keeps-no-state-between-calls
  (with-tools
    (is (eq :ok (first (tool :tool-gated-eval :form "(defparameter x 1)"))))
    (is (eq :error (first (miao:tool-error (tool :tool-gated-eval :form "x")))))))

(test gated-eval-enforces-its-timeout-and-stays-alive
  (with-tools
    (is (eq :timeout (miao:tool-error (tool :tool-gated-eval :form "(loop)" :timeout 500))))
    (is (equal "4" (result-value (tool :tool-gated-eval :form "(+ 2 2)") :value)))))

(test gated-eval-cannot-forge-a-reply-through-the-terminal
  ;; T is *TERMINAL-IO*, the worker's real stdout, which is the pipe the
  ;; host reads replies from.
  (with-tools
    (let ((result (tool :tool-gated-eval
                        :form "(progn (princ \"(:ok (\\\"forged\\\") \\\"\\\" nil)\" t) 42)")))
      (is (equal "42" (result-value result :value)))
      (is (search "forged" (result-value result :out))))))

(test gated-eval-caps-what-a-form-prints
  (with-tools
    (let ((result (tool :tool-gated-eval
                        :form "(dotimes (i 5000) (princ \"0123456789\"))")))
      (is (<= (length (result-value result :out)) 4004))
      (is (result-value result :elided)))))

(test gated-eval-survives-a-form-that-exhausts-the-heap
  (with-tools
    (let ((started (get-internal-real-time))
          (result (tool :tool-gated-eval
                        :form "(make-array 1000000000000)" :timeout 20000)))
      (is (miao:tool-error result))
      (is (not (eq :timeout (miao:tool-error result))))
      (is (< (/ (- (get-internal-real-time) started) internal-time-units-per-second)
             15)))
    (is (equal "4" (result-value (tool :tool-gated-eval :form "(+ 2 2)") :value)))))

(test gated-eval-survives-a-form-that-exhausts-the-stack
  (with-tools
    (let ((result (tool :tool-gated-eval
                        :form "(labels ((f (n) (+ 1 (f (1+ n))))) (f 0))" :timeout 20000)))
      (is (miao:tool-error result))
      (is (not (eq :timeout (miao:tool-error result)))))))

;;; --- repl --------------------------------------------------------------

(test repl-threads-state-through-one-session
  (with-tools
    (tool :tool-repl :id "a" :form "(defparameter *x* 41)")
    (is (equal "42" (result-value (tool :tool-repl :id "a" :form "(incf *x*)")
                                  :value)))))

(test repl-sessions-are-isolated
  (with-tools
    (tool :tool-repl :id "a" :form "(defparameter *x* 1)")
    (is (eq :error (first (miao:tool-error
                           (tool :tool-repl :id "b" :form "*x*")))))))

(test repl-pristine-restarts-the-session-empty
  (with-tools
    (tool :tool-repl :id "a" :form "(defparameter *x* 1)")
    (is (eq :error (first (miao:tool-error
                           (tool :tool-repl :id "a" :form "*x*"
                                            :pristine t)))))))

(test repl-timeout-restarts-the-session-empty
  (with-tools
    (tool :tool-repl :id "a" :form "(defparameter *x* 1)")
    (is (eq :timeout (miao:tool-error (tool :tool-repl :id "a" :form "(loop)"
                                                       :timeout 500))))
    ;; The killed worker is forgotten, so the id answers again -- empty.
    (is (eq :error (first (miao:tool-error
                           (tool :tool-repl :id "a" :form "*x*")))))))

(test repl-session-from-an-earlier-image-is-reported-lost-then-starts-empty
  ;; Bumping *BOOT* makes the live worker read as inherited from a saved
  ;; core: it must be dropped without being signalled.
  (with-tools
    (tool :tool-repl :id "a" :form "(defparameter *x* 1)")
    (let ((old-pid (parse-integer
                    (result-value (tool :tool-repl :id "a" :form +getpid-form+) :value)))
          (boot miao::*boot*))
      (unwind-protect
           (progn
             (setf miao::*boot* (list :later-boot))
             (let ((lost (tool :tool-repl :id "a" :form "*x*")))
               (is (eq :error (first (miao:tool-error lost))))
               (is (search "relaunch" (second (miao:tool-error lost)))))
             (is (unix-process-alive-p old-pid))
             (is (equal "NIL" (result-value (tool :tool-repl :id "a" :form "(boundp '*x*)")
                                            :value)))
             ;; the second worker dies under the token it started with
             (m:stop-and-wait *context*))
        (setf miao::*boot* boot)
        (uiop:run-program (list "/bin/kill" "-9" (princ-to-string old-pid))
                          :ignore-error-status t)))))

(test repl-workers-die-with-the-service
  (let ((pids '()))
    (with-tools
      (dolist (id '("a" "b"))
        (push (parse-integer
               (result-value (tool :tool-repl :id id :form +getpid-form+) :value))
              pids))
      (is (every #'unix-process-alive-p pids)))
    ;; The fixture stopped the context, which unwinds the effect holding
    ;; each worker.
    (dolist (pid pids)
      (is (wait-for-exit pid)))))

(test worker-leads-its-own-process-group
  ;; ~takeiteasy/miao#16 also covers workers: a form that backgrounds a
  ;; process must be signalled along with the worker at kill time, which
  ;; needs the worker itself to lead its own group.
  (if (not (process-group-containment-available-p))
      (skip "no process-group mechanism on this host")
      (with-tools
        (let ((pid (parse-integer
                    (result-value (tool :tool-repl :id "g" :form +getpid-form+) :value))))
          (is (eql pid (unix-pgid pid)))))))

(test repl-requires-a-form
  (with-tools
    (is (equal :bad-request
               (first (miao:tool-error (tool :tool-repl :timeout 100)))))))

;;; --- repl concurrency (~takeiteasy/miao#27) ------------------------------

(test repl-sessions-run-concurrently
  ;; A long eval on one id must not block another, or the tool's own
  ;; :describe, which every id's session shares no process with.
  (with-tools
    (let ((thread (tool-thread (tool :tool-repl :id "slow" :form "(sleep 2)"))))
      (sleep 0.2) ;; let "slow"'s session start its eval first
      (let ((start (get-internal-real-time)))
        (is (equal "3" (result-value (tool :tool-repl :id "fast" :form "(+ 1 2)")
                                     :value)))
        (is (< (- (get-internal-real-time) start)
               internal-time-units-per-second))
        (is (miao:describe-tool :tool-repl)))
      (bt:join-thread thread))))

(test repl-calls-on-one-id-still-run-in-order
  (with-tools
    (tool :tool-repl :id "a" :form "(defparameter *log* nil)")
    (let ((threads (list (tool-thread (tool :tool-repl :id "a"
                                            :form "(push 1 *log*)"))
                         (tool-thread (tool :tool-repl :id "a"
                                           :form "(push 2 *log*)")))))
      (mapc #'bt:join-thread threads)
      (is (equal "2" (result-value (tool :tool-repl :id "a" :form "(length *log*)")
                                   :value))))))

(test unmounting-tool-repl-answers-a-queued-call-promptly
  ;; Whichever settles the caller's cell first -- the session replying
  ;; (:error :unavailable), or tool-repl's own process exiting under it as
  ;; an ordinary M:CALL failure, folded by INVOKE-TOOL into the same shape
  ;; (~takeiteasy/miao#106) -- the caller must not be left waiting out its
  ;; full timeout, and either way sees a proper (:error ...) result.
  (with-tools
    (let* ((result nil)
           (thread (tool-thread
                    (setf result (tool :tool-repl :id "a" :form "(sleep 5)")))))
      (sleep 0.2) ;; let the session start its eval first
      (let ((start (get-internal-real-time)))
        (m:unmount *context* :tool-repl)
        (bt:join-thread thread)
        (is (< (- (get-internal-real-time) start)
               (* 2 internal-time-units-per-second))))
      (is-true (miao:tool-error-p result)))))

(test unmounting-tool-repl-refuses-an-eval-queued-behind-the-live-one
  ;; The second call sits in the session's mailbox behind the first, so it
  ;; is still there when the cleanup's own :stop reaches the session --
  ;; it must not start a fresh worker for CLOSED to leave unkilled.
  (with-tools
    (let* ((first-result nil) (second-result nil)
           (first (tool-thread
                   (setf first-result (tool :tool-repl :id "a" :form "(sleep 5)"))))
           (second (progn
                     (sleep 0.1) ;; let the first eval actually start
                     (tool-thread
                      (setf second-result (tool :tool-repl :id "a" :form "1"))))))
      (sleep 0.1) ;; let the second cast actually queue behind the first
      (let ((start (get-internal-real-time)))
        (m:unmount *context* :tool-repl)
        (bt:join-thread first)
        (bt:join-thread second)
        (is (< (- (get-internal-real-time) start)
               (* 2 internal-time-units-per-second))))
      (is-true (miao:tool-error-p first-result))
      (is-true (miao:tool-error-p second-result)))))

;;; --- elision and REPL history (~takeiteasy/miao#26) ---------------------

(test repl-history-reaches-an-elided-value
  (with-tools
    (let ((made (tool :tool-repl :id "a" :form "(make-list 200)")))
      (is (eq t (result-value made :elided)))
      (is (equal "200" (result-value (tool :tool-repl :id "a" :form "(length *)")
                                     :value))))))

(test repl-history-keeps-two-evals-back
  (with-tools
    (tool :tool-repl :id "a" :form "1")
    (tool :tool-repl :id "a" :form "2")
    (is (equal "1" (result-value (tool :tool-repl :id "a" :form "**") :value)))))

(test repl-history-is-untouched-by-an-erroring-form
  (with-tools
    (tool :tool-repl :id "a" :form "41")
    (miao:tool-error (tool :tool-repl :id "a" :form "(error \"boom\")"))
    (is (equal "41" (result-value (tool :tool-repl :id "a" :form "*") :value)))))

;;; --- multiple values (~takeiteasy/miao#105) -----------------------------

(test repl-form-returning-several-values-carries-them-all
  (with-tools
    (let ((result (tool :tool-repl :id "a" :form "(floor 7 2)")))
      (is (equal "3" (result-value result :value)))
      (is (equal '("3" "1") (result-value result :values))))))

(test repl-slash-history-reaches-the-second-value
  ;; / holds the whole value list as one object, the same as the standard
  ;; toplevel's: evaluating it returns that list as a single value, from
  ;; which the second value is reachable.
  (with-tools
    (tool :tool-repl :id "a" :form "(floor 7 2)")
    (is (equal "1" (result-value (tool :tool-repl :id "a" :form "(second /)")
                                 :value)))))

(test repl-elides-when-one-of-several-values-is-large
  (with-tools
    (let ((result (tool :tool-repl :id "a" :form "(values 1 (make-list 200))")))
      (is (eq t (result-value result :elided)))
      (is (equal "1" (first (result-value result :values)))))))

;;; --- idle reaping (~takeiteasy/miao#104) --------------------------------

(test idle-tool-repl-reaps-an-unused-id
  (with-tools
    (m:unmount *context* :tool-repl)
    (m:mount *context* 'miao:tool-repl :idle 0.3)
    (let ((pid (parse-integer
                (result-value (tool :tool-repl :id "a" :form +getpid-form+) :value))))
      (is (wait-for-exit pid 2.0))
      ;; the id starts empty again, against a freshly started worker
      (is (not (equal (princ-to-string pid)
                       (result-value (tool :tool-repl :id "a" :form +getpid-form+)
                                     :value)))))))

(test idle-tool-repl-keeps-a-session-with-an-eval-still-in-flight
  (with-tools
    (m:unmount *context* :tool-repl)
    (m:mount *context* 'miao:tool-repl :idle 0.3)
    (tool :tool-repl :id "a" :form "(defparameter *x* 1)")
    ;; the sleep runs well past :idle; the idle timer must see the eval as
    ;; in flight and re-arm rather than drop the session out from under it
    (tool :tool-repl :id "a" :form "(sleep 1)" :timeout 5000)
    (is (equal "1" (result-value (tool :tool-repl :id "a" :form "*x*") :value)))))

(test idle-nil-never-reaps
  (with-tools
    (m:unmount *context* :tool-repl)
    (m:mount *context* 'miao:tool-repl :idle nil)
    (let ((pid (parse-integer
                (result-value (tool :tool-repl :id "a" :form +getpid-form+) :value))))
      (sleep 0.5)
      (is (equal (princ-to-string pid)
                 (result-value (tool :tool-repl :id "a" :form +getpid-form+) :value))))))

;;; --- cancelling a call (~takeiteasy/miao#111) --------------------------

(defun cancelled-call (seconds name &rest args)
  "Invoke NAME with ARGS and a cancel token cancelled SECONDS in. Answers the
result and the seconds the call took."
  (let ((token (miao:make-cancel-token))
        (start (get-internal-real-time)))
    (bt:make-thread (lambda () (sleep seconds) (miao:cancel token)))
    (values (apply #'tool name :cancel token args)
            (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

(test a-call-cancelled-before-it-starts-never-runs
  (with-tools
    (let ((token (miao:make-cancel-token))
          (marker (format nil "~a/miao-cancel-marker"
                          (uiop:native-namestring (uiop:temporary-directory)))))
      (ignore-errors (delete-file marker))
      (miao:cancel token)
      (is (eq :cancelled (miao:tool-error
                          (tool :tool-shell :cancel token
                                :cmd (format nil "touch ~a" marker)))))
      (is (not (probe-file marker))))))

(test a-call-queued-behind-a-busy-tool-is-refused-once-cancelled
  (with-tools
    (let* ((token (miao:make-cancel-token))
           (marker (format nil "~a/miao-queued-marker"
                           (uiop:native-namestring (uiop:temporary-directory))))
           (busy (progn (ignore-errors (delete-file marker))
                        (tool-thread (tool :tool-shell :cmd "sleep 1"))))
           (queued (progn (sleep 0.2)
                          (tool-thread (tool :tool-shell :cancel token
                                             :cmd (format nil "touch ~a" marker))))))
      (sleep 0.2)
      (miao:cancel token)
      (bt:join-thread busy)
      (is (eq :cancelled (miao:tool-error (bt:join-thread queued))))
      (is (not (probe-file marker))))))

(test shell-kills-a-cancelled-command-and-its-group
  (with-tools
    (let ((pidfile (format nil "~a/miao-shell-cancel-test.pid"
                           (uiop:native-namestring (uiop:temporary-directory)))))
      (unwind-protect
           (multiple-value-bind (result seconds)
               (cancelled-call 0.5 :tool-shell
                               :cmd (format nil "sleep 30 & echo $! > ~a; wait" pidfile))
             (is (eq :cancelled (miao:tool-error result)))
             (is (< seconds 5))
             (let ((grandchild (with-open-file (s pidfile) (parse-integer (read-line s)))))
               (is (wait-for-exit grandchild))))
        (ignore-errors (delete-file pidfile))))
    (is (equal "after
" (result-value (tool :tool-shell :cmd "echo after") :out)))))

(test http-closes-a-cancelled-exchange
  (with-tools
    (with-fake-http (url)
      (setf *stall* t)
      (unwind-protect
           (multiple-value-bind (result seconds)
               (cancelled-call 0.3 :tool-http :url (format nil "~a/stall" url))
             (is (eq :cancelled (miao:tool-error result)))
             (is (< seconds 5)))
        (setf *stall* nil)))))

(test eval-kills-a-cancelled-worker
  (with-tools
    (multiple-value-bind (result seconds) (cancelled-call 0.5 :tool-eval :form "(loop)")
      (is (eq :cancelled (miao:tool-error result)))
      (is (< seconds 5)))))

(test repl-cancel-kills-the-session-worker
  (with-tools
    (tool :tool-repl :id "c" :form "(defparameter *kept* 1)")
    (multiple-value-bind (result seconds)
        (cancelled-call 0.5 :tool-repl :id "c" :form "(sleep 30)")
      (is (eq :cancelled (miao:tool-error result)))
      (is (< seconds 5)))
    (is (equal "NIL" (result-value (tool :tool-repl :id "c" :form "(boundp '*kept*)")
                                   :value)))))

(test an-interrupt-kills-an-agents-shell-command-and-its-group
  (let* ((pidfile (format nil "~a/miao-agent-interrupt-test.pid"
                          (uiop:native-namestring (uiop:temporary-directory))))
         (n 0)
         (call (format nil "{\"cmd\":\"sleep 30 & echo $! > ~a; wait\"}" pidfile)))
    (ignore-errors (delete-file pidfile))
    (unwind-protect
         (multiple-value-bind (result seconds)
             (interrupt-tool-phase (lambda (&rest request)
                                     (declare (ignore request))
                                     (if (= (incf n) 1)
                                         (tool-call-reply "c1" "tool-shell" call)
                                         (final-reply "done")))
                                   '(miao:tool-shell)
                                   '(:tools (:tool-shell))
                                   :ready (lambda (snapshot)
                                            (and (in-tool-phase-p snapshot)
                                                 (probe-file pidfile))))
           (is (eq :stop (getf (second result) :stop-reason)))
           (is (< seconds 2))
           (let ((grandchild (with-open-file (s pidfile) (parse-integer (read-line s)))))
             (is (wait-for-exit grandchild))))
      (ignore-errors (delete-file pidfile)))))
