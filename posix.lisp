(in-package #:miao)

;;; libc through CFFI, and the few implementation hooks no library covers.
;;; Constants are per OS; ECL and SBCL share everything else.

;; TODO: constants and the dirent layout cover darwin-arm64 and linux only;
;; x86_64 macOS needs readdir$INODE64 and its own d_name offset (#228).
#-(or darwin linux) (error "posix.lisp needs darwin or linux.")

;;; --- errno ---------------------------------------------------------------

(defconstant +eperm+ 1)
(defconstant +enoent+ 2)
(defconstant +eintr+ 4)
(defconstant +eexist+ 17)
(defconstant +enotdir+ 20)
(defconstant +eisdir+ 21)
(defconstant +enotempty+ #+darwin 66 #+linux 39)
(defconstant +eloop+ #+darwin 62 #+linux 40)

(defun %errno ()
  "The calling thread's errno. Read straight after the failing call."
  (cffi:mem-ref (cffi:foreign-funcall #+darwin "__error" #+linux "__errno_location" :pointer)
                :int))

;;; --- open flags and friends ---------------------------------------------

(defconstant +o-rdonly+ 0)
(defconstant +o-wronly+ 1)
(defconstant +o-rdwr+ 2)
(defconstant +o-creat+ #+darwin #x200 #+linux #x40)
(defconstant +o-trunc+ #+darwin #x400 #+linux #x200)
(defconstant +o-nofollow+ #+darwin #x100
  #+(and linux (or arm64 aarch64)) #x8000
  #+(and linux (not (or arm64 aarch64))) #x20000)
(defconstant +o-directory+ #+darwin #x100000
  #+(and linux (or arm64 aarch64)) #x4000
  #+(and linux (not (or arm64 aarch64))) #x10000)
(defconstant +at-removedir+ #+darwin #x80 #+linux #x200)
(defconstant +f-setfd+ 2)
(defconstant +fd-cloexec+ 1)
(defconstant +lock-ex+ 2)

;;; --- calls -----------------------------------------------------------------

;;; open(2), openat(2) and fcntl(2) are variadic: on arm64 macOS a variadic
;;; argument travels on the stack, so it must be passed with the variadic
;;; convention. CFFI does that on SBCL; ECL's dynamic calls do not, so ECL
;;; compiles real C calls instead.
#+ecl (ffi:clines "#include <fcntl.h>")

(defun %open (path flags mode)
  #+sbcl (cffi:foreign-funcall-varargs "open" (:string path :int flags) :unsigned-int mode :int)
  #+ecl (ffi:c-inline (path flags mode) (:cstring :int :unsigned-int) :int
                      "open(#0, #1, #2)" :one-liner t))

(defun %openat (dirfd name flags mode)
  #+sbcl (cffi:foreign-funcall-varargs "openat" (:int dirfd :string name :int flags)
                                       :unsigned-int mode :int)
  #+ecl (ffi:c-inline (dirfd name flags mode) (:int :cstring :int :unsigned-int) :int
                      "openat(#0, #1, #2, #3)" :one-liner t))

(cffi:defcfun ("close" %close) :int (fd :int))
(cffi:defcfun ("mkdirat" %mkdirat) :int (dirfd :int) (name :string) (mode :unsigned-int))
(cffi:defcfun ("unlinkat" %unlinkat) :int (dirfd :int) (name :string) (flags :int))
(cffi:defcfun ("readlinkat" %readlinkat) :long
  (dirfd :int) (name :string) (buffer :pointer) (size :unsigned-long))
(cffi:defcfun ("fdopendir" %fdopendir) :pointer (fd :int))
(cffi:defcfun ("readdir" %readdir) :pointer (dir :pointer))
(cffi:defcfun ("closedir" %closedir) :int (dir :pointer))
(cffi:defcfun ("flock" %flock) :int (fd :int) (operation :int))
(cffi:defcfun ("getpid" posix-getpid) :int)
(cffi:defcfun ("getppid" posix-getppid) :int)
(cffi:defcfun ("kill" %kill) :int (pid :int) (signal :int))
(cffi:defcfun ("read" %read) :long (fd :int) (buffer :pointer) (count :unsigned-long))
(cffi:defcfun ("write" %write) :long (fd :int) (buffer :pointer) (count :unsigned-long))
(cffi:defcfun ("umask" posix-umask) :unsigned-int (mask :unsigned-int))
(cffi:defcfun ("chmod" posix-chmod) :int (path :string) (mode :unsigned-int))

(defun %fcntl-setfd (fd flags)
  #+sbcl (cffi:foreign-funcall-varargs "fcntl" (:int fd :int +f-setfd+) :int flags :int)
  #+ecl (ffi:c-inline (fd flags) (:int :int) :int "fcntl(#0, F_SETFD, #1)" :one-liner t))

(defun dirent-name (entry)
  "The name in a struct dirent ENTRY."
  (cffi:foreign-string-to-lisp (cffi:inc-pointer entry #+darwin 21 #+linux 19)))

(defun posix-kill-alive-p (pid)
  "True when signal 0 to PID succeeds, or is refused only for permission."
  (or (zerop (%kill pid 0))
      (= (%errno) +eperm+)))

(defun unix-time-micros ()
  "(values seconds microseconds) since the Unix epoch."
  (cffi:with-foreign-object (tv :int64 2)
    (cffi:foreign-funcall "gettimeofday" :pointer tv :pointer (cffi:null-pointer) :int)
    (values (cffi:mem-aref tv :int64 0)
            #+darwin (cffi:mem-ref tv :int32 8)
            #+linux (cffi:mem-aref tv :int64 1))))

(defun file-mode (path)
  "PATH's permission bits, read through stat(1) because struct stat differs by OS."
  (parse-integer (uiop:run-program (list "stat" #+darwin "-f%Lp" #+linux "-c%a"
                                         (namestring path))
                                   :output '(:string :stripped t))
                 :radix 8))

;;; --- implementation hooks --------------------------------------------------

(defun spawn-thread (function &key name)
  "A thread running FUNCTION whose unhandled error is reported on *ERROR-OUTPUT*
and ends only that thread. ECL otherwise opens a REPL in the thread."
  (bt:make-thread (lambda ()
                    (handler-case (funcall function)
                      (error (condition)
                        (format *error-output* "~&miao: thread ~a: ~a~%" name condition)
                        nil)))
                  :name name))

(defmacro without-interrupts (&body body)
  `(#+sbcl sb-sys:without-interrupts #+ecl mp:without-interrupts ,@body))

(deftype interactive-interrupt ()
  "The condition a terminal Ctrl-C signals."
  #+sbcl 'sb-sys:interactive-interrupt
  #+ecl 'ext:interactive-interrupt)

(defun add-exit-hook (function)
  #+sbcl (pushnew function sb-ext:*exit-hooks*)
  #+ecl (pushnew function si:*exit-hooks*))

(defun lisp-runtime-path ()
  "The executable running this image, as a string."
  #+sbcl (namestring sb-ext:*runtime-pathname*)
  #+ecl (let ((name (si:argv 0)))
          (if (find #\/ name)
              name
              (or (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
                        for candidate = (and (plusp (length dir))
                                             (probe-file (merge-pathnames name (uiop:ensure-directory-pathname dir))))
                        when candidate return (namestring candidate))
                  name))))
