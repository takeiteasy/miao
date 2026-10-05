(in-package #:miao)

;;; libc through CFFI. Constants are per OS.

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
;;; convention.

(defun %open (path flags mode)
  (cffi:foreign-funcall-varargs "open" (:string path :int flags) :unsigned-int mode :int))

(defun %openat (dirfd name flags mode)
  (cffi:foreign-funcall-varargs "openat" (:int dirfd :string name :int flags)
                               :unsigned-int mode :int))

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
(cffi:defcfun ("umask" posix-umask) :unsigned-int (mask :unsigned-int))
(cffi:defcfun ("chmod" posix-chmod) :int (path :string) (mode :unsigned-int))

(defun %fcntl-setfd (fd flags)
  (cffi:foreign-funcall-varargs "fcntl" (:int fd :int +f-setfd+) :int flags :int))

(defun dirent-name (entry)
  "The name in a struct dirent ENTRY."
  (cffi:foreign-string-to-lisp (cffi:inc-pointer entry #+darwin 21 #+linux 19)))

(defun posix-kill-alive-p (pid)
  "True when signal 0 to PID succeeds, or is refused only for permission."
  (or (zerop (%kill pid 0))
      (= (%errno) +eperm+)))

(defun file-mode (path)
  "PATH's permission bits, read through stat(1) because struct stat differs by OS."
  (parse-integer (uiop:run-program (list "stat" #+darwin "-f%Lp" #+linux "-c%a"
                                         (namestring path))
                                   :output '(:string :stripped t))
                 :radix 8))
