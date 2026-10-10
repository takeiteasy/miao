(in-package #:miao)

(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

;;; Primitives for tool-fs's atomic sandbox walk. Each path component is opened with O_NOFOLLOW relative to the
;;; directory fd held for its parent -- refusing a symlink outright rather
;;; than resolving it -- and the final component is operated on relative to
;;; the last fd, so the check that it is not a symlink and the operation share
;;; one file descriptor and cannot be swapped apart. The process's current
;;; directory is never touched.
;;;
;;; sb-posix carries none of the *at calls, so they are bound here.

;;; --- errno, normalised to keywords -----------------------------------

(defun errno-keyword (errno)
  (cond ((= errno sb-posix:enoent) :enoent)
        ((= errno sb-posix:eexist) :eexist)
        ((= errno sb-posix:enotdir) :enotdir)
        ((= errno sb-posix:eisdir) :eisdir)
        ((= errno sb-posix:eperm) :eperm)
        ((= errno sb-posix:enotempty) :enotempty)
        ((= errno sb-posix:eloop) :eloop)
        (t :other)))

(defun sb-posix-errno-keyword (condition)
  (errno-keyword (sb-posix:syscall-errno condition)))

(defun syscall-result (value failed)
  "VALUE, or (values nil errno-keyword) when FAILED. Reads errno at once, so
nothing runs between the failing call and here."
  (if failed
      (values nil (errno-keyword (sb-alien:get-errno)))
      (values value nil)))

;;; --- the *at calls ----------------------------------------------------

;;; openat(2) is variadic: on arm64 macOS a variadic argument travels on the
;;; stack, so binding MODE as an ordinary fixed argument hands the kernel
;;; garbage. Declaring it after &OPTIONAL makes SBCL use the variadic
;;; convention, as sb-posix's own OPEN does.
(defun %openat (dirfd name flags mode)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "openat" (function sb-alien:int sb-alien:int sb-alien:c-string
                                             sb-alien:int &optional sb-alien:unsigned-int))
   dirfd name flags mode))

(sb-alien:define-alien-routine ("mkdirat" %mkdirat) sb-alien:int
  (dirfd sb-alien:int) (name sb-alien:c-string) (mode sb-alien:unsigned-int))

(sb-alien:define-alien-routine ("unlinkat" %unlinkat) sb-alien:int
  (dirfd sb-alien:int) (name sb-alien:c-string) (flags sb-alien:int))

;;; Not exported by sb-posix.
(defconstant +at-removedir+ #+darwin #x80 #+linux #x200)

(sb-alien:define-alien-routine ("readlinkat" %readlinkat) sb-alien:long
  (dirfd sb-alien:int) (name sb-alien:c-string)
  (buffer (* sb-alien:char)) (size sb-alien:unsigned-long))

(sb-alien:define-alien-routine ("fdopendir" %fdopendir) sb-alien:system-area-pointer
  (fd sb-alien:int))

;;; --- directory fds ----------------------------------------------------
;;; Each returns an fd, or (values nil errno-kw).

(defun fs-open-root (root)
  (handler-case (values (sb-posix:open root (logior sb-posix:o-directory
                                                     sb-posix:o-nofollow))
                        nil)
    (sb-posix:syscall-error (e) (values nil (sb-posix-errno-keyword e)))))

(defun fs-open-dir (dirfd name)
  "NAME under DIRFD as a directory, refusing a symlink."
  (let ((fd (%openat dirfd name (logior sb-posix:o-directory sb-posix:o-nofollow) 0)))
    (syscall-result fd (minusp fd))))

(defun fs-close (fd)
  (ignore-errors (sb-posix:close fd)))

;;; --- the leaf: open, mkdir, unlink, readlink, list --------------------

(defun fs-open-leaf (dirfd name flags mode)
  "NAME under DIRFD, opened with FLAGS (a list of :RDONLY :WRONLY :CREAT
:TRUNC), always with O_NOFOLLOW added. Returns an fd, or (values nil
errno-keyword)."
  (let ((fd (%openat dirfd name
                     (logior sb-posix:o-nofollow
                             (if (member :wronly flags) sb-posix:o-wronly sb-posix:o-rdonly)
                             (if (member :creat flags) sb-posix:o-creat 0)
                             (if (member :trunc flags) sb-posix:o-trunc 0))
                     mode)))
    (syscall-result fd (minusp fd))))

(defun fs-mkdir-leaf (dirfd name mode)
  "T on success, (values nil :eexist) if it is already there, or another
errno keyword."
  (syscall-result t (minusp (%mkdirat dirfd name mode))))

(defun fs-unlink-leaf (dirfd name)
  (syscall-result t (minusp (%unlinkat dirfd name 0))))

(defun fs-rmdir-leaf (dirfd name)
  "T on success, or (values nil errno-keyword). Fails on a directory that is
not empty, and on anything that is not a directory."
  (syscall-result t (minusp (%unlinkat dirfd name +at-removedir+))))

(defun fs-symlink-leaf-p (dirfd name)
  "True when NAME under DIRFD is itself a symlink, dangling or not.
READLINKAT fails with EINVAL on anything else, which is enough to tell the
two apart without a full STAT."
  (sb-alien:with-alien ((buffer (array sb-alien:char 8)))
    (not (minusp (%readlinkat dirfd name (sb-alien:cast buffer (* sb-alien:char)) 8)))))

(defun fs-list-names (dirfd name &key hide-links)
  "Every entry of NAME under DIRFD (DIRFD itself when NAME is NIL), sorted.
Returns the list, or (values nil errno-keyword) if the directory cannot be
opened. HIDE-LINKS leaves symlinks out. Entries are read from the fd the walk
already validated, so the listing cannot be redirected by a swapped path."
  (multiple-value-bind (fd errno) (fs-open-dir dirfd (or name "."))
    (unless fd (return-from fs-list-names (values nil errno)))
    (let ((sap (%fdopendir fd)))
      (when (zerop (sb-sys:sap-int sap))
        (fs-close fd)
        (return-from fs-list-names (values nil :other)))
      ;; CLOSEDIR now owns FD.
      (let ((dir (sb-alien:sap-alien sap (* t)))
            (names '()))
        (unwind-protect
             (loop for entry = (sb-posix:readdir dir)
                   until (sb-alien:null-alien entry)
                   do (let ((entry-name (sb-posix:dirent-name entry)))
                        (unless (or (string= entry-name ".") (string= entry-name "..")
                                    (and hide-links (fs-symlink-leaf-p fd entry-name)))
                          (push entry-name names))))
          (sb-posix:closedir dir))
        (values (sort names #'string<) nil)))))

;;; --- the walk ----------------------------------------------------------

(defun fs-walk (root components &key create)
  "Open ROOT, then each of COMPONENTS in turn relative to the one before --
each with O_NOFOLLOW, so a symlink anywhere along the way is refused rather
than followed. Returns an fd for the last directory, which the caller closes,
or (values nil errno-keyword) at the component that failed.

CREATE makes a missing component with MKDIRAT first and retries the open --
still through O_NOFOLLOW, so a symlink swapped in between the two is
refused exactly as an existing one would be, rather than trusted because
this walk just created it."
  (multiple-value-bind (fd errno) (fs-open-root root)
    (unless fd (return-from fs-walk (values nil errno)))
    (dolist (component components fd)
      (multiple-value-bind (next errno) (fs-open-dir fd component)
        (when (and (not next) create (eq errno :enoent))
          (fs-mkdir-leaf fd component #o755)
          (setf (values next errno) (fs-open-dir fd component)))
        (fs-close fd)
        (unless next (return-from fs-walk (values nil errno)))
        (setf fd next)))))
