(in-package #:miao/tests)
(in-suite :miao)

(test errno-keywords-name-the-failures-the-walk-handles
  (is (eq :enoent (nth-value 1 (miao::fs-open-root "/no/such/dir"))))
  (with-tools
    (let ((fd (miao::fs-open-root *sandbox*)))
      (unwind-protect
           (is (eq :enoent (nth-value 1 (miao::fs-open-dir fd "no-such-dir"))))
        (miao::fs-close fd)))))

(test openat-passes-its-mode-through-the-variadic-convention
  (with-tools
    (let ((old (miao::posix-umask 0)))
      (unwind-protect
           (let ((fd (miao::fs-open-root *sandbox*)))
             (unwind-protect
                  (miao::fs-close (miao::fs-open-leaf fd "mode.txt" '(:wronly :creat) #o640))
               (miao::fs-close fd)))
        (miao::posix-umask old)))
    (is (= #o640 (miao::file-mode (concatenate 'string *sandbox* "/mode.txt"))))))

(test directory-entries-are-read-by-name
  (with-tools
    (write-file (concatenate 'string *sandbox* "/héllo.txt") "x")
    (let ((fd (miao::fs-open-root *sandbox*)))
      (unwind-protect
           (is (member "héllo.txt" (miao::fs-list-names fd nil) :test #'string=))
        (miao::fs-close fd)))))

(test the-messages-digest-hashes-utf-8
  (is (string= "2b2faee6c0473e3a8d938b0fd8621e75"
               (miao::messages-digest (list (list :role "user" :content "héllo ✓"))))))
