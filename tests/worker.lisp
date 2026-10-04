(in-package #:miao/tests)
(in-suite :miao)

;;; The worker protocol on its own, below the tools that use it: the
;;; handshake, one exchange, and what a lapsed deadline leaves behind.

(defmacro with-worker ((worker) &body body)
  `(let ((,worker (miao::start-worker)))
     (unwind-protect (progn (is (not (null ,worker))) ,@body)
       (miao::kill-worker ,worker))))

(test worker-round-trips-a-form
  (with-worker (w)
    (let ((result (miao::worker-eval w "(list 1 2)" 5000)))
      (is (equal "(1 2)" (getf (second result) :value))))))

(test worker-is-unavailable-when-it-cannot-start
  (let ((miao:*worker-command* (list "/nonexistent/lisp" "--eval")))
    (is (null (miao::start-worker)))))

(test worker-dies-with-its-deadline
  (with-worker (w)
    (is (eq :timeout (miao:tool-error (miao::worker-eval w "(loop)" 500))))
    (is (not (miao::worker-alive-p w)))
    ;; A dead worker answers, rather than blocking a caller that reuses it.
    (is (eq :unavailable (miao:tool-error (miao::worker-eval w "1" 500))))))

(test stale-worker-reads-dead-and-is-never-signalled
  (let* ((worker (miao::start-worker))
         (boot miao::*boot*))
    (unwind-protect
         (progn
           (setf miao::*boot* (list :later-boot))
           (is (miao::worker-stale-p worker))
           (is (not (miao::worker-alive-p worker)))
           (miao::kill-worker worker)
           (is (uiop:process-alive-p (miao::worker-process worker))))
      (setf miao::*boot* boot)
      (miao::terminate-process-group (miao::worker-process worker)))))

(test kill-live-workers-kills-each-registered-worker
  (let ((miao::*live-workers* '())
        (miao::*live-workers-lock* (bt:make-lock)))
    (let ((workers (list (miao::start-worker) (miao::start-worker))))
      (is (= 2 (length miao::*live-workers*)))
      (miao::kill-live-workers)
      (is (null miao::*live-workers*))
      (is (notany #'miao::worker-alive-p workers)))))

(test a-worker-is-killed-once
  ;; A second kill must not signal a pid the first has already reaped.
  (let ((worker (miao::start-worker))
        (kills 0))
    (let ((original (fdefinition 'miao::terminate-process-group)))
      (setf (fdefinition 'miao::terminate-process-group)
            (lambda (process)
              (incf kills)
              (funcall original process)))
      (unwind-protect
           (progn (miao::kill-worker worker)
                  (miao::kill-worker worker)
                  (is (eql 1 kills)))
        (setf (fdefinition 'miao::terminate-process-group) original)))))

;;; --- elision (~takeiteasy/miao#26) --------------------------------------

(test a-small-value-is-not-elided
  (with-worker (w)
    (let ((result (miao::worker-eval w "(+ 1 2)" 5000)))
      (is (equal "3" (getf (second result) :value)))
      (is (null (getf (second result) :elided))))))

(test a-value-past-print-length-is-elided
  (with-worker (w)
    (is (eq t (getf (second (miao::worker-eval w "(make-list 200)" 5000)) :elided)))))

(test a-value-past-print-level-is-elided
  (with-worker (w)
    (is (eq t (getf (second (miao::worker-eval w "(list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7 (list 8 (list 9)))))))))" 5000)) :elided)))))

(test a-value-past-the-character-cap-is-elided
  (with-worker (w)
    (is (eq t (getf (second (miao::worker-eval w "(make-string 5000 :initial-element #\\a)" 5000)) :elided)))))

(test printed-text-containing-dots-is-not-elided
  (with-worker (w)
    (let ((result (miao::worker-eval w "(princ \"...\")" 5000)))
      (is (equal "..." (getf (second result) :out)))
      (is (null (getf (second result) :elided))))))

;;; --- multiple values (~takeiteasy/miao#105) -----------------------------

(test worker-keeps-every-value-a-form-returns
  (with-worker (w)
    (let ((result (miao::worker-eval w "(values 1 2 3)" 5000)))
      (is (equal "1" (getf (second result) :value)))
      (is (equal '("1" "2" "3") (getf (second result) :values))))))

(test worker-reports-no-values-as-nil
  (with-worker (w)
    (let ((result (miao::worker-eval w "(values)" 5000)))
      (is (equal "NIL" (getf (second result) :value)))
      (is (null (getf (second result) :values))))))

(test worker-elides-past-100-values
  (if (< multiple-values-limit 150)
      (skip "needs MULTIPLE-VALUES-LIMIT of 150 or more")
      (with-worker (w)
        (let ((result (miao::worker-eval
                       w "(apply #'values (loop for i below 150 collect i))" 5000)))
          (is (= 100 (length (getf (second result) :values))))
          (is (eq t (getf (second result) :elided)))))))
