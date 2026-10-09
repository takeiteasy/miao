(in-package #:miao/tests)

(def-suite :miao)
(in-suite :miao)

(defun eventually (function &optional (timeout 2))
  "Poll FUNCTION until it returns true or TIMEOUT seconds pass."
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        for value = (funcall function)
        until (or value (> (get-internal-real-time) deadline))
        do (sleep 0.01)
        finally (return value)))

(defun mount-backend (context name &rest initargs)
  "Mount the client backend NAME under CONTEXT as a service of its own name."
  (apply #'m:mount context 'miao:backend-service :name name initargs))

(defun call-with-pool-sizes (sizes body)
  "Run BODY with each tier in SIZES, a plist, capped at its size, starting
from no idle threads."
  (let ((old (loop for (tier) on sizes by #'cddr
                   collect tier collect (miao::pool-max-threads (miao::pool-for tier)))))
    (miao::retire-idle-workers)
    (unwind-protect
         (progn
           (loop for (tier size) on sizes by #'cddr
                 do (setf (miao::pool-max-threads (miao::pool-for tier)) size))
           (funcall body))
      (loop for (tier size) on old by #'cddr
            do (setf (miao::pool-max-threads (miao::pool-for tier)) size))
      (miao::retire-idle-workers))))

(defmacro with-pool-sizes ((&rest sizes) &body body)
  `(call-with-pool-sizes (list ,@sizes) (lambda () ,@body)))

(defun sinks-idle-p ()
  "True when no emitter is draining or waiting to."
  (let ((stats (miao:pool-stats :sink)))
    (and (zerop (getf stats :running)) (zerop (getf stats :queued)))))
