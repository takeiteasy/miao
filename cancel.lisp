(in-package #:miao)

;;; A caller-held handle rather than a message: it reaches a turn through
;;; every layer without the caller knowing which service holds it, and
;;; without a service to address it to.

(defstruct (cancel-token (:constructor make-cancel-token ()))
  (lock (bt:make-lock)) cancelled actions
  ;; :QUEUED until a tool's handler starts the call, :RUNNING while it runs,
  ;; then :SETTLED, which SETTLED signals.
  (phase :queued) (settled (bt:make-semaphore)))

(defun cancelled-p (token)
  (bt:with-lock-held ((cancel-token-lock token))
    (cancel-token-cancelled token)))

(defun on-cancel (token function)
  "Call FUNCTION when TOKEN is cancelled, at once if it already is."
  (when (bt:with-lock-held ((cancel-token-lock token))
          (or (cancel-token-cancelled token)
              (progn (push function (cancel-token-actions token)) nil)))
    (funcall function)))

(defun cancel (token)
  "Cancel whatever TOKEN was passed to as :CANCEL. Idempotent; true the first
time."
  (let* ((first nil)
         (actions (bt:with-lock-held ((cancel-token-lock token))
                    (unless (cancel-token-cancelled token)
                      (setf first t
                            (cancel-token-cancelled token) t)
                      (shiftf (cancel-token-actions token) nil)))))
    (mapc #'funcall actions)
    first))

(defun stuck-p (token grace)
  "True when the call TOKEN was passed to is running and, GRACE seconds on,
has still not returned. A call that has not started is not stuck: it sees
TOKEN cancelled and answers without running."
  (and (eq (cancel-token-phase token) :running)
       (not (bt:wait-on-semaphore (cancel-token-settled token) :timeout grace))))
