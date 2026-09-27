(in-package #:miao)

;;; Evaluate one form that passed the allowlist gate (gate.lisp), in a
;;; single-use worker with a capped heap. What is evaluated is the gate's
;;; canonical text, never the caller's own.
;;;
;;; The allowlist bounds what a form can reach, not what it can spend: the
;;; deadline bounds time and :HEAP bounds memory.

(define-tool :tool-gated-eval
    (:trust :agent
     :summary "Evaluate a Lisp form checked against an allowlist, in a single-use worker"
     :slots ((heap :initarg :heap :initform 256 :reader gated-eval-heap
                   :documentation "Megabytes of heap a worker gets, or nil
for the host's own default."))
     :params ((:form string :required t :doc "source text of one form")
              (:timeout (integer 1) :default +default-tool-timeout+
               :doc "kill the worker after this many milliseconds")))
  (:invoke (form timeout)
    (multiple-value-bind (source reason) (gate-check form)
      (if reason
          (bad-request "~a" reason)
          (let ((worker (start-worker :heap (gated-eval-heap service))))
            (if (null worker)
                (fail :unavailable)
                (unwind-protect
                     (worker-eval worker source timeout cancel-token)
                  (kill-worker worker))))))))
