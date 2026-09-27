(in-package #:miao)

;;; Evaluate one form in a worker that is started for it and killed after
;;; it. No state survives a call.
;;;
;;; Trust posture: arbitrary evaluation. Trusted operator only. A model
;;; reaches evaluation through tool-gated-eval, which checks a form against
;;; an allowlist (~takeiteasy/miao#44); no gated form of this tool exists.

(define-tool :tool-eval
    (:trust :operator
     :summary "Evaluate a Lisp form in a single-use worker process"
     :params ((:form string :required t :doc "source text of one form")
              (:timeout (integer 1) :default +default-tool-timeout+
               :doc "kill the worker after this many milliseconds")))
  (:invoke (form timeout)
    (let ((worker (start-worker)))
      (if (null worker)
          (fail :unavailable)
          (unwind-protect
               (worker-eval worker form timeout cancel-token)
            (kill-worker worker))))))
