;;; The worker's read/eval/print loop. This file is not part of the miao
;;; system: it is read as text at compile time and handed to a bare child
;;; Lisp on its command line, so it must stay a single form that loads
;;; nothing. See worker.lisp for the protocol.

(let ((p (or (find-package "MIAO-WORKER")
             (make-package "MIAO-WORKER" :use '("CL")))))
  ;; Where the gate's fresh symbols live (gate.lisp): it uses nothing, so a
  ;; name a form invents can never be a name the worker already has.
  (unless (find-package "MIAO-GATE")
    (make-package "MIAO-GATE" :use '()))
  (labels ((render-under (v length level)
             (let ((*print-length* length) (*print-level* level)
                   (*print-readably* nil) (*print-circle* t))
               (prin1-to-string v)))
           (render (v)
             ;; Capped, so a large or circular value cannot flood the pipe.
             ;; Elided when the cap cuts the string outright, or when
             ;; printing one step wider would print more of it: that second
             ;; pass only runs when the first output looks cut ("..." from
             ;; *PRINT-LENGTH*, "#" from *PRINT-LEVEL*), so a value that
             ;; fits under both limits prints once.
             (let* ((s (render-under v 100 8))
                    (capped (> (length s) 4000)))
               (values (if capped (concatenate 'string (subseq s 0 4000) " ...") s)
                       (or capped
                           (and (or (find #\# s) (search "..." s))
                                (string/= s (render-under v 101 9)))))))
           (render-values (values)
             ;; Each of VALUES renders under RENDER's own cap; a form
             ;; returning more than 100 values, or whose combined printed
             ;; form runs past 4000 characters, has its remainder dropped
             ;; and ELIDED set -- the same guard against flooding the pipe
             ;; RENDER applies to one value.
             (let* ((many (> (length values) 100))
                    (values (if many (subseq values 0 100) values))
                    (elided many) (total 0) (rendered '()))
               (dolist (v values)
                 (if (> total 4000)
                     (setf elided t)
                     (multiple-value-bind (s e) (render v)
                       (push s rendered)
                       (incf total (length s))
                       (when e (setf elided t)))))
               (values (nreverse rendered) elided)))
           (cap-output (s)
             ;; The same cap RENDER puts on a value, so a form that prints
             ;; without end cannot flood the pipe.
             (if (> (length s) 4000)
                 (values (concatenate 'string (subseq s 0 4000) " ...") t)
                 (values s nil)))
           (say (form)
             (prin1 form)
             (terpri)
             (finish-output)))
    (let ((*package* p) (*read-eval* nil))
      (say '(:ready))
      (loop
        (let ((message (handler-case (read *standard-input* nil :eof)
                         (error () :eof))))
          (unless (and (consp message) (eq (first message) :eval))
            (return))
          (let ((out (make-string-output-stream)))
            (say (handler-case
                     (let ((form (read-from-string (second message))))
                       (handler-case
                           (let ((values (let* ((*standard-output* out)
                                                (*error-output* out)
                                                ;; The pipe to the host is
                                                ;; this process's real
                                                ;; stdin and stdout, which
                                                ;; T and the *-IO* streams
                                                ;; reach; a form that wrote
                                                ;; there could forge a
                                                ;; reply.
                                                (*terminal-io*
                                                  (make-two-way-stream
                                                   (make-concatenated-stream) out))
                                                (*query-io* *terminal-io*)
                                                (*debug-io* *terminal-io*)
                                                (*trace-output* out)
                                                (*standard-input* (make-concatenated-stream)))
                                          (multiple-value-list (eval form)))))
                             ;; The REPL's own history, so a value the
                             ;; reply elided can still be inspected: *,
                             ;; **, *** and /, //, /// shift the same way
                             ;; the standard toplevel's do, the latter over
                             ;; every value a form returned. An erroring
                             ;; form below leaves them alone.
                             (setf *** ** ** * * (first values))
                             (setf /// // // / / values)
                             (multiple-value-bind (rendered elided) (render-values values)
                               (multiple-value-bind (printed cut) (cap-output (get-output-stream-string out))
                                 (list :ok rendered printed
                                       (if (or elided cut) :elided nil)))))
                         ;; SERIOUS-CONDITION, not ERROR: exhausting the
                         ;; stack or the heap is one, and would otherwise
                         ;; end the worker.
                         (serious-condition (e)
                           (list :error (princ-to-string e)
                                 (cap-output (get-output-stream-string out))))))
                   (error (e) (list :reader-error (princ-to-string e)))))))))))
