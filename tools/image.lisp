(in-package #:miao)

;; SB-INTROSPECT backs the lambda lists and source locations below; it
;; ships with SBCL itself, so REQUIRE rather than a Quicklisp dependency,
;; ahead of the DEFUNs that call into it.
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-introspect))

;;; Read-only introspection over the live CL image: describe, apropos,
;;; documentation and source locations..
;;;
;;; Never a value, never a slot. A provider's API key lives in a slot
;;; (provider.lisp), and PROVIDER.LISP:93 keeps it out of published
;;; metadata for exactly that reason -- this tool must hold the same line,
;;; so every op below reports flags and shapes, never SYMBOL-VALUE. Seeing
;;; a value stays TOOL-EVAL, TOOL-REPL or TOOL-SELF's job, at :operator.
;;; The one content it returns is a function's own code (:form), so a
;;; literal written into that code travels with it.
;;;
;;; Symbols are looked up with FIND-SYMBOL, never READ-FROM-STRING or
;;; INTERN: a lookup must not be able to grow the image.

(define-tool :tool-image
    (:trust :agent
     :resumable t
     :summary "Read-only introspection over the live Lisp image: describe, apropos, documentation, source"
     :params ((:op (member :describe :apropos :documentation :source :packages)
               :required t :doc "operation to perform")
              (:symbol string :required-when (:op (:describe :documentation :source))
               :doc "symbol name, e.g. \"miao:complete\" or \"complete\"")
              (:package string :doc "package to resolve :symbol or :pattern against")
              (:pattern string :required-when (:op :apropos) :doc "substring to search for")
              (:external-only boolean :default t
               :doc "restrict :apropos to exported symbols")
              (:limit (integer 1 1000) :default 100 :doc ":apropos result cap")
              (:doc-type (member :function :variable :type :structure :setf :compiler-macro)
               :default :function :doc "documentation kind, for :documentation and :source")))
  (:invoke (op symbol package pattern external-only limit doc-type)
    (case op
      (:describe (op-describe symbol package))
      (:apropos (op-apropos pattern package external-only limit))
      (:documentation (op-documentation symbol package doc-type))
      (:source (op-source symbol package doc-type))
      (:packages (op-packages)))))

;;; --- symbol resolution -------------------------------------------------

(defun split-symbol-text (text)
  "TEXT split on its first \":\" or \"::\" into (package-name . name), or
(nil . TEXT) if it names none."
  (let ((pos (position #\: text)))
    (if pos
        (let ((end (if (and (< (1+ pos) (length text)) (char= (char text (1+ pos)) #\:))
                       (+ pos 2)
                       (1+ pos))))
          (cons (subseq text 0 pos) (subseq text end)))
        (cons nil text))))

(defun resolve-symbol (text package-text)
  "TEXT resolved against PACKAGE-TEXT or a prefix within TEXT itself, as
(values symbol status): status is :internal, :external or :inherited on a
hit, or :not-found / :no-package. FIND-SYMBOL only."
  (destructuring-bind (prefix . name) (split-symbol-text text)
    (let* ((package-name (or prefix package-text))
           (package (if package-name (find-package (string-upcase package-name)) *package*)))
      (if (null package)
          (values nil :no-package)
          (multiple-value-bind (symbol found) (find-symbol (string-upcase name) package)
            (if found (values symbol found) (values nil :not-found)))))))

(defmacro with-resolved-symbol ((symbol-var symbol-text package-text) &body body)
  "Resolve SYMBOL-TEXT and run BODY with SYMBOL-VAR bound, or
answer the (:bad-request ...) naming what went wrong."
  (a:with-gensyms (status)
    `(multiple-value-bind (,symbol-var ,status) (resolve-symbol ,symbol-text ,package-text)
       (case ,status
         (:no-package (bad-request "no package named ~a" (or ,package-text ,symbol-text)))
         (:not-found (bad-request "no symbol named ~a" ,symbol-text))
         (t (progn ,@body))))))

;;; --- :describe -----------------------------------------------------

(defun op-describe (symbol-text package-text)
  (with-resolved-symbol (symbol symbol-text package-text)
    (ok :name (symbol-name symbol)
        :package (and (symbol-package symbol) (package-name (symbol-package symbol)))
        :fboundp (and (fboundp symbol) t)
        :boundp (and (boundp symbol) t)
        :kind (symbol-kind symbol)
        :lambda-list (symbol-lambda-list symbol)
        :documentation (documentation symbol 'function)
        :variable-documentation (documentation symbol 'variable)
        :source (source-report symbol))))

(defun symbol-kind (symbol)
  "SYMBOL's role, best guess: a symbol can be several of these at once --
FBOUNDP and BOUNDP both, say -- so this names the most specific one."
  (cond
    ((special-operator-p symbol) :special-operator)
    ((macro-function symbol) :macro)
    ((and (fboundp symbol) (typep (symbol-function symbol) 'generic-function)) :generic-function)
    ((fboundp symbol) :function)
    ((find-class symbol nil) :class)
    ((and (boundp symbol) (constantp symbol)) :constant)
    ((boundp symbol) :variable)
    (t :unbound)))

(defun symbol-lambda-list (symbol)
  "SYMBOL's lambda list, printed as text, or NIL when it is not fbound or
the implementation cannot say."
  (and (fboundp symbol)
       (ignore-errors
        (let ((list (function-lambda-list symbol)))
          (and list (prin1-to-string list))))))

(defun function-lambda-list (symbol)
  (sb-introspect:function-lambda-list symbol))

;;; --- :apropos --------------------------------------------------------

(defun op-apropos (pattern-text package-text external-only limit)
  (let ((package (and package-text (find-package (string-upcase package-text)))))
    (if (and package-text (null package))
        (bad-request "no package named ~a" package-text)
        (let* ((matches (matching-symbols pattern-text package external-only))
               (total (length matches)))
          (ok :symbols (mapcar #'qualified-name (subseq matches 0 (min limit total)))
              :total total
              :truncated (> total limit))))))

(defun matching-symbols (pattern package external-only)
  (let ((symbols (remove-duplicates (apropos-list pattern package) :test #'eq)))
    (sort (if external-only (remove-if-not #'exported-symbol-p symbols) symbols)
          #'string< :key #'symbol-name)))

(defun exported-symbol-p (symbol)
  (and (symbol-package symbol)
       (eq :external (nth-value 1 (find-symbol (symbol-name symbol) (symbol-package symbol))))))

(defun qualified-name (symbol)
  (if (symbol-package symbol)
      (format nil "~(~a~):~(~a~)" (package-name (symbol-package symbol)) (symbol-name symbol))
      (format nil "#:~(~a~)" (symbol-name symbol))))

;;; --- :documentation and :source -----------------------------------------

(defun doc-type-symbol (doc-type)
  (ecase doc-type
    (:function 'function) (:variable 'variable) (:type 'type)
    (:structure 'structure) (:setf 'setf) (:compiler-macro 'compiler-macro)))

(defun op-documentation (symbol-text package-text doc-type)
  (with-resolved-symbol (symbol symbol-text package-text)
    (ok :documentation (documentation symbol (doc-type-symbol doc-type)))))

(defun op-source (symbol-text package-text doc-type)
  (declare (ignore doc-type)) ;; only function source locations are offered; see docs/introspection.md
  (with-resolved-symbol (symbol symbol-text package-text)
    (apply #'ok (source-report symbol))))

(defun source-report (symbol)
  "SYMBOL's :available, its source location or form, and for a generic
function its :methods."
  (let ((source (symbol-source symbol))
        (methods (symbol-methods symbol)))
    (append (list :available (and (or source methods) t))
            source
            (and methods (list :methods (mapcar #'method-report (subseq methods 0 (min (length methods) +method-limit+)))
                               :methods-total (length methods)
                               :methods-truncated (> (length methods) +method-limit+))))))

(defconstant +source-form-limit+ 4000
  "Characters of a definition's printed form :source returns.")

(defun symbol-source (symbol)
  "SYMBOL's (:file ... :position ...) or, for a definition made in the image,
its (:form ... :truncated ...); NIL when it is not fbound or has neither."
  (and (fboundp symbol)
       (or (function-source symbol) (function-form symbol))))

(defconstant +method-limit+ 100
  "Methods of a generic function :source lists.")

(defun print-standard (object)
  "OBJECT printed on one line, unaffected by the caller's *PRINT-* bindings,
or NIL if it cannot be printed."
  (ignore-errors
   (with-standard-io-syntax
     (let ((*print-readably* nil) (*print-pretty* nil)
           (*package* (find-package '#:miao)))
       (prin1-to-string object)))))

(defun symbol-methods (symbol)
  (and (fboundp symbol)
       (typep (fdefinition symbol) 'generic-function)
       (ignore-errors (sb-mop:generic-function-methods (fdefinition symbol)))))

(defun method-report (method)
  (let* ((source (ignore-errors (sb-introspect:find-definition-source method)))
         (path (and source (sb-introspect:definition-source-pathname source))))
    (append (list :specializers (print-standard (mapcar #'specializer-name
                                                        (sb-mop:method-specializers method)))
                  :qualifiers (print-standard (method-qualifiers method)))
            (and path (list :file (namestring path)
                            :position (sb-introspect:definition-source-character-offset source))))))

(defun specializer-name (specializer)
  (if (typep specializer 'sb-mop:eql-specializer)
      (list 'eql (sb-mop:eql-specializer-object specializer))
      (class-name specializer)))

(defun function-form (symbol)
  (let ((expression (ignore-errors (function-lambda-expression (fdefinition symbol)))))
    (and expression
         (let ((text (print-standard expression)))
           (and text
                (list :form (subseq text 0 (min (length text) +source-form-limit+))
                      :truncated (> (length text) +source-form-limit+)))))))

(defun function-source (symbol)
  (let ((source (first (ignore-errors
                         (sb-introspect:find-definition-sources-by-name symbol :function)))))
    (and source
         (let ((path (sb-introspect:definition-source-pathname source)))
           (and path (list :file (namestring path)
                           :position (sb-introspect:definition-source-character-offset source)))))))

;;; --- :packages -----------------------------------------------------

(defun op-packages ()
  (ok :packages (mapcar #'package-info
                        (sort (copy-list (list-all-packages)) #'string< :key #'package-name))))

(defun package-info (package)
  (list :name (package-name package) :nicknames (package-nicknames package)))
