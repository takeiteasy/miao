(in-package #:miao/tests)
(in-suite :miao)

;;; The allowlist gate on its own (~takeiteasy/miao#44): what it reads, what
;;; it refuses and the canonical text it hands a worker. Nothing here starts
;;; a worker; see the gated-eval tests in tools.lisp for that.

(defun gate-refuses (text &optional reason)
  (multiple-value-bind (source why) (miao:gate-check text)
    (and (null source) (stringp why)
         (or (null reason) (search reason why)))))

(defun gate-accepts (text)
  (values (miao:gate-check text)))

(test gate-prints-a-form-in-canonical-text
  (is (equal "(COMMON-LISP:+ 1 2)" (gate-accepts "(+ 1 2)")))
  (is (equal "(COMMON-LISP:LIST :K MIAO-GATE::FOO COMMON-LISP:NIL)"
             (gate-accepts "(list :k foo nil)")))
  (is (equal "(COMMON-LISP:QUOTE (1 2))" (gate-accepts "'(1 2)")))
  (is (equal "(COMMON-LISP:FUNCTION COMMON-LISP:CAR)" (gate-accepts "#'car")))
  (is (equal "()" (gate-accepts "()"))))

(test gate-folds-symbol-names-to-upper-case
  (is (equal "(COMMON-LISP:CAR MIAO-GATE::XY)" (gate-accepts "(Car xY)"))))

(test gate-reads-atoms
  (is (equal "(COMMON-LISP:LIST 1 -2 1/2 1.5d0 -0.5d0 100.0d0 2 \"a\\\"b\\\\\" #\\a #\\Space)"
             (gate-accepts "(list 1 -2 2/4 1.5 -.5 1e2 2. \"a\\\"b\\\\\" #\\a #\\space)"))))

(test gate-prints-a-float-so-that-it-reads-back-as-a-double
  (let ((source (gate-accepts "1.5")))
    (is (eql 1.5d0 (let ((*read-default-float-format* 'single-float))
                     (read-from-string source))))))

(test gate-skips-comments
  (is (equal "(COMMON-LISP:+ 1 2)" (gate-accepts "; one
(+ 1 ; two
   2)"))))

(test gate-takes-exactly-one-form
  (is (gate-refuses "(+ 1 2) 3" "one form"))
  (is (gate-refuses "" "end of input"))
  (is (gate-refuses "(+ 1 2" "unclosed"))
  (is (gate-refuses ")" "unexpected")))

;;; --- the reader ---------------------------------------------------------

(test gate-refuses-reader-syntax-it-does-not-read
  (dolist (text '("#.(+ 1 2)" "#+sbcl 1" "#-sbcl 1" "#S(foo)" "#P\"/etc\""
                  "#C(1 2)" "#*101" "#:foo" "#(1 2)" "#|x|# 1" "#1=(a . #1#)"
                  "`(a ,b)" ",a" "|x|" "a\\b" "(a . b)" "."))
    (is (gate-refuses text) "~s should be refused" text)))

(test gate-refuses-a-package-prefix
  (is (gate-refuses "sb-ext:quit" "package prefix"))
  (is (gate-refuses "cl::car" "package prefix"))
  (is (gate-refuses "cl:car" "package prefix"))
  (is (gate-refuses "(list ::x)" "package prefix")))

(test gate-refuses-an-unknown-character-name-or-escape
  (is (gate-refuses "#\\Nosuchname" "character name"))
  (is (gate-refuses "\"a\\nb\"" "escaped")))

(test gate-refuses-a-form-past-its-limits
  (is (gate-refuses (make-string (1+ miao::*gate-max-source*) :initial-element #\1)
                    "longer"))
  (is (gate-refuses (make-string (1+ miao::*gate-max-token*) :initial-element #\a)
                    "token"))
  (let ((deep (concatenate 'string
                           (make-string 200 :initial-element #\()
                           (make-string 200 :initial-element #\)))))
    (is (gate-refuses deep "nested")))
  (is (gate-refuses (format nil "~{'~*~}1" (make-list 200)) "nested")))

(test gate-bounds-a-number-before-computing-it
  (is (gate-refuses "1e1000000000" "out of range"))
  (is (gate-refuses "1d400" "out of range"))
  (is (gate-refuses "1/0" "zero"))
  (is (gate-accepts (make-string miao::*gate-max-token* :initial-element #\9))))

(test gate-reads-a-number-lookalike-as-a-symbol
  (is (equal "(COMMON-LISP:1+ MIAO-GATE::E5 MIAO-GATE::1F5)"
             (gate-accepts "(1+ e5 1f5)"))))

;;; --- the allowlist ------------------------------------------------------

(test gate-refuses-a-cl-symbol-that-is-not-allowed
  (dolist (text '("(intern \"X\")" "(read-from-string \"1\")" "(eval 1)"
                  "(find-symbol \"CAR\")" "(symbol-function 'car)" "(type-of 1)"
                  "(class-of 1)" "(coerce \"car\" 'function)" "(open \"/etc/passwd\")"
                  "*read-eval*" "*package*" "(setf *print-base* 2)"
                  "(defmacro m () 1)" "(defclass c () ())" "(throw 'x 1)"
                  "(warn \"x\")" "(signal 'error)" "(assert nil)"
                  "(cerror \"x\" \"y\")"))
    (is (gate-refuses text "not an allowed") "~s should be refused" text)))

(test gate-refuses-declarations-and-the
  (dolist (text '("(declare (optimize (safety 0)))" "(declaim (optimize (safety 0)))"
                  "(proclaim '(optimize (safety 0)))" "(locally 1)"
                  "(the fixnum 1)" "(let ((x 1)) (declare (dynamic-extent x)) x)"))
    (is (gate-refuses text "not an allowed") "~s should be refused" text)))

(test gate-checks-a-symbol-in-quoted-data
  (is (gate-refuses "'(a intern)" "INTERN"))
  (is (gate-refuses "'(a pi)" "PI"))
  (is (gate-accepts "'(a car b)")))

(test gate-checks-a-symbol-in-a-lambda-list-and-a-binding
  (is (gate-accepts "(defun f (a &optional (b 1) &rest c) (list a b c))"))
  (is (gate-refuses "(defun f (a &whole b) a)" "&WHOLE"))
  (is (gate-refuses "(let ((x (intern \"A\"))) x)" "INTERN")))

(test gate-allows-a-fresh-name-that-cl-does-not-have
  (is (gate-accepts "(let ((foo 1) (bar 2)) (+ foo bar))"))
  (is (search "MIAO-GATE::FOO" (gate-accepts "'foo"))))

(test gate-allowlist-names-only-cl-externals
  (dolist (name miao::*gate-allowed-names*)
    (is (eq :external (nth-value 1 (find-symbol name :common-lisp)))
        "~a is not a CL external" name)))

(test gate-keeps-its-hazards-off-the-allowlist
  (dolist (name '("INTERN" "READ" "EVAL" "COERCE" "TYPE-OF" "CLASS-OF" "GET"
                  "SYMBOL-VALUE" "SYMBOL-FUNCTION" "FDEFINITION" "DECLARE"
                  "THE" "WARN" "SIGNAL" "ASSERT" "OPEN" "FIND-PACKAGE"))
    (is (not (member name miao::*gate-allowed-names* :test #'string=))
        "~a should stay off the allowlist" name)))

;;; --- loop ---------------------------------------------------------------

(test gate-accepts-loop-with-its-keywords
  (is (gate-accepts "(loop for x in '(1 2 3) collect (* x x))"))
  (is (gate-accepts "(loop for i from 0 below 3 sum i)"))
  (is (gate-accepts "(loop for k being the hash-keys of h using (hash-value v) collect (list k v))"))
  (is (gate-accepts "(loop repeat 3 do (princ \"x\") finally (return 1))")))

(test gate-refuses-loop-package-iteration
  (dolist (text '("(loop for s being the external-symbols of \"CL\" collect s)"
                  "(loop for s being the symbols of \"CL\" collect s)"
                  "(loop for s being the present-symbols of \"CL\" collect s)"
                  "(loop for s being each external-symbol in \"CL\" collect s)"
                  "(list :external-symbols)"))
    (is (gate-refuses text "not allowed") "~s should be refused" text)))

(test gate-allows-the-only-inside-loop-keywords
  (is (gate-refuses "(loop for x in (the list y) collect x)" "THE"))
  (is (gate-refuses "(list the)" "THE")))

;;; --- format and error ---------------------------------------------------

(test gate-accepts-format-with-a-literal-control-string
  (is (gate-accepts "(format nil \"~a and ~s: ~10,'0d ~:@(~a~)\" 1 2 3 4)"))
  (is (gate-accepts "(error \"boom ~a\" 1)")))

(test gate-refuses-a-control-string-that-calls-out
  (is (gate-refuses "(format nil \"~/cl:car/\" 1)" "may not use"))
  (is (gate-refuses "(format nil \"~10,2/x:y/\" 1)" "may not use"))
  (is (gate-refuses "(format nil \"~?\" \"~a\" '(1))" "may not use"))
  (is (gate-refuses "(format nil \"~@?\" \"~a\" 1)" "may not use"))
  (is (gate-refuses "(error \"~/cl:car/\" 1)" "may not use"))
  (is (gate-accepts "(format nil \"~~/ ~~?\")")))

(test gate-refuses-a-control-string-built-at-runtime
  (is (gate-refuses "(format nil (concatenate 'string \"~\" \"/cl:car/\") 1)" "literal"))
  (is (gate-refuses "(format nil x)" "literal"))
  (is (gate-refuses "(error x)" "literal"))
  (is (gate-refuses "(error 'simple-error)" "literal")))

(test gate-refuses-format-and-error-reached-as-a-value
  (dolist (text '("(funcall #'format nil \"x\")" "(funcall 'format nil \"x\")"
                  "(apply #'error '(\"x\"))" "(mapcar 'error '(\"x\"))"
                  "(list format)"))
    (is (gate-refuses text "called directly") "~s should be refused" text))
  (is (gate-refuses "(let ((f (car '(format)))) f)")))

(test gate-reads-error-as-a-type-in-handler-case
  (is (gate-accepts "(handler-case (car 1) (error (e) e))"))
  (is (gate-accepts "(handler-case (car 1) ((or type-error division-by-zero) () 0))"))
  (is (gate-refuses "(handler-case (car 1) ((satisfies foo) (e) e))" "SATISFIES"))
  (is (gate-refuses "(handler-case (car 1) (error (e) (funcall 'error x)))" "called directly")))
