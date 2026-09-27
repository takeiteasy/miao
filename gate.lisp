(in-package #:miao)

;;; The allowlist gate for untrusted Lisp (~takeiteasy/miao#44). GATE-CHECK
;;; reads a form with its own reader, never the CL reader, so nothing is
;;; interned and no reader macro runs on the host. It then prints the form
;;; back as canonical text, which is what a worker evaluates.
;;;
;;; Invariant: every symbol in the form is a keyword, an allowlisted CL
;;; external, or a fresh name in MIAO-GATE, a package that uses nothing. No
;;; allowlisted operator returns a symbol the source did not contain, so
;;; nothing outside the allowlist is reachable however the form is shaped.
;;; Adding an entry means asking whether it breaks that.

(defparameter *gate-max-source* 20000 "Characters a form may have.")
(defparameter *gate-max-depth* 100 "Levels of nesting a form may have.")
(defparameter *gate-max-token* 100 "Characters a symbol or number may have.")
(defparameter *gate-max-exponent* 300 "Magnitude of a float's exponent.")

(defparameter *gate-allowed-names*
  '(;; special forms and macros
    "QUOTE" "FUNCTION" "LAMBDA" "LET" "LET*" "FLET" "LABELS" "PROGN" "PROG1"
    "IF" "COND" "CASE" "ECASE" "WHEN" "UNLESS" "AND" "OR" "NOT" "BLOCK"
    "RETURN" "RETURN-FROM" "TAGBODY" "GO" "SETQ" "SETF" "PSETF" "INCF" "DECF"
    "PUSH" "POP" "PUSHNEW" "MULTIPLE-VALUE-BIND" "MULTIPLE-VALUE-LIST"
    "VALUES" "VALUES-LIST" "DESTRUCTURING-BIND" "DO" "DO*" "DOLIST" "DOTIMES"
    "LOOP" "UNWIND-PROTECT" "HANDLER-CASE" "IGNORE-ERRORS" "TYPECASE"
    "ETYPECASE" "DEFUN" "DEFPARAMETER" "OTHERWISE" "T" "NIL"
    "&OPTIONAL" "&REST" "&KEY" "&AUX" "&ALLOW-OTHER-KEYS"
    ;; calling
    "FUNCALL" "APPLY" "IDENTITY" "COMPLEMENT" "CONSTANTLY"
    ;; conditions: types for HANDLER-CASE clauses
    "ERROR" "SIMPLE-ERROR" "TYPE-ERROR" "ARITHMETIC-ERROR" "DIVISION-BY-ZERO"
    "SERIOUS-CONDITION" "STORAGE-CONDITION"
    ;; equality and predicates
    "EQ" "EQL" "EQUAL" "EQUALP" "NULL" "ATOM" "CONSP" "LISTP" "SYMBOLP"
    "KEYWORDP" "STRINGP" "CHARACTERP" "NUMBERP" "INTEGERP" "RATIONALP"
    "FLOATP" "REALP" "FUNCTIONP" "VECTORP" "ARRAYP" "HASH-TABLE-P" "TYPEP"
    ;; types, named in TYPEP, MAKE-ARRAY, MAP and CONCATENATE
    "INTEGER" "FIXNUM" "FLOAT" "DOUBLE-FLOAT" "SINGLE-FLOAT" "RATIO"
    "RATIONAL" "REAL" "NUMBER" "STRING" "CHARACTER" "LIST" "VECTOR"
    "SEQUENCE" "CONS" "BIT" "UNSIGNED-BYTE" "SIGNED-BYTE" "OR"
    ;; lists
    "CAR" "CDR" "CAAR" "CADR" "CDAR" "CDDR" "CADDR" "FIRST" "SECOND" "THIRD"
    "FOURTH" "FIFTH" "REST" "LAST" "BUTLAST" "NTH" "NTHCDR" "LIST*" "APPEND"
    "REVERSE" "NREVERSE" "LENGTH" "LIST-LENGTH" "ELT" "SUBSEQ" "COPY-LIST"
    "COPY-SEQ" "COPY-TREE" "MEMBER" "ASSOC" "RASSOC" "ADJOIN" "UNION"
    "INTERSECTION" "SET-DIFFERENCE" "REMOVE" "REMOVE-IF" "REMOVE-IF-NOT"
    "REMOVE-DUPLICATES" "DELETE" "DELETE-IF" "FIND" "FIND-IF" "POSITION"
    "POSITION-IF" "COUNT" "COUNT-IF" "SORT" "STABLE-SORT" "MAPCAR" "MAPC"
    "MAPCAN" "MAPLIST" "MAP" "REDUCE" "EVERY" "SOME" "NOTANY" "NOTEVERY"
    "FILL" "REPLACE" "SEARCH" "MISMATCH" "SUBSTITUTE" "GETF" "ACONS" "PAIRLIS"
    "ENDP" "TREE-EQUAL" "CONCATENATE"
    ;; numbers
    "+" "-" "*" "/" "1+" "1-" "=" "/=" "<" ">" "<=" ">=" "MIN" "MAX" "ABS"
    "MOD" "REM" "FLOOR" "CEILING" "TRUNCATE" "ROUND" "GCD" "LCM" "EXPT"
    "SQRT" "ISQRT" "EXP" "LOG" "SIN" "COS" "TAN" "ATAN" "ASIN" "ACOS" "SIGNUM"
    "ZEROP" "PLUSP" "MINUSP" "EVENP" "ODDP" "NUMERATOR" "DENOMINATOR" "RANDOM"
    "LOGAND" "LOGIOR" "LOGXOR" "LOGNOT" "ASH" "INTEGER-LENGTH" "LOGCOUNT"
    "PARSE-INTEGER"
    ;; strings and characters
    "STRING=" "STRING<" "STRING>" "STRING-EQUAL" "STRING-UPCASE"
    "STRING-DOWNCASE" "STRING-CAPITALIZE" "STRING-TRIM" "STRING-LEFT-TRIM"
    "STRING-RIGHT-TRIM" "CHAR" "SCHAR" "CHAR=" "CHAR<" "CHAR>" "CHAR-CODE"
    "CODE-CHAR" "CHAR-UPCASE" "CHAR-DOWNCASE" "ALPHA-CHAR-P" "DIGIT-CHAR-P"
    "DIGIT-CHAR" "ALPHANUMERICP" "UPPER-CASE-P" "LOWER-CASE-P" "MAKE-STRING"
    ;; hash tables and arrays
    "MAKE-HASH-TABLE" "GETHASH" "REMHASH" "CLRHASH" "MAPHASH"
    "HASH-TABLE-COUNT" "MAKE-ARRAY" "AREF" "SVREF" "VECTOR-PUSH"
    "VECTOR-PUSH-EXTEND" "VECTOR-POP" "ARRAY-DIMENSION" "ARRAY-TOTAL-SIZE"
    "FILL-POINTER"
    ;; output, to strings or to the worker's captured *STANDARD-OUTPUT*
    "PRINC" "PRIN1" "PRINT" "TERPRI" "FRESH-LINE" "WRITE-STRING" "WRITE-CHAR"
    "WRITE-LINE" "PRINC-TO-STRING" "PRIN1-TO-STRING" "WRITE-TO-STRING"
    "WITH-OUTPUT-TO-STRING" "MAKE-STRING-OUTPUT-STREAM"
    "GET-OUTPUT-STREAM-STRING"
    ;; called only directly, with a literal control string
    "FORMAT")
  "The CL externals a form may name. Everything else in CL is refused.")

;;; Left out on purpose:
;;;   symbols and packages: INTERN, FIND-SYMBOL, MAKE-SYMBOL, SYMBOL-NAME,
;;;     SYMBOL-VALUE, SYMBOL-FUNCTION, SYMBOL-PLIST, GET, SYMBOL-PACKAGE,
;;;     FDEFINITION, DO-SYMBOLS, every package function
;;;   things that return a symbol the form did not contain: TYPE-OF,
;;;     CLASS-OF, FUNCTION-LAMBDA-EXPRESSION, condition and restart accessors
;;;   the reader and the compiler: READ*, EVAL, COMPILE, COERCE, MACROEXPAND
;;;   files and streams other than string streams
;;;   special variables such as *READ-EVAL* and *PACKAGE*
;;;   DECLARE, DECLAIM, PROCLAIM, LOCALLY, THE outside LOOP: SAFETY 0 and
;;;     DYNAMIC-EXTENT break memory safety
;;;   CLOS, DEFMACRO, CATCH and THROW
;;;   condition makers other than ERROR: WARN, SIGNAL, CERROR, ASSERT,
;;;     MAKE-CONDITION take control strings too

(defparameter *gate-control-string-names* '("FORMAT" "ERROR")
  "Operators that take a format control string. A control string can name a
function with ~/pkg:fn/, or splice in another with ~?, so each is only
callable directly, with a literal string that has neither. Reached any
other way -- #'FORMAT, 'ERROR -- one would take a string built at runtime.")

(defparameter *gate-refused-names*
  '("SYMBOL" "SYMBOLS" "PRESENT-SYMBOL" "PRESENT-SYMBOLS"
    "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS")
  "Refused as a symbol or a keyword. LOOP compares its keywords by name, and
these make it walk a package.")

(defvar *gate-allowed*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (name *gate-allowed-names* table)
      (unless (eq :external (nth-value 1 (find-symbol name :common-lisp)))
        (error "The gate allowlist names ~a, which CL does not export." name))
      (setf (gethash name table) t))))

(define-condition gate-rejection (error)
  ((reason :initarg :reason :reader gate-rejection-reason))
  (:report (lambda (c s) (write-string (gate-rejection-reason c) s))))

(defun %gate-reject (control &rest args)
  (error 'gate-rejection :reason (apply #'format nil control args)))

;;; --- reading -------------------------------------------------------------

(defstruct (gsym (:constructor make-gsym (name keyword-p)))
  "A symbol as the gate read it: a name and whether it had a leading colon."
  name keyword-p)

(defvar *gate-text*)
(defvar *gate-pos*)

(defun %peek ()
  (when (< *gate-pos* (length *gate-text*))
    (char *gate-text* *gate-pos*)))

(defun %next ()
  (prog1 (%peek) (incf *gate-pos*)))

(defun %blank-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun %constituent-p (c)
  (and c (not (%blank-p c)) (not (find c "()'\";"))))

(defun %digit-p (c)
  (char<= #\0 c #\9))

(defun %skip-blank ()
  (loop for c = (%peek)
        do (cond ((null c) (return))
                 ((%blank-p c) (incf *gate-pos*))
                 ((char= c #\;)
                  (loop for d = (%next) until (or (null d) (char= d #\Newline))))
                 (t (return)))))

(defun %read-form (depth)
  (when (> depth *gate-max-depth*)
    (%gate-reject "form nested deeper than ~d" *gate-max-depth*))
  (%skip-blank)
  (case (%peek)
    ((nil) (%gate-reject "unexpected end of input"))
    (#\( (incf *gate-pos*) (%read-list depth))
    (#\) (%gate-reject "unexpected )"))
    (#\' (incf *gate-pos*)
     (list (make-gsym "QUOTE" nil) (%read-form (1+ depth))))
    (#\" (incf *gate-pos*) (%read-string))
    (#\# (incf *gate-pos*) (%read-dispatch depth))
    (t (%read-token))))

(defun %read-list (depth)
  (let ((items '()))
    (loop
      (%skip-blank)
      (let ((c (%peek)))
        (cond ((null c) (%gate-reject "unclosed ("))
              ((char= c #\)) (incf *gate-pos*) (return (nreverse items)))
              (t (push (%read-form (1+ depth)) items)))))))

(defun %read-string ()
  (with-output-to-string (out)
    (loop for c = (%next)
          do (cond ((null c) (%gate-reject "unclosed string"))
                   ((char= c #\") (return))
                   ((char= c #\\)
                    (let ((escaped (%next)))
                      (unless (member escaped '(#\" #\\))
                        (%gate-reject "only \\\" and \\\\ may be escaped in a string"))
                      (write-char escaped out)))
                   (t (write-char c out))))))

(defun %read-dispatch (depth)
  (case (%next)
    (#\' (list (make-gsym "FUNCTION" nil) (%read-form (1+ depth))))
    (#\\ (%read-character))
    (t (%gate-reject "unsupported reader syntax after #"))))

(defun %read-character ()
  (let ((first (%next)))
    (unless (and first (graphic-char-p first) (char/= first #\Space))
      (%gate-reject "unsupported character syntax"))
    (let ((more (loop while (%constituent-p (%peek)) collect (%next))))
      (if (null more)
          first
          (let ((name (string-upcase (coerce (cons first more) 'string))))
            (cond ((string= name "SPACE") #\Space)
                  ((string= name "NEWLINE") #\Newline)
                  ((string= name "TAB") #\Tab)
                  ((string= name "RETURN") #\Return)
                  (t (%gate-reject "unknown character name ~a" name))))))))

(defun %read-token ()
  (let ((start *gate-pos*))
    (loop while (%constituent-p (%peek)) do (incf *gate-pos*))
    (let ((token (subseq *gate-text* start *gate-pos*)))
      (when (> (length token) *gate-max-token*)
        (%gate-reject "a token is longer than ~d characters" *gate-max-token*))
      (or (%parse-number token) (%parse-symbol token)))))

(defun %name-char-p (c)
  (or (and (< (char-code c) 128) (alphanumericp c))
      (find c "+-*/<>=!?%&_^~.@$")))

(defun %parse-symbol (token)
  (let* ((keyword-p (char= (char token 0) #\:))
         (name (if keyword-p (subseq token 1) token)))
    (when (or (zerop (length name)) (every (lambda (c) (char= c #\.)) name))
      (%gate-reject "~s is not a symbol" token))
    (loop for c across name
          unless (%name-char-p c)
            do (%gate-reject "~s is not allowed in ~s~:[~; (a package prefix)~]"
                             c token (char= c #\:)))
    (make-gsym (string-upcase name) keyword-p)))

(defun %parse-number (token)
  "TOKEN as an integer, a ratio or a double-float, or NIL when it is none of
them. A float is always a double, whatever its exponent marker."
  (let ((n (length token)) (i 0) (sign 1))
    (labels ((at (chars) (and (< i n) (find (char token i) chars)))
             (digits ()
               (let ((start i))
                 (loop while (and (< i n) (%digit-p (char token i))) do (incf i))
                 (subseq token start i)))
             (signed ()
               (let ((s 1))
                 (when (at "+-")
                   (when (char= (char token i) #\-) (setf s -1))
                   (incf i))
                 s)))
      (setf sign (signed))
      (let ((int (digits)) (frac "") (dot nil) (exponent nil))
        (cond
          ((at "/")
           (incf i)
           (let ((denominator (digits)))
             (when (and (plusp (length int)) (plusp (length denominator)) (= i n))
               (when (zerop (parse-integer denominator))
                 (%gate-reject "~a divides by zero" token))
               (* sign (/ (parse-integer int) (parse-integer denominator))))))
          (t
           (when (at ".")
             (setf dot t)
             (incf i)
             (setf frac (digits)))
           (when (at "eEdD")
             (incf i)
             (let* ((exponent-sign (signed))
                    (e (digits)))
               (when (zerop (length e))
                 (return-from %parse-number nil))
               (setf exponent (* exponent-sign (parse-integer e)))))
           (cond
             ((< i n) nil)
             ((zerop (+ (length int) (length frac))) nil)
             ((and (null exponent) (or (not dot) (zerop (length frac))))
              (* sign (parse-integer int)))
             (t (%gate-float sign int frac exponent token)))))))))

(defun %gate-float (sign int frac exponent token)
  (when (and exponent (> (abs exponent) *gate-max-exponent*))
    (%gate-reject "the exponent of ~a is out of range" token))
  (handler-case
      (* sign (coerce (* (parse-integer (concatenate 'string int frac))
                         (expt 10 (- (or exponent 0) (length frac))))
                      'double-float))
    (error () (%gate-reject "~a is out of range" token))))

;;; --- walking and printing ------------------------------------------------

(defun %symbol-kind (sym &key allow-control loop-atom)
  "SYM's kind -- :KEYWORD, :CL or :FRESH -- or a rejection."
  (let ((name (gsym-name sym)))
    (when (member name *gate-refused-names* :test #'string=)
      (%gate-reject "~a is not allowed" name))
    (cond ((gsym-keyword-p sym) :keyword)
          ((null (nth-value 1 (find-symbol name :common-lisp))) :fresh)
          ((and loop-atom (string= name "THE")) :cl)
          ((member name *gate-control-string-names* :test #'string=)
           (unless allow-control
             (%gate-reject "~a may only be called directly, with a literal control string"
                           name))
           :cl)
          ((gethash name *gate-allowed*) :cl)
          (t (%gate-reject "~a is not an allowed symbol" name)))))

(defun %emit-symbol (sym out &rest options)
  (let ((kind (apply #'%symbol-kind sym options)))
    (write-string (ecase kind
                    (:keyword ":")
                    (:cl "COMMON-LISP:")
                    (:fresh "MIAO-GATE::"))
                  out)
    (write-string (gsym-name sym) out)))

(defun %emit-character (c out)
  (write-string "#\\" out)
  (write-string (case c
                  (#\Space "Space")
                  (#\Newline "Newline")
                  (#\Tab "Tab")
                  (#\Return "Return")
                  (t (string c)))
                out))

(defun %emit (form out)
  (typecase form
    (null (write-string "()" out))
    (gsym (%emit-symbol form out))
    (cons (%emit-list form out))
    (string (prin1 form out))
    (character (%emit-character form out))
    (number (prin1 form out))
    (t (%gate-reject "unsupported datum"))))

(defun %check-directives (control)
  "Reject a control string with ~/ or ~?, whichever its parameters and
modifiers are."
  (let ((i 0) (n (length control)))
    (loop while (< i n)
          do (if (char/= (char control i) #\~)
                 (incf i)
                 (progn
                   (incf i)
                   (loop while (and (< i n) (find (char control i) "0123456789,+-#vV:@'"))
                         do (incf i (if (char= (char control i) #\') 2 1)))
                   (when (and (< i n) (find (char control i) "/?"))
                     (%gate-reject "a format control string may not use ~~~a" (char control i)))
                   (incf i))))))

(defun %check-control-string (name form)
  (let ((control (if (string= name "FORMAT") (third form) (second form))))
    (unless (stringp control)
      (%gate-reject "~a needs a literal control string" name))
    (%check-directives control)))

(defun %emit-items (items out &key control loop-atoms)
  (write-char #\( out)
  (loop for item in items
        for first = t then nil
        do (unless first (write-char #\Space out))
           (cond ((and first control) (%emit-symbol item out :allow-control t))
                 ((and loop-atoms (not first) (gsym-p item))
                  (%emit-symbol item out :loop-atom t))
                 (t (%emit item out))))
  (write-char #\) out))

(defun %emit-type (type out)
  "A condition type from a HANDLER-CASE clause, where ERROR names a type
rather than a call."
  (typecase type
    (gsym (%emit-symbol type out :allow-control t))
    (cons (write-char #\( out)
          (loop for x in type
                for first = t then nil
                do (unless first (write-char #\Space out))
                   (%emit-type x out))
          (write-char #\) out))
    (t (%emit type out))))

(defun %emit-handler-case (form out)
  (when (null (rest form))
    (%gate-reject "HANDLER-CASE needs a form"))
  (write-char #\( out)
  (%emit-symbol (first form) out)
  (write-char #\Space out)
  (%emit (second form) out)
  (dolist (clause (cddr form))
    (unless (consp clause)
      (%gate-reject "a HANDLER-CASE clause must be a list"))
    (write-string " (" out)
    (%emit-type (first clause) out)
    (dolist (item (rest clause))
      (write-char #\Space out)
      (%emit item out))
    (write-char #\) out))
  (write-char #\) out))

(defun %emit-list (form out)
  (let* ((head (first form))
         (name (and (gsym-p head) (not (gsym-keyword-p head)) (gsym-name head))))
    (cond ((member name *gate-control-string-names* :test #'equal)
           (%check-control-string name form)
           (%emit-items form out :control t))
          ((equal name "HANDLER-CASE") (%emit-handler-case form out))
          ((equal name "LOOP") (%emit-items form out :loop-atoms t))
          (t (%emit-items form out)))))

(defun gate-check (text)
  "Check TEXT, the source of one form, against the allowlist. Two values:
the canonical source a worker may evaluate and NIL, or NIL and the reason
TEXT was refused."
  (handler-case
      (let ((*gate-text* text)
            (*gate-pos* 0)
            (*print-pretty* nil) (*print-readably* nil) (*print-escape* t)
            (*print-base* 10) (*print-radix* nil)
            (*read-default-float-format* 'single-float))
        (when (> (length text) *gate-max-source*)
          (%gate-reject "the form is longer than ~d characters" *gate-max-source*))
        (let ((form (%read-form 0)))
          (%skip-blank)
          (when (%peek)
            (%gate-reject "only one form is allowed"))
          (values (with-output-to-string (out) (%emit form out)) nil)))
    (gate-rejection (c) (values nil (gate-rejection-reason c)))))
