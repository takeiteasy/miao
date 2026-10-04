(in-package #:miao)

;;; Typed parameter schemas. A schema is canonical in both directions: it
;;; renders to the JSON Schema a model expects, and imports from one an
;;; external tool arrives with. See docs/schema.md.
;;;
;;; Specifiers are compared by symbol name, so a schema written in any
;;; package reads the same.

(defvar *absent* '#:absent
  "Marks a parameter the caller did not supply, which an explicit NIL is not.")

(defun param-name (param) (first param))
(defun param-type (param) (second param))
(defun param-options (param) (cddr param))

(defun options-condition (options)
  "OPTIONS' :REQUIRED-WHEN as (controller . values), or NIL."
  (a:when-let ((condition (getf options :required-when)))
    (cons (first condition) (a:ensure-list (second condition)))))

(defun param-condition (param)
  (options-condition (param-options param)))

(defun param-key (param)
  "PARAM's name as a JSON property name."
  (string-downcase (symbol-name (param-name param))))

;;; --- the closed vocabulary -------------------------------------------

(defun spec-head (spec)
  (if (consp spec) (first spec) spec))

(defun spec-is (spec name)
  (let ((head (spec-head spec)))
    (and (symbolp head) (string= (symbol-name head) name))))

(defun validate-schema (schema)
  "Signal an error unless SCHEMA is a parameter list over the vocabulary.
A specifier outside it is a definition error, not a silent pass-through."
  (unless (listp schema)
    (error "Schema must be a list of parameters, got ~s." schema))
  (dolist (param schema schema)
    (unless (and (consp param) (keywordp (param-name param)))
      (error "Parameter must be (:name specifier . options), got ~s." param))
    (let ((options (param-options param)))
      (unless (evenp (length options))
        (error "Options of ~s must be a plist." (param-name param)))
      (loop for key in options by #'cddr
            unless (member key '(:doc :required :default :required-when))
              do (error "Unknown option ~s on ~s." key (param-name param))))
    (validate-specifier (param-type param)))
  (dolist (param schema)
    (when (getf (param-options param) :required-when)
      (validate-condition param schema)))
  schema)

(defun validate-condition (param schema)
  "Signal an error unless PARAM's :REQUIRED-WHEN, written (:controller :value)
or (:controller (:value ...)), names a member parameter of SCHEMA and values it
holds, and PARAM is otherwise optional."
  (let* ((name (param-name param))
         (options (param-options param))
         (written (getf options :required-when))
         (condition (param-condition param))
         (controller (find (car condition) schema :key #'param-name)))
    (unless (and (listp written) (= 2 (length written)) (keywordp (first written))
                 (every #'keywordp (cdr condition)) (cdr condition))
      (error ":required-when on ~s must be (:param :value) or (:param (:value ...)), got ~s."
             name written))
    (when (or (getf options :required) (member :default options))
      (error "~s takes :required-when or one of :required and :default, not both." name))
    (unless (and controller (not (eq controller param))
                 (spec-is (param-type controller) "MEMBER"))
      (error ":required-when on ~s names ~s, which is not another member parameter."
             name (car condition)))
    (dolist (value (cdr condition))
      (unless (member value (rest (param-type controller)))
        (error ":required-when on ~s names ~s, not a member of ~s."
               name value (car condition))))))

(defun validate-specifier (spec)
  (cond
    ((or (spec-is spec "STRING") (spec-is spec "NUMBER") (spec-is spec "BOOLEAN"))
     (when (consp spec) (error "~s takes no arguments." (spec-head spec))))
    ((spec-is spec "INTEGER")
     (unless (and (<= (length (a:ensure-list spec)) 3)
                  (every (lambda (bound) (or (eq bound '*) (integerp bound)))
                         (when (consp spec) (rest spec))))
       (error "Bad integer bounds in ~s." spec)))
    ((spec-is spec "MEMBER")
     (unless (and (consp spec) (rest spec) (every #'keywordp (rest spec)))
       (error "member takes one or more keywords, got ~s." spec)))
    ((spec-is spec "OR")
     (unless (and (consp spec) (= 3 (length spec)) (spec-is (second spec) "NULL"))
       (error "Only (or null X) is supported, got ~s." spec))
     (validate-specifier (third spec)))
    ((or (spec-is spec "ARRAY-OF") (spec-is spec "MAP-OF"))
     (unless (and (consp spec) (= 2 (length spec)))
       (error "~a takes one specifier, got ~s." (spec-head spec) spec))
     (validate-specifier (second spec)))
    ((spec-is spec "OBJECT")
     (validate-schema (rest spec)))
    ((spec-is spec "ANY")
     (when (consp spec) (error "~s takes no arguments." (spec-head spec))))
    (t (error "Unknown specifier ~s." spec))))

;;; --- coercion ---------------------------------------------------------

;;; Model-supplied arguments arrive as strings whatever the declared type,
;;; and in-image callers pass keywords. The schema drives coercion, so no
;;; tool carries its own.

(defun coerce-args (schema args)
  "ARGS coerced and checked against SCHEMA. Returns the coerced plist, or
NIL and a message naming the parameter at fault."
  (validate-schema schema)
  (unless (and (listp args) (evenp (length args)))
    (return-from coerce-args (values nil "arguments must be a plist")))
  (loop for (name) on args by #'cddr
        unless (find name schema :key #'param-name)
          do (return-from coerce-args
               (values nil (format nil "unknown parameter ~(~s~)" name))))
  (let ((coerced '()))
    (dolist (param schema)
      (let ((value (getf args (param-name param) *absent*))
            (options (param-options param)))
        (cond
          ((not (eq value *absent*))
           (multiple-value-bind (result ok) (coerce-value value (param-type param))
             (unless ok
               (return-from coerce-args
                 (values nil (format nil "~(~s~) must be ~a"
                                     (param-name param)
                                     (describe-specifier (param-type param))))))
             (push (param-name param) coerced)
             (push result coerced)))
          ((getf options :required)
           (return-from coerce-args
             (values nil (format nil "~(~s~) is required" (param-name param)))))
          ((member :default options)
           (push (param-name param) coerced)
           (push (getf options :default) coerced)))))
    (setf coerced (nreverse coerced))
    (a:when-let ((param (find-if (lambda (param) (conditionally-missing param coerced))
                                 schema)))
      (return-from coerce-args
        (values nil (format nil "~(~s~) is required when ~(~s~) is ~(~{~a~^ or ~}~)"
                            (param-name param) (car (param-condition param))
                            (cdr (param-condition param))))))
    (values coerced nil)))

(defun conditionally-missing (param coerced)
  "True when PARAM's :REQUIRED-WHEN holds in COERCED and PARAM is absent."
  (a:when-let ((condition (param-condition param)))
    (and (member (getf coerced (car condition)) (cdr condition))
         (eq *absent* (getf coerced (param-name param) *absent*)))))

(defun coerce-value (value spec)
  "VALUE as SPEC, and T when it coerced. NIL alone is a coerced (or null X)."
  (cond
    ((spec-is spec "STRING") (let ((text (as-text value)))
                               (values text (and text t))))
    ((spec-is spec "INTEGER") (as-integer value spec))
    ((spec-is spec "NUMBER") (as-number value))
    ((spec-is spec "BOOLEAN") (as-boolean value))
    ((spec-is spec "MEMBER") (as-member value (rest spec)))
    ((spec-is spec "OR") (if (null value)
                             (values nil t)
                             (coerce-value value (third spec))))
    ((spec-is spec "ARRAY-OF") (as-array value (second spec)))
    ((spec-is spec "MAP-OF") (as-map value (second spec)))
    ((spec-is spec "OBJECT")
     (if (and (listp value) (evenp (length value)))
         (multiple-value-bind (plist problem) (coerce-args (rest spec) value)
           (values plist (not problem)))
         (values nil nil)))
    ((spec-is spec "ANY") (values value t))))

(defun as-text (value)
  "VALUE as a string: strings pass through and symbols give their name, in
lower case. NIL is absence, never \"nil\"."
  (typecase value
    (null nil)
    (string value)
    (symbol (string-downcase (symbol-name value)))
    (t nil)))

(defun as-integer (value spec)
  (let ((number (typecase value
                  (integer value)
                  (string (ignore-errors (parse-integer value)))
                  (t nil))))
    (values number
            (and number (typep number (list* 'integer (when (consp spec)
                                                        (rest spec))))))))

(defun as-number (value)
  (let ((number (typecase value
                  (real value)
                  (string (parse-real value))
                  (t nil))))
    (values number (and number t))))

(defun parse-real (text)
  "TEXT as a real, or NIL. Read in the keyword package, with *READ-EVAL*
off, so text that is not a number interns nothing and evaluates nothing."
  (multiple-value-bind (value end)
      (let ((*read-eval* nil)
            (*package* (find-package '#:keyword)))
        (ignore-errors (read-from-string text)))
    (and (realp value) (eql end (length text)) value)))

(defun as-boolean (value)
  (cond
    ((eq value t) (values t t))
    ((null value) (values nil t))
    (t (let ((text (as-text value)))
         (cond ((equal text "true") (values t t))
               ((equal text "false") (values nil t))
               (t (values nil nil)))))))

(defun as-member (value members)
  "VALUE as one of MEMBERS, matched by name without regard to case: a model
naming an HTTP verb sends \"POST\" as readily as \"post\"."
  (let* ((text (as-text value))
         (match (and text (find text members :key #'symbol-name
                                             :test #'string-equal))))
    (values match (and match t))))

(defun as-array (value spec)
  (unless (typep value 'sequence)
    (return-from as-array (values nil nil)))
  (let ((out '()))
    (map nil (lambda (element)
               (multiple-value-bind (result ok) (coerce-value element spec)
                 (unless ok (return-from as-array (values nil nil)))
                 (push result out)))
         value)
    (values (nreverse out) t)))

(defun as-map (value spec)
  "VALUE, a plist of names and values, with every name a string. An alist
has an even length too, and must not pass as one empty entry."
  (unless (and (listp value) (evenp (length value)))
    (return-from as-map (values nil nil)))
  (let ((out '()))
    (loop for (name entry) on value by #'cddr
          for key = (as-text name)
          do (multiple-value-bind (result ok) (coerce-value entry spec)
               (unless (and key ok) (return-from as-map (values nil nil)))
               (push key out)
               (push result out)))
    (values (nreverse out) t)))

(defun describe-specifier (spec)
  "SPEC in the words a caller reading a (:bad-request ...) needs."
  (cond
    ((spec-is spec "MEMBER")
     (format nil "one of ~{~a~^, ~}"
             (mapcar (lambda (m) (string-downcase (symbol-name m))) (rest spec))))
    ((spec-is spec "OR") (format nil "~a or null" (describe-specifier (third spec))))
    ((spec-is spec "ARRAY-OF") (format nil "a list of ~a" (describe-specifier (second spec))))
    ((spec-is spec "MAP-OF") (format nil "a plist of ~a" (describe-specifier (second spec))))
    ((spec-is spec "OBJECT") "a plist")
    ((spec-is spec "ANY") "any value")
    ((and (spec-is spec "INTEGER") (consp spec))
     (format nil "an integer in ~{~a~^..~}" (rest spec)))
    (t (format nil "a ~(~a~)" (spec-head spec)))))

;;; --- JSON Schema ------------------------------------------------------

(defstruct (json-object (:constructor %make-json-object ()))
  "A JSON object jzon writes in insertion order, which a hash table does not
promise on every implementation. PAIRS is newest first."
  (pairs '()))

(defun json-object (&rest plist)
  (let ((object (%make-json-object)))
    (loop for (key value) on plist by #'cddr
          do (setf (json-get object key) value))
    object))

(defun json-get (object key)
  (cdr (assoc key (json-object-pairs object) :test #'equal)))

(defun (setf json-get) (value object key)
  "Set KEY, keeping its place if OBJECT already has it."
  (let ((pair (assoc key (json-object-pairs object) :test #'equal)))
    (if pair
        (setf (cdr pair) value)
        (push (cons key value) (json-object-pairs object)))
    value))

(defmethod json:write-value ((writer json:writer) (object json-object))
  (json:with-object writer
    (loop for (key . value) in (reverse (json-object-pairs object))
          do (json:write-key writer key)
             (json:write-value writer value))))

(defun schema->json-schema (schema)
  "SCHEMA as a JSON Schema object: a JSON-OBJECT jzon serialises directly."
  (validate-schema schema)
  (let ((properties (json-object))
        (required '()))
    (dolist (param schema)
      (setf (json-get properties (param-key param))
            (property->json (param-type param) (param-options param)))
      (when (getf (param-options param) :required)
        (push (param-key param) required)))
    (json-object "type" "object"
                 "properties" properties
                 "required" (coerce (nreverse required) 'vector)
                 "additionalProperties" nil)))

(defun property->json (spec options)
  (let ((json (specifier->json spec))
        (doc (getf options :doc))
        (condition (options-condition options)))
    (when condition
      (setf doc (format nil "~@[~a. ~]Required when ~(~a~) is ~(~{~a~^ or ~}~)."
                        (and doc (string-right-trim "." doc))
                        (car condition) (cdr condition))))
    (when doc
      (setf (json-get json "description") doc))
    (when (member :default options)
      (setf (json-get json "default") (json-value (getf options :default))))
    json))

(defun json-value (value)
  "VALUE as jzon writes it. Keywords are member values, which travel as
their lower-cased name rather than as a symbol."
  (if (keywordp value) (string-downcase (symbol-name value)) value))

(defun specifier->json (spec)
  (cond
    ((spec-is spec "STRING") (json-object "type" "string"))
    ((spec-is spec "NUMBER") (json-object "type" "number"))
    ((spec-is spec "BOOLEAN") (json-object "type" "boolean"))
    ((spec-is spec "INTEGER")
     (let ((json (json-object "type" "integer"))
           (bounds (when (consp spec) (rest spec))))
       (destructuring-bind (&optional (low '*) (high '*)) bounds
         (unless (eq low '*) (setf (json-get json "minimum") low))
         (unless (eq high '*) (setf (json-get json "maximum") high)))
       json))
    ((spec-is spec "MEMBER")
     (json-object "type" "string"
                  "enum" (map 'vector #'json-value (rest spec))))
    ((spec-is spec "OR")
     (let ((json (specifier->json (third spec))))
       (setf (json-get json "type") (vector (json-get json "type") "null"))
       json))
    ((spec-is spec "ARRAY-OF")
     (json-object "type" "array" "items" (specifier->json (second spec))))
    ((spec-is spec "MAP-OF")
     (json-object "type" "object"
                  "additionalProperties" (specifier->json (second spec))))
    ((spec-is spec "OBJECT") (schema->json-schema (rest spec)))
    ((spec-is spec "ANY") (json-object))))

(defun json-schema->schema (json)
  "A JSON Schema object, as jzon parses one, as a parameter list."
  (unless (and (hash-table-p json)
               (equal "object" (gethash "type" json))
               (hash-table-p (gethash "properties" json)))
    (error "Not a JSON Schema object: ~s." json))
  (let ((required (coerce (or (gethash "required" json) #()) 'list))
        (schema '()))
    (maphash (lambda (key property)
               (push (json-property->param key property
                                           (member key required :test #'equal))
                     schema))
             (gethash "properties" json))
    ;; By name, not by hash iteration: jzon parses properties into a hash
    ;; table, so an import ordered by it differs between implementations and
    ;; a round trip through JSON would not compare equal.
    (sort schema #'string< :key #'param-name)))

(defun json-property->param (key property required)
  (let ((name (a:make-keyword (string-upcase key)))
        (options '()))
    (when (nth-value 1 (gethash "default" property))
      (setf options (list :default (gethash "default" property))))
    (a:when-let ((doc (gethash "description" property)))
      (setf options (list* :doc doc options)))
    (when required
      (setf options (list* :required t options)))
    (list* name (json->specifier property) options)))

(defun json->specifier (property)
  (unless (hash-table-p property)
    (error "Not a JSON Schema property: ~s." property))
  (let ((type (gethash "type" property)))
    (if (and (vectorp type) (not (stringp type)))
        (let ((names (remove "null" (coerce type 'list) :test #'equal)))
          (unless (= 1 (length names))
            (error "Only a nullable single type is supported: ~s." type))
          (list 'or 'null (json-type->specifier (first names) property)))
        (json-type->specifier type property))))

(defun json-type->specifier (type property)
  (cond
    ((null type) 'any)
    ((equal type "string")
     (a:if-let ((enum (gethash "enum" property)))
       (list* 'member (map 'list (lambda (value) (a:make-keyword (string-upcase value)))
                           enum))
       'string))
    ((equal type "integer")
     (let ((low (gethash "minimum" property))
           (high (gethash "maximum" property)))
       (cond ((and (null low) (null high)) 'integer)
             ((null high) (list 'integer low))
             (t (list 'integer (or low '*) high)))))
    ((equal type "number") 'number)
    ((equal type "boolean") 'boolean)
    ((equal type "array")
     (let ((items (gethash "items" property)))
       (unless items (error "An array needs items: ~s." property))
       (list 'array-of (json->specifier items))))
    ((equal type "object")
     (let ((properties (gethash "properties" property))
           (extra (gethash "additionalProperties" property)))
       (cond ((hash-table-p properties) (cons 'object (json-schema->schema property)))
             ((hash-table-p extra) (list 'map-of (json->specifier extra)))
             (t (error "An object needs properties or additionalProperties: ~s."
                       property)))))
    (t (error "Unknown JSON Schema type ~s." type))))
