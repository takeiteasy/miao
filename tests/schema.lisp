(in-package #:miao/tests)
(in-suite :miao)

;;; The parameter schema: the closed vocabulary, coercion of the arguments a
;;; model supplies, and the JSON Schema rendering both directions.

(defun coerced (schema args)
  (miao:coerce-args schema args))

(defun problem (schema args)
  (nth-value 1 (miao:coerce-args schema args)))

(defun json (schema)
  (com.inuoe.jzon:stringify (miao:schema->json-schema schema)))

(defun same-json (json text)
  "JSON, a rendered schema, against TEXT. Compared structurally: a JSON
object carries no key order."
  (labels ((same (a b)
             (cond
               ((hash-table-p a)
                (and (hash-table-p b)
                     (= (hash-table-count a) (hash-table-count b))
                     (block compare
                       (maphash (lambda (key value)
                                  (multiple-value-bind (other found)
                                      (gethash key b)
                                    (unless (and found (same value other))
                                      (return-from compare nil))))
                                a)
                       t)))
               ((and (vectorp a) (not (stringp a)))
                (and (vectorp b) (not (stringp b)) (= (length a) (length b))
                     (every #'same a b)))
               (t (equal a b)))))
    (same json (com.inuoe.jzon:parse text))))

(defparameter +every-specifier+
  `((:name string :required t :doc "a name")
    (:size (integer 1 100) :doc "how many" :default 10)
    (:ratio number)
    (:loud boolean :default nil)
    (:op (member :read :write) :required t)
    (:note (or null string))
    (:tags (miao:array-of string))
    (:headers (miao:map-of string))
    (:where (miao:object (:city string :required t) (:zip string)))
    (:extra miao:any :doc "anything at all")))

;;; --- coercion ---------------------------------------------------------

(test coercion-accepts-strings-symbols-and-keywords
  (is (equal '(:cmd "ls") (coerced '((:cmd string)) '(:cmd "ls"))))
  (is (equal '(:cmd "ls") (coerced '((:cmd string)) '(:cmd :ls))))
  (is (equal '(:n 30000) (coerced '((:n integer)) '(:n "30000"))))
  (is (equal '(:n 1.5d0) (coerced '((:n number)) '(:n "1.5d0"))))
  (is (equal '(:on t) (coerced '((:on boolean)) '(:on "true"))))
  (is (equal '(:on nil) (coerced '((:on boolean)) '(:on "false"))))
  (is (equal '(:op :write) (coerced '((:op (member :read :write))) '(:op "write"))))
  (is (equal '(:op :write) (coerced '((:op (member :read :write))) '(:op :write)))))

(test coercion-rejects-what-will-not-coerce
  (is (search ":cmd" (problem '((:cmd string)) '(:cmd 7))))
  (is (problem '((:n integer)) '(:n "ten")))
  (is (problem '((:n (integer 1 100)) ) '(:n 0)))
  (is (problem '((:n number)) '(:n "1 2")))
  (is (problem '((:on boolean)) '(:on "maybe")))
  (is (problem '((:op (member :read :write))) '(:op "delete"))))

(test required-missing-and-defaults-filled
  (is (search ":cmd" (problem '((:cmd string :required t)) '())))
  (is (equal '(:n 30000) (coerced '((:n integer :default 30000)) '())))
  ;; An explicit value wins over the default, including NIL.
  (is (equal '(:on nil) (coerced '((:on boolean :default t)) '(:on nil))))
  ;; A parameter with neither is simply absent.
  (is (equal '() (coerced '((:note string)) '()))))

;;; --- :required-when -----------------------------------------------------

(defparameter +conditional+
  '((:op (member :read :write :list) :default :list)
    (:data string :required-when (:op :write))
    (:key string :required-when (:op (:read :write)))))

(test required-when-holds-only-for-the-named-values
  (is (equal '(:op :list) (coerced +conditional+ '())))
  (is (equal '(:op :read :key "k") (coerced +conditional+ '(:op :read :key "k"))))
  (is (search ":data is required when :op is write"
              (problem +conditional+ '(:op :write :key "k"))))
  (is (search ":key is required when :op is read or write"
              (problem +conditional+ '(:op :read))))
  (is (equal '(:op :write :data "d" :key "k")
             (coerced +conditional+ '(:op :write :data "d" :key "k")))))

(test required-when-reads-the-coerced-controller
  (is (search ":data" (problem +conditional+ '(:op "WRITE" :key "k"))))
  ;; The controller may follow the parameter it governs, and its default counts.
  (is (search ":data" (problem '((:data string :required-when (:op :write))
                                 (:op (member :read :write) :default :write))
                               '()))))

(test required-when-applies-inside-a-nested-object
  (let ((schema `((:where (miao:object ,@+conditional+)))))
    (is (search ":where" (problem schema '(:where (:op :write :key "k")))))
    (is (coerced schema '(:where (:op :write :data "d" :key "k"))))))

(test a-malformed-required-when-is-a-definition-error
  (dolist (schema '(((:d string :required-when (:op :write)))
                    ((:op string) (:d string :required-when (:op :write)))
                    ((:op (member :read :write)) (:d string :required-when (:op :delete)))
                    ((:op (member :read :write)) (:d string :required-when (:op)))
                    ((:op (member :read :write)) (:d string :required-when (:op ())))
                    ((:op (member :read :write)) (:d string :required-when :write))
                    ((:op (member :read :write))
                     (:d string :required t :required-when (:op :write)))
                    ((:op (member :read :write))
                     (:d string :default "x" :required-when (:op :write)))
                    ((:op (member :read :write) :required-when (:op :write)))))
    (signals error (miao:validate-schema schema))))

(test required-when-renders-as-prose
  (let ((properties (gethash "properties" (miao:schema->json-schema
                                           '((:op (member :read :write))
                                             (:data string :required-when (:op :write)
                                              :doc "file contents")
                                             (:key string :required-when (:op (:read :write)))))))
        (required (gethash "required" (miao:schema->json-schema
                                       '((:op (member :read :write))
                                         (:data string :required-when (:op :write)))))))
    (is (equal "file contents. Required when op is write."
               (gethash "description" (gethash "data" properties))))
    (is (equal "Required when op is read or write."
               (gethash "description" (gethash "key" properties))))
    (is (zerop (length required)))))

(test unknown-parameter-is-rejected
  (is (search ":colour" (problem '((:cmd string)) '(:cmd "ls" :colour t)))))

(test compound-specifiers-coerce-elementwise
  (is (equal '(:tags ("a" "b"))
             (coerced '((:tags (miao:array-of string))) '(:tags (:a "b")))))
  (is (problem '((:tags (miao:array-of integer))) '(:tags ("x"))))
  (is (equal '(:headers ("x-tag" "abc"))
             (coerced '((:headers (miao:map-of string))) '(:headers (:x-tag "abc")))))
  ;; An alist has an even length too; it must not pass as one empty entry.
  (is (problem '((:headers (miao:map-of string)))
               '(:headers (("X-Tag" . "a") ("Y" . "b")))))
  (is (equal '(:where (:city "berlin"))
             (coerced '((:where (miao:object (:city string :required t))))
                      '(:where (:city "berlin")))))
  (is (problem '((:where (miao:object (:city string :required t))))
               '(:where ()))))

(test null-is-allowed-only-where-declared
  (is (equal '(:note nil) (coerced '((:note (or null string))) '(:note nil))))
  (is (equal '(:note "hi") (coerced '((:note (or null string))) '(:note "hi"))))
  (is (problem '((:note string)) '(:note nil))))

(test a-specifier-outside-the-set-is-a-definition-error
  (signals error (miao:validate-schema '((:x pathname))))
  (signals error (miao:validate-schema '((:x (member "read")))))
  (signals error (miao:validate-schema '((:x (or string integer)))))
  (signals error (miao:validate-schema '((:x string :colour t))))
  (signals error (miao:validate-schema '(("x" string))))
  (is (eq :ok (progn (miao:validate-schema +every-specifier+) :ok))))

;;; --- JSON Schema ------------------------------------------------------

(test rendering-produces-a-tools-array-object-schema
  (is (same-json (miao:schema->json-schema '((:cmd string :required t :doc "run it")))
                 "{\"type\":\"object\",
                   \"properties\":{\"cmd\":{\"type\":\"string\",\"description\":\"run it\"}},
                   \"required\":[\"cmd\"],
                   \"additionalProperties\":false}")))

(test rendering-covers-every-specifier
  (flet ((rendered (spec)
           (gethash "x" (gethash "properties"
                                 (miao:schema->json-schema (list (list :x spec)))))))
    (is (same-json (rendered 'string) "{\"type\":\"string\"}"))
    (is (same-json (rendered '(integer 1)) "{\"type\":\"integer\",\"minimum\":1}"))
    (is (same-json (rendered '(integer 1 100))
                   "{\"type\":\"integer\",\"minimum\":1,\"maximum\":100}"))
    (is (same-json (rendered 'number) "{\"type\":\"number\"}"))
    (is (same-json (rendered 'boolean) "{\"type\":\"boolean\"}"))
    (is (same-json (rendered '(member :read :write))
                   "{\"type\":\"string\",\"enum\":[\"read\",\"write\"]}"))
    (is (same-json (rendered '(or null string)) "{\"type\":[\"string\",\"null\"]}"))
    (is (same-json (rendered '(miao:array-of string))
                   "{\"type\":\"array\",\"items\":{\"type\":\"string\"}}"))
    (is (same-json (rendered '(miao:map-of string))
                   "{\"type\":\"object\",\"additionalProperties\":{\"type\":\"string\"}}"))
    (is (same-json (rendered '(miao:object (:city string :required t)))
                   "{\"type\":\"object\",
                     \"properties\":{\"city\":{\"type\":\"string\"}},
                     \"required\":[\"city\"],
                     \"additionalProperties\":false}"))
    (is (same-json (rendered 'miao:any) "{}"))))

#+sbcl ; hash-table iteration order (#230)
(test rendering-keeps-declaration-order
  "Property order follows declaration, not alphabetical or hash order --
SBCL's hash tables iterate in insertion order, so a rendered schema renders
the same way on every run."
  (is (string= (json '((:zeta string :doc "last letter" :default "z")
                        (:alpha (miao:object (:yak integer :required t) (:bee boolean)))
                        (:mid (integer 1 9))))
               "{\"type\":\"object\",\"properties\":{\"zeta\":{\"type\":\"string\",\"description\":\"last letter\",\"default\":\"z\"},\"alpha\":{\"type\":\"object\",\"properties\":{\"yak\":{\"type\":\"integer\"},\"bee\":{\"type\":\"boolean\"}},\"required\":[\"yak\"],\"additionalProperties\":false},\"mid\":{\"type\":\"integer\",\"minimum\":1,\"maximum\":9}},\"required\":[],\"additionalProperties\":false}")))

(test any-coerces-whatever-it-is-given
  (is (equal '(:x 7) (coerced '((:x miao:any)) '(:x 7))))
  (is (equal '(:x "s") (coerced '((:x miao:any)) '(:x "s"))))
  (is (equal '(:x (:a 1)) (coerced '((:x miao:any)) '(:x (:a 1))))))

(defun sorted-schema (schema)
  "SCHEMA by parameter name: a JSON object carries no order to preserve."
  (sort (copy-list schema) #'string< :key #'first))

(test a-schema-survives-a-round-trip-through-json
  (let ((back (miao:json-schema->schema
               (com.inuoe.jzon:parse (json +every-specifier+)))))
    (is (equal (sorted-schema +every-specifier+) (sorted-schema back)))))

(test json-schema-survives-a-round-trip-through-a-schema
  (let ((source (json +every-specifier+)))
    (is (same-json (miao:schema->json-schema
                    (miao:json-schema->schema (com.inuoe.jzon:parse source)))
                   source))))

(test importing-an-unknown-construct-is-an-error
  (signals error (miao:json-schema->schema
                  (com.inuoe.jzon:parse "{\"type\":\"object\",\"properties\":{\"x\":{\"type\":\"date\"}}}")))
  (signals error (miao:json-schema->schema
                  (com.inuoe.jzon:parse "{\"type\":\"object\",\"properties\":{\"x\":{\"type\":\"array\"}}}")))
  (signals error (miao:json-schema->schema (com.inuoe.jzon:parse "{\"type\":\"string\"}"))))
