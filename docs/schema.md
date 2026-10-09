# Parameter schemas

A tool's `:params` is a typed schema. It is canonical in both directions: it
renders to the JSON Schema a model needs for tool calling, and imports from one
an external tool arrives with. One validator, one coercion path. The schema
is [cl-inference/client](https://github.com/takeiteasy/cl-inference/blob/trunk/docs/client.md#schemas)'s;
miao re-exports it, and `define-tool` checks its `:params` with it.

```lisp
:params '((:cmd     string                     :doc "command string to run")
          (:timeout (integer 1) :default 30000 :doc "kill after this many ms")
          (:op      (member :read :write :list :mkdir :delete :rmdir) :required t
                    :doc "operation to perform"))
```

Each parameter is `(:name specifier . options)`. The options are `:doc`,
`:required`, `:default` and `:required-when`; any other is a definition error. A `:default` must print and read back (a
number, keyword, string or boolean, not a function or hash table); `define-tool`
refuses one at load, and mounting any tool fails the same way.

## Conditional parameters

`:required-when` makes a parameter required for some values of another:

```lisp
:params '((:op   (member :read :write :list) :required t)
          (:data string :required-when (:op :write) :doc "file contents"))
```

The value is `(:param :value)` or `(:param (:value ...))`. `:param` must be a
`member` parameter of the same schema, and every value one of its members. A
parameter cannot also carry `:required` or `:default`.

Coercion checks it against the coerced controller, so `"WRITE"` counts, and a
controller's `:default` does too. A missing parameter is refused before the
tool runs: `(:bad-request ":data is required when :op is write")`.

It renders into the parameter's description, `file contents. Required when op
is write.`, and not into `required`, `allOf` or `if`, which not every backend
accepts.

## Vocabulary

| Specifier | JSON Schema |
|---|---|
| `string` | `{"type":"string"}` |
| `integer`, `(integer lo)`, `(integer lo hi)` | `{"type":"integer"}` with `minimum` and `maximum` |
| `number` | `{"type":"number"}` |
| `boolean` | `{"type":"boolean"}` |
| `(member :a :b)` | `{"type":"string","enum":["a","b"]}` |
| `(or null X)` | X's schema, with `"null"` added to its `type` |
| `(array-of X)` | `{"type":"array","items":X}` |
| `(object (:k X ...) ...)` | an object schema, as at the top level |
| `(map-of X)` | `{"type":"object","additionalProperties":X}` |
| `any` | `{}` — matches anything, coerces nothing |

The set is closed: a specifier outside it is a definition error, not a silent
pass-through. Specifiers are compared by symbol name, so a schema written in
any package reads the same.

`array-of`, `object`, `map-of` and `any` are defined here — CL has no
equivalent. `object` names its fields; `map-of` is for dynamic keys, such as
HTTP headers; `any` is for a value whose shape depends on something the
schema itself cannot name, such as a plan step's `:args` — see
[the plan gate](plan.md).

## Coercion

Model-supplied arguments arrive as strings whatever the declared type, and
in-image callers pass keywords. `coerce-args` takes both:

| Declared | Accepts |
|---|---|
| `string` | a string, or a symbol by its lower-cased name |
| `integer`, `number` | the number itself, or a string that parses wholly |
| `boolean` | `t`, `nil`, `"true"`, `"false"` |
| `(member ...)` | a string or symbol naming a member, whatever the case |
| `(or null X)` | `nil`, which is null rather than false, or anything X accepts |
| `(array-of X)` | a list or vector, coerced elementwise |
| `(map-of X)` | a plist whose names are strings or symbols |
| `(object ...)` | a plist, coerced as a nested schema |
| `any` | anything; passed through unchanged |

Coercion then fills in a `:default` for an absent parameter, and rejects:

- a value that will not coerce, `(:bad-request ":timeout must be an integer in 1..")`
- a missing `:required` parameter
- a parameter the schema does not declare, matching the rendered
  `additionalProperties: false`

`invoke-tool` coerces before dispatching, and `define-tool-handler` coerces
again on entry, so a tool reached by a bare `m:call` sees the same checked
arguments. A tool therefore reads its arguments with plain `getf`, in the
declared type, and carries no coercion of its own.

## JSON Schema

```lisp
(miao:schema->json-schema (miao:tool-schema (miao:describe-tool :tool-shell)))
(miao:json-schema->schema (com.inuoe.jzon:parse text))
```

`schema->json-schema` returns an insertion-ordered JSON object, which jzon
serialises directly, so a backend embeds it in a larger request body rather
than splicing strings. Properties render in declaration order, which keeps a
request body stable for prompt caching. `json-schema->schema` is its inverse;
an unrecognised construct is an error, matching the closed set.

A round trip preserves every specifier, its options and the `required` set,
except `:required-when`, which stays in the description as text. It does not
preserve parameter order: a JSON object carries none, so an import is ordered
by parameter name.
