# The allowlist gate

`tool-gated-eval` evaluates one Lisp form from an *untrusted* caller. The form
is checked against an allowlist first, then run in a single-use
[worker](tools.md#workers) with a deadline and a heap cap. It is `:agent`-trusted,
so a model can call it.

```lisp
(invoke-tool :tool-gated-eval
             :form "(loop for x in '(1 2 3) collect (* x x))")
; => (:ok (:value "(1 4 9)" :values ("(1 4 9)") :out "" :elided nil))

(invoke-tool :tool-gated-eval :form "(intern \"X\")")
; => (:error (:bad-request "INTERN is not an allowed symbol"))
```

A refused form answers `:bad-request` and starts no worker. State does not
survive a call; `tool-repl` has no gated form ([#18](https://github.com/communal-software/miao/issues/18)).

This gate is for untrusted input. The operator's own code is not gated, and
[`tool-plan`](plan.md) is the other gate: it limits what a plan may *call*,
where this one limits what a form may *say*.

## The rule

Every symbol in the form is one of:

| Kind | Example | Allowed when |
|---|---|---|
| keyword | `:test` | always (bar the [refused names](#refused-names)) |
| CL symbol | `car`, `loop` | it is on the allowlist |
| any other name | `foo`, `x` | always; it is a fresh symbol the worker owns |

A package prefix (`cl:car`, `sb-ext:quit`) is refused, so a form cannot name
anything outside CL. No allowlisted operator returns a symbol the form did not
contain, so nothing outside the allowlist is reachable, however the form is
built.[^invariant]

## What is allowed

| Group | Examples |
|---|---|
| Control | `let`, `flet`, `labels`, `lambda`, `if`, `cond`, `case`, `do`, `dolist`, `dotimes`, `loop`, `block`, `tagbody`, `handler-case`, `ignore-errors`, `unwind-protect` |
| Definitions | `defun`, `defparameter`, `setf`, `incf`, `push` |
| Data | lists, sequences, numbers, strings, characters, hash tables, arrays |
| Output | `princ`, `print`, `write-string`, `with-output-to-string`, `format` |

`gate.lisp`'s `*gate-allowed-names*` is the full list. Its comments record
what is left out and why.

## Reading

The gate has its own reader, so nothing is interned on the host and no reader
macro runs. It reads lists, `'x`, `#'x`, strings (escapes `\"` and `\\` only),
`#\a`, `#\Space`, `#\Newline`, `#\Tab`, `#\Return`, integers, ratios,
floats, symbols and `;` comments. Anything else is refused: `#.`, `#+`, `#-`,
`#S`, `#(`, `#|`, `|x|`, `\`, backquote, comma and dotted pairs.

Floats read as double-floats, so `1.5` prints as `1.5d0`.

Limits: 20000 characters, 100 levels of nesting, 100 characters a token, a
float exponent within ±300.

The gate then prints the form back as canonical, fully qualified text
(`(COMMON-LISP:+ 1 2)`), and the worker evaluates that. The caller's own text
is never evaluated, so the gate and the worker cannot disagree about how it
reads.

## format and error

`format` and `error` take a control string, and a control string can call any
function (`~/pkg:fn/`) or splice in another string (`~?`). So each is allowed
only when called directly with a literal string that uses neither:

```lisp
(format nil "~a and ~s" 1 2)                       ; allowed
(format nil (concatenate 'string "~" "/cl:car/"))  ; refused
(funcall #'format nil "x")                         ; refused
```

## Refused names

`symbol`, `symbols`, `present-symbol(s)` and `external-symbol(s)` are refused
as symbols and as keywords. `loop` compares its keywords by name, and these
make it walk a package.

## Resources

An allowlist bounds what a form can reach, not what it spends.

| Resource | Bound |
|---|---|
| Time | `:timeout`, then the worker is killed |
| Memory | `:heap` on the mount, in megabytes (256 by default; `nil` for the host's default) |
| Output | 4000 characters; more sets `:elided` |

```lisp
(m:mount context 'miao:tool-gated-eval :heap 512)
```

A worker that runs out of memory answers an error, or `:unavailable` if it
had to exit, and never waits in the debugger for its deadline. A
`*worker-command*` is used as given, so it ignores `:heap`.

## Limitations

- No `tool-repl` form
  ([#18](https://github.com/communal-software/miao/issues/18)).
- `format` and `error` need a literal control string; `warn`, `signal`,
  `cerror` and `assert` are refused
  ([#19](https://github.com/communal-software/miao/issues/19)).
- No `declare` or `the`
  ([#20](https://github.com/communal-software/miao/issues/20)).
- No backquote, `defmacro`, CLOS, `catch` or `throw`
  ([#21](https://github.com/communal-software/miao/issues/21)).
- No condition accessors or restarts
  ([#22](https://github.com/communal-software/miao/issues/22)).

[^invariant]: Adding an allowlist entry means asking whether it returns a
    symbol the form did not contain, as `type-of`, `class-of`, `intern` and
    the condition accessors do. `handler-case` clause types are the one place
    `error` may appear as a bare symbol. The worker also rebinds
    `*terminal-io*`, `*standard-input*`, `*query-io*`, `*debug-io*` and
    `*trace-output*` around each form, so `(princ "..." t)` cannot write a
    forged reply onto the pipe the host reads.
