# Self-modification

`tool-self` is the one place miao can change its own running image: evaluate
a form in the host, redefine a function or class, or reload a mounted
child. Everything else -- `tool-eval`, `tool-repl`, a worker -- runs in a
throwaway process that loads nothing and shares no state with the harness.
See [~takeiteasy/miao#12](https://todo.sr.ht/~takeiteasy/miao/12).

```lisp
(m:mount *ctx* 'miao:tool-self :enable '(:eval :define :reload))
(miao:invoke-tool :tool-self :op :eval :form "(+ 1 2)")
;; => (:ok (:value "3" :values ("3") :out "" :elided nil))
```

## `:enable`

Each write op -- `:eval`, `:define`, `:reload` -- is refused unless it is
named in `:enable`, a mount option that defaults to nil. `:log` always
answers, since it only reads back what a write already did.

```lisp
(miao:invoke-tool :tool-self :op :eval :form "1")
;; => (:error (:forbidden ":eval is not enabled"))
```

This records operator intent and gives an audit trail; it is not a
sandbox. A `:form` runs with nothing between it and `cl:eval` and can
redefine anything in the image, including `tool-trust` or `resolve-tools`
themselves. The [plan gate](plan.md) constrains what a *model* may reach
through `tool-plan`; `tool-self` is `:operator`-trusted like `tool-eval`,
and stays off the model's own allow-list the same way.

## Ops

| `:op` | Params | Answers |
|---|---|---|
| `:eval` | `:form` (required), `:package` (default `"CL-USER"`), `:timeout` | `:value`, `:values`, `:out`, `:elided` |
| `:define` | `:form` (required), `:package`, `:timeout` | `:name` |
| `:reload` | `:name` (required), `:timeout` | `:name` |
| `:log` | `:limit` (default 50) | `:entries`, `:total` |

`:eval` answers the same value shape `tool-eval` and `tool-repl` do (see
[tools](tools.md#results)): `:values` carries every value the form
returned, printed in order; `:value` is its first entry, or `"NIL"` when
there is none. `:elided` is true when a value's print limits, character
cap, or the value-list cap cut what came back.

`:form` is read as exactly one expression, with `*read-eval*` nil -- the
same guard a generation's own read applies -- against `:package`, resolved
with `find-package` only: a package that doesn't already exist is a
`(:bad-request ...)`, never created on the caller's behalf. `:define`
additionally requires the form's head to be a definition -- `defun`,
`defmacro`, `defgeneric`, `defmethod`, `defclass`, `defstruct`,
`defparameter`, `defvar`, `m:defservice` or `miao:define-tool` -- so its
checkpoint and log entry describe an actual definition; anything else is
`:eval`'s job.

`:reload` is `m:reload` on the tool's own context: the named child is
stopped, reinitialised with its own mount initargs and started again. A
reload that has begun always runs to its end: at `:timeout`, or when the
call's token is cancelled, the caller gets `:timeout` or `:cancelled` at
once, and the reload's own result is logged as a `:late-outcome` entry.
There is no `:mount` or `:unmount` here -- caller-supplied initargs would
have to be logged, and a provider's `:api-key` is exactly the kind of
initarg that must never reach disk (see
[checkpoints](checkpoints.md#credentials)). A rollback does mount a service
again, from what its generation recorded.

`:eval` and `:define` run on their own thread, interrupted at `:timeout` --
the same shape as `tool-http`'s deadline (see [tools](tools.md#workers)) --
so a wedged form costs a timeout, not a wedged service. A value is printed
under the same caps a worker applies: `*print-length*` 100, `*print-level*`
8, a 4000-character cap.

The form is evaluated with `*package*` bound to `:package`, so symbols a
macro interns while expanding -- a `defstruct`'s accessors -- land there too.

A lapsed `:timeout` interrupts pre-emptively, except while the Lisp is
mid-way through one class, method, generic-function or struct definition:
the interrupt waits for that definition to finish, then lands. A definition
that outlasts a 2 second grace period -- user code it calls, such as a MOP
method or a `defstruct` constructor macro, has wedged -- is torn: the
interrupt throws out of it and that one definition may be half applied. A
`defgeneric` redefinition removes the old initial methods first, outside
SBCL's own uninterruptible section, so a wedged `remove-method` method is
torn the same way.[^ecl] A wedged `:eql` specializer or
slow compile is killed, not leaked, whether or not the form mutated something earlier.
The caller gets `:timeout`; a form killed after landing one or more
mutations is left partially applied and logged as a `:late-outcome` entry
with `:outcome (:error :abandoned)`, or `(:error :torn)` if killed inside
a definition.

Cancelling the call's [cancel token](tools.md#cancelling-a-call) interrupts a
running `:eval` or `:define` the same way, and the caller gets `:cancelled`.

## Checkpoint and log

Every write takes a [checkpoint](checkpoints.md) first, then writes an
intent log entry naming it -- before the op runs, so a crash mid-eval still
points at what to roll back to -- and an outcome entry after:

```lisp
(:at "2026-09-23T10:00:00Z" :kind :intent :op :eval
 :form "(+ 1 2)" :label nil :checkpoint "~/.miao/generations/....generation"
 :previous-source nil)
(:at "2026-09-23T10:00:00Z" :kind :outcome :op :eval :outcome :ok)
```

A write that finishes, or is killed after mutating, after its caller
received `:timeout` adds a third entry,
`(:kind :late-outcome ... :checkpoint "..." :outcome :ok)` or
`:outcome (:error :abandoned)` or `(:error :torn)`, whose
`:checkpoint` matches its intent entry's. The log always reads intent,
`:timeout` outcome, then `:late-outcome`.

`:previous-source` is `:define`'s defined name's `symbol-source`
([introspection](introspection.md)) as it stood before the write --
a `:file`/`:position`, or the old `:form` when it was itself defined in the
image -- rollback restores declared service state, never code
([#48](https://todo.sr.ht/~takeiteasy/miao/48)), so this pointer is the
only way back to the old definition. The log is an append-only
s-expression file (`:log`, default `~/.miao/self.log`), read the same
guarded way a generation is: `*read-eval*` nil, so a log can never run code
merely by being read back. Each entry also reaches [meow's
logger](https://github.com/takeiteasy/meow/blob/trunk/docs/logger.md) when
one is mounted, at `:info` or `:warn`.

## `self-define` and `:require-image`

`:previous-source` is a manual way back, and only for one symbol.
[`self-define`](images.md) closes that for real: it takes an
[image generation](images.md) immediately before the write, so
`miao:relaunch`ing that core undoes the redefinition itself, not just
declared state.

```lisp
(miao:self-define *ctx* "(defun greet () :hi)" :package "MY-APP")
;; => :hi, #P"~/.miao/generations/....core"
```

It is a REPL entry, not a tool op: `save-image` needs the main thread, so
no agent turn can reach it, and there is no worker thread or `:timeout`
here for an operator to abandon -- interrupt it the ordinary way.
`(context form &key package label log)`; `context` is the mounted
context's process to image, matching `save-image`'s own argument.

`:require-image`, a mount option (default nil), refuses `:eval` and
`:define` once it's true unless an image has been taken and nothing has
written since:

```lisp
(m:mount *ctx* 'miao:tool-self :enable '(:eval :define) :require-image t)
(miao:invoke-tool :tool-self :op :eval :form "1")
;; => (:error (:bad-request "take an image generation first (~takeiteasy/miao#48)"))
```

With it set, `self-define` becomes the only way to still redefine
anything: every write it enables stays code-exact and undoable. Every
intent log entry, `:require-image` or not, also records `:image`, the
newest image's path at the time -- `nil` if none has been taken yet.

## Trust posture

`:operator` only. Host eval can read anything the image holds, including a
provider's `:api-key` -- unlike `tool-image`, which never returns a value
or a slot (see [introspection](introspection.md#trust-posture)), `tool-self`
*is* the way to see one, alongside `tool-eval` and `tool-repl`.

## Limitations

- On ECL a deadline between a `defstruct` being defined and its constructor
  can leave the struct without one
  ([#238](https://todo.sr.ht/~takeiteasy/miao/238)).
- Rollback restores declared service state, not code: an ordinary
  tool-self `:define`'s checkpoint does not undo the redefinition itself,
  only whatever state drifted around it. `:previous-source` is the manual
  way back; `self-define`'s image generation is the code-exact one, but
  only tracks tool-self's own writes -- code loaded any other way is not
  reflected in `:require-image`'s staleness check.
- The checkpoint taken before a write waits up to `checkpoint`'s own 30
  second `:timeout` for a busy service, independent of `:timeout`, and
  does not report which services it caught mid-run.

[^ecl]: ECL expands a `defstruct` constructor's macros before it defines the
    struct, so a wedge there is killed with nothing applied and is never
    `:torn`.
