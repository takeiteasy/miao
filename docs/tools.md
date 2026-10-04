# Tools

A tool is a [meow](https://github.com/takeiteasy/meow) service that follows one
extra convention, so any caller — including the agent loop — can discover,
describe and invoke every tool the same way.

## The convention

A tool registers under `:tool-<name>`, and its `metadata` plist carries
`:kind :tool`, a `:summary`, its `:params` and its `:trust` level.
`define-tool` declares all of this in one form:

```lisp
(define-tool :tool-shell
    (:trust :operator
     :summary "Run a shell command (sh -c) and capture merged output"
     :params ((:cmd string :required t :doc "command string to run")
              (:timeout (integer 1) :default +default-tool-timeout+
               :doc "kill the command after this many milliseconds")))
  (:invoke (cmd timeout)
    (run-command cmd timeout cancel-token)))
```

`NAME` is given once, as the leading keyword, and is used for the class, the
registration and the metadata — it cannot drift between them the way it could
when `defservice`, a `metadata` method and a handler were three separate forms
each naming the tool.

`:params` is a typed [schema](schema.md), checked at macroexpansion: a bad
specifier is a compile-time error. It drives coercion and validation, so a
tool's `:invoke` clause reads its arguments already coerced — `(:invoke (cmd
timeout) ...)` binds `cmd` and `timeout` from the plist, in their declared
types — and it renders to the JSON Schema a model needs for tool calling.
`tool-schema` reads it out of the metadata. A `:default` must print and read
back, since a call carries the schema into a [checkpoint](checkpoints.md)
generation: `define-tool` signals at load for a function or hash table, and
mounting any tool that defines its handler with `define-tool-handler` fails
the same way.
`:slots` passes extra slots
through to the generated class, as `tool-fs`'s sandbox root does. Inside
`:invoke`, the anaphoric `service` is the tool itself and `cancel-token` is
the call's [cancel token](#cancelling-a-call), or nil.

`:trust` is `:operator` for a tool only a trusted operator may reach, and
`:agent` for one a model may call. `tool-trust` reads it, and answers `:agent`
for metadata that names none. The [agent loop](agent.md)'s default allow-list
is exactly the `:agent`-trusted tools — `tool-fs`, `tool-plan`,
`tool-gated-eval`, `tool-image` and `tool-services` are the standard tools at
that level, so granting the
others to a model is explicit at the mount site.

`:background t` marks a tool whose calls an [agent](agent.md#detached-tool-calls)
detaches as soon as it dispatches them, so the turn never waits on one. It is
an option of `define-tool` and a key of the metadata; `tool-metadata` reads it.

`:resumable t` marks a tool whose calls are safe to run twice, so the
[call log](calls.md#resuming-a-call) may resume one a crash cut short. It is an option of `define-tool` and a key of the metadata. Only
`tool-image` and `tool-services` declare it; every op of a tool must be safe to
repeat.

A tool needing another `handle` clause beyond `:describe` and `:invoke` falls
back to `defservice` and the lower-level `define-tool-handler` directly. It
answers two messages either way:

- `(:describe)` — replies with the metadata plist
- `(:invoke . plist)` — coerces the plist against the schema, then performs the
  operation

Every tool also answers `(:snapshot)` and `(:restore state)`, backed by the
`snapshot`/`restore` generic functions a tool may specialise; both default to
nil. See [checkpoints](checkpoints.md).

Meow intercepts the heads `%update-config`, `%effects` and `%timer-fire` before
`handle`, so a tool must not use them.

## Results

```lisp
(:ok plist)       ; the tool's own return values
(:error reason)   ; one vocabulary across every tool and adapter
```

`reason` is one of:

| Reason | Meaning |
|---|---|
| `(:bad-request msg)` | The invoke arguments are malformed, or will not coerce. |
| `:timeout` | The deadline lapsed. |
| `:cancelled` | The call's cancel token was cancelled. |
| `:unavailable` | The far end could not be reached. |
| `(:error detail)` | Anything else. |
| `(:forbidden msg)` | `tool-fs`: the path escapes the sandbox. `tool-self`: the op is not in `:enable`. |

`tool-error-p` and `tool-error` take a result apart.

`invoke-tool` folds a call that fails below the tool itself -- its process
already exited, or the call would have deadlocked -- into the same
vocabulary: `:timeout`, `:unavailable` for the tool's process being gone, and
`(:error detail)` for anything else, `detail` a printed string.

## Discovery

`(tools)` scans registration props for `:kind :tool`. Props are a snapshot taken
at registration — there is no setter — so a tool's advertised `:params` change
only when it is reloaded. `invoke-tool` reads the schema from there, and
`tool-metadata` the whole plist, neither costing a message to the tool, so a
tool busy with a long call never holds them up. `describe-tool` asks the tool
itself.

```lisp
(miao:tools)  ; => (:tool-fs :tool-http :tool-shell)
```

## Definitions

`define-tool`, `define-protocol` and [`define-provider`](providers.md) record
each definition by name, so a front end can mount one without a hand-kept list:

```lisp
(miao:definitions :kind :provider)              ; => (:provider-ollama)
(miao:ensure-mounted ctx :provider-ollama :model "llama3.2")
```

`ensure-mounted` mounts a provider's protocol first, and does nothing for a
name already registered. The initargs apply to the named service only. It
signals for a name nothing defines.

## Invocation

```lisp
(miao:describe-tool :tool-shell)
(miao:invoke-tool :tool-shell :cmd "ls -la")
(miao:schema->json-schema (miao:tool-schema (miao:describe-tool :tool-shell)))
```

`invoke-tool` coerces its arguments against the tool's schema first, so a
model-supplied `:timeout "15000"` bounds the wait exactly as `15000` does.

`invoke-tool` waits longer than the tool's own `:timeout`, so the tool's bounded
`(:error :timeout)` is what a caller sees. Calling a tool with a bare `m:call`
instead would abort the *caller* after meow's 5-second default while the tool
kept running.

## Cancelling a call

`:cancel` is a reserved argument: a cancel token, as
[`complete`](protocols.md#cancelling) takes, handed to the tool rather than
coerced against its schema.

```lisp
(let ((token (miao:make-cancel-token)))
  (bt:make-thread (lambda () (sleep 1) (miao:cancel token)))
  (miao:invoke-tool :tool-shell :cmd "sleep 30" :cancel token))
; => (:error :cancelled)
```

Every tool refuses a call whose token is already cancelled, without running
it -- a call queued behind another on the same tool included. Cancelling one
already running stops its work, as a lapsed `:timeout` does, in `tool-shell`
(the whole process group), `tool-http` (the connection), `tool-eval`, `tool-gated-eval` and
`tool-repl` (the worker, so that `:id` starts empty), `tool-plan` (the step
in flight, and none after it) and `tool-self` (an `:eval` or `:define`, or the wait on a `:reload`; see
[self-modification](self.md)). The other tools finish what they started.

## The standard tools

| Tool | Parameters | Notes |
|---|---|---|
| `:tool-fs` | `:op` (member), `:path`, `:data`, `:encoding` (member) | Sandboxed to the root given at mount. Ops: `read`, `write`, `list`, `mkdir`, `delete`, `rmdir`. `:encoding` is `text` (the default) or `base64`, for `read` and `write` ([below](#tool-fs-bytes)). `list` shows every entry, symlinks included; mount with `:hide-links t` to leave symlinks out. |
| `:tool-shell` | `:cmd`, `:timeout` | Runs via `sh -c` in its own process group; merged stdout and stderr, plus the exit status. |
| `:tool-http` | `:url`, `:method` (member), `:headers` (map), `:body`, `:timeout` | Single request. Redirects are not followed and statuses pass through. The request body is sent as UTF-8; the body comes back as text or base64, named by `:body-encoding` ([below](#tool-http-text)). |
| `:tool-eval` | `:form`, `:timeout` | Evaluates one form in a [worker](#workers) started for it and killed after it. |
| `:tool-gated-eval` | `:form`, `:timeout` | Evaluates one form that passed an allowlist, in a single-use worker with a heap cap. `:agent`-trusted. See [the allowlist gate](gate.md). |
| `:tool-repl` | `:id`, `:form`, `:pristine`, `:timeout` | One session per `:id`, started on first use, so state threads through successive forms. `:pristine` restarts its worker. Sessions run concurrently with each other. An id idle past the mount's `:idle` (600 s by default) is dropped. |
| `:tool-plan` | `:steps`, `:timeout` | Runs a checked sequence of declared tool calls. See [the plan gate](plan.md). |
| `:tool-image` | `:op`, `:symbol`, `:package`, `:pattern`, `:external-only`, `:limit`, `:doc-type` | Read-only introspection over the live Lisp image: `describe`, `apropos`, `documentation`, `source`, `packages`. See [introspection](introspection.md). |
| `:tool-services` | `:op`, `:kind`, `:recursive`, `:name` | Read-only introspection over the meow supervision tree: `registry`, `children`, `describe`. See [introspection](introspection.md). |
| `:tool-checkpoint` | `:op`, `:label`, `:keep`, `:path` | Save, list and roll back generations of the harness's declared state. See [checkpoints](checkpoints.md). |
| `:tool-self` | `:op`, `:form`, `:package`, `:name`, `:label`, `:limit`, `:timeout` | Evaluate, redefine and reload in the host image, each write gated by `:enable`, checkpointed and logged. See [self-modification](self.md). |
| `:tool-vault` | `:op`, `:id`, `:agent`, `:status`, `:limit`, `:max-age` | List, restore, discard and compact entries in the steering message vault. See [the vault](vault.md). |
| `:tool-calls` | `:op`, `:ids`, `:agent`, `:force`, `:status`, `:limit`, `:max-age` | List and compact the tool call log, and resume calls it holds as lost. `:operator` trust. See [the call log](calls.md#tool-calls). |

`:timeout` is in milliseconds and defaults to 30000. `tool-fs`, `tool-image`
and `tool-services` bound no work of their own, so they declare no `:timeout`
and refuse one. Each tool's exact types are in its `:params`; see
[schemas](schema.md) for the vocabulary.

```lisp
(m:mount context 'miao:tool-fs :root "/srv/workspace")
(m:mount context 'miao:tool-shell)
(m:mount context 'miao:tool-http)
(m:mount context 'miao:tool-eval)
(m:mount context 'miao:tool-repl)
(m:mount context 'miao:tool-plan :allow '(:tool-fs))
(m:mount context 'miao:tool-image)
(m:mount context 'miao:tool-services)
(m:mount context 'miao:tool-checkpoint)
(m:mount context 'miao:tool-self :enable '(:eval :define :reload))
(m:mount context 'miao:tool-vault)
(m:mount context 'miao:tool-calls)
```

`tool-fs` `delete` refuses directories, and there is no recursive delete: a tool
this easy to call should not be able to `rm -rf`. `rmdir` removes a directory
only when it is empty, and refuses a file, a symlink and the sandbox root.

`tool-eval` and `tool-repl` answer `(:ok (:value "<printed first value>"
:values ("<printed value>" ...) :out "<what the form printed>" :elided
<bool>))`. `:values` holds every value the form returned, printed in order
and empty for a form returning none; `:value` is `:values`'s first entry, or
`"NIL"` when there is none, kept for callers that only want the primary
value. `:elided` is true when the worker's print limits, character cap, or
value-list cap cut what came back; see [Workers](#workers) for getting past
it. Source that does not read is a
`(:bad-request ...)`, a form that signals is an `(:error detail)`, and a worker
that missed its deadline is killed: `tool-eval` starts a fresh one next call,
and a `tool-repl` id starts empty again. A session inherited through a
relaunched [image](images.md) is reported lost once, then starts empty.

Each `tool-repl` id runs on its own session, mounted under the tool's
context on first use, so each id evaluates independently of the others and
of the tool's own `:describe`. Calls on one id still run in order, since a
session's own mailbox serialises them.

An id with no eval in flight for `:idle` seconds (a `tool-repl` mount
option, 600 by default; `nil` keeps every session until the tool itself
stops) is dropped, killing its worker the same way an unmounted `tool-repl`
does. An eval still running past `:idle` keeps its session; the check is
against time since the last one finished, not time since the session
started.

<a id="tool-fs-bytes"></a>
`tool-fs` `:encoding` `"base64"` reads and writes a file's bytes as base64 in
`:data`. Invalid base64 is a `:bad-request` and leaves the file untouched. A `text`
read of a file that is not valid UTF-8 is a `:bad-request` too; read it as
`base64`.
Saving a `tool-http` body as it came:

```lisp
(let ((r (second (invoke-tool :tool-http :url "https://example.com/a.png"))))
  (invoke-tool :tool-fs :op :write :path "a.png"
               :data (getf r :body) :encoding (getf r :body-encoding)))
```

<a id="tool-http-text"></a>
`tool-http` answers `:body` with `:body-encoding` `"text"` or `"base64"`:


| `Content-Type` | `:body` | `:body-encoding` |
|---|---|---|
| `text/*`, JSON, XML, JavaScript (also `+json`, `+xml`), form data | decoded in the declared charset, else UTF-8, else Latin-1 | `"text"` |
| none, bytes valid UTF-8 | decoded as UTF-8 | `"text"` |
| none, bytes not valid UTF-8 | base64 | `"base64"` |
| anything else (`image/png`, `application/zip`, ...) | base64 | `"base64"` |

An empty body is `""`, `"text"`.
`tool-http` folds a caller-supplied `Content-Type` into drakma's own argument,
so it is sent once, as asked, rather than duplicated or overridden.

`tool-http` opens its own connection and hands drakma the wrapped stream it
expects for `:stream` — drakma's own `:connection-timeout` does not bound the
whole exchange, and it cannot close a connection it opened internally.
`:timeout` bounds the connect phase too, ahead of the exchange deadline. A
deadline that lapses, or a cancel, shuts the socket down and unwinds the
exchange wherever it stalled, so the call answers instead of running until
the server answers. The exchange runs on the tool's own thread.

## Workers

`tool-eval`, `tool-gated-eval` and `tool-repl` evaluate in a worker: a separate Lisp process that
loads nothing — no Quicklisp, no meow, no miao — and runs a read/eval/print loop
over stdio. A crash or a hang there costs a deadline, never the host image.

Both sides read with `*read-eval*` bound to nil, and the worker evaluates in a
fresh `MIAO-WORKER` package. One exchange per line:

```lisp
(:eval "(+ 1 2)")            ; host to worker
(:ready)                     ; worker, once, at boot
(:ok ("3") "" nil)           ; every value, output, then :elided or nil
(:error "message" "")        ; the form signalled
(:reader-error "message")    ; the source did not read
```

The source travels as a string, so source that does not read costs one reply
rather than desynchronising the stream. Each value prints under
`*print-length*`, `*print-level*` and a character cap; the reply's fourth
element is `:elided` when any value was cut that way, when the form returned
more than 100 values (ECL cannot return more than 64), or when their combined
printed form ran past 4000 characters (later values dropped), `nil` when everything printed in full.

Output a form prints is capped at 4000 characters the same way, and sets
`:elided`. `t` and the `*-io*` streams are rebound to that captured output
around each form, so a form cannot write to the pipe the host reads replies
from. A form that exhausts the stack or heap answers `:error`.

No value is lost to elision on a `tool-repl` session: the worker keeps a REPL
history under `*`, `**` and `***` for the first value and `/`, `//` and `///`
for the whole list, shifted the same way the standard toplevel's are after
each successful eval (an erroring form leaves them alone). The session's
history survives across calls on that id, so an elided value can still be
inspected:

```lisp
(invoke-tool :tool-repl :id "a" :form "(make-list 500)")
;; => (:ok (:value "(NIL NIL NIL ...)" :values ("(NIL NIL NIL ...)") :out "" :elided t)
(invoke-tool :tool-repl :id "a" :form "(defparameter v *)")
(invoke-tool :tool-repl :id "a" :form "(length v)")
;; => (:ok (:value "500" :values ("500") :out "" :elided nil))
(invoke-tool :tool-repl :id "a" :form "(floor 7 2)")
;; => (:ok (:value "3" :values ("3" "1") :out "" :elided nil))
(invoke-tool :tool-repl :id "a" :form "(second /)")
;; => (:ok (:value "1" :values ("1") :out "" :elided nil))
```

`tool-eval`'s worker is killed after the call, so its history is of no use
past the reply — a caller that hits `:elided` there needs `tool-repl` to look
further at the value.

A worker runs the host's own Lisp binary. `*worker-command*` overrides the
invocation. Starting one costs about 33 ms, and an exchange with a running
one about 0.2 ms, which is why an evaluation gets a fresh process instead
of a pooled one.

A worker leads its own process group, the same as a `tool-shell` command (see
the standard tools table above), so a process a form backgrounds is killed
along with it rather than outliving the deadline. Containment picks the best
mechanism the host offers, in order: a process group set natively by the
launch itself, one set by a `perl` or `setsid` wrapper, or — with none of
those available — walking and killing the descendant process tree by hand.

## Trust posture

`tool-shell` runs any command, `tool-http` makes arbitrary network requests from
the host, `tool-eval` and `tool-repl` evaluate arbitrary forms,
`tool-checkpoint` writes and reverts the harness's own declared state,
`tool-self` evaluates, redefines and reloads in the host image, and
`tool-vault` restores a steering message into a named agent's conversation.
All seven are trusted-operator surfaces, marked `:trust :operator` at the
definition site. `tool-fs` is confined to its sandbox root: a lexical check
first, so a path outside the root is rejected before anything touches the
filesystem, then an fd-based walk from the root, opening each component with
`O_NOFOLLOW` relative to its parent's directory fd — a symlink anywhere below
the root is refused outright rather than resolved, and the final component is
operated on relative to that same fd, so the check and the operation share one
file descriptor with no window between them for a swap to land in. The
process's current directory is never changed, so the walk cannot disturb, or be
disturbed by, other code in the process.
`tool-gated-eval` is `:agent`-trusted: its form passes [an allowlist](gate.md)
first, and its worker has a deadline and a heap cap.
`tool-plan` is `:agent`-trusted, but only reaches what its own `:allow` names,
and only tools that are themselves `:agent`-trusted — see
[the plan gate](plan.md) for what that buys and what it does not.

`tool-image` and `tool-services` are `:agent`-trusted and read-only: neither
ever returns a value or a slot, only flags and shapes, so a provider's
`:api-key` (kept out of published metadata; see [providers](providers.md))
cannot surface through either. `tool-image :source` does return a
function's own code, literals included. Seeing a value stays `tool-eval`, `tool-repl`
or `tool-self`'s job. See [introspection](introspection.md).

## Limitations

- `tool-services`'s `:state` is `m:children`'s restart bookkeeping, not the
  richer lifecycle `service-status` tracks
  ([#46](https://todo.sr.ht/~takeiteasy/miao/46)).
- `tool-vault`'s compaction is safe within one process only
  ([#84](https://todo.sr.ht/~takeiteasy/miao/84)). See
  [the vault](vault.md#limitations).
