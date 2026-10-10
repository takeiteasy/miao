# Checkpoints and rollback

A generation is a snapshot of every named service's own declared state and of
how each was mounted, written as one s-expression file. `checkpoint` takes
one; `rollback` puts it back, mounting again a service that has gone. A
delegated [sub-agent](agent.md#sub-agents) runs once and is left out.

```lisp
(miao:checkpoint *ctx* :label "before edit")
;; => #P"~/.miao/generations/20260922-171610-129774-482.generation"

(miao:generations)
;; => ((:path #P"..." :created "2026-09-22T17:16:10Z" :label "before edit"
;;      :services (:tool-fs :assistant)) ...)

(miao:rollback *ctx* "~/.miao/generations/20260922-171610-129774-482.generation"
               :timeout 30)
;; => (:ok (:restored (:tool-fs :assistant) :failed nil :failures nil :interrupted nil
;;          :unavailable nil :remounted nil :unremounted nil :missing nil
;;          :mismatched nil :extra nil))
```

`checkpoint` returns the path, then the names of the services snapshotted
mid-work and of those that could not be snapshotted. Every service is asked
at once and given `:timeout` seconds (default 30), so one busy service does
not delay the rest.

Declared-state generations by default; a generation can also carry the
image itself -- see [image generations](images.md).

## `snapshot` and `restore`

Every tool, the agent and a provider answer two messages, `(:snapshot)` and
`(:restore state)`, backed by a generic function each service may specialise:

```lisp
(defgeneric snapshot (service))   ; -> a plain value, or nil
(defgeneric restore (service state))
```

Both default to `nil`: most services hold nothing worth carrying across a
restart. The agent is the one service with a method today — it keeps
`:messages` and `:turns`. While a run is in progress it also reports
`:in-flight (:turn n :tool-calls (ids...))`, plus `:detached (ids...)` while a
[detached call](agent.md#detached-tool-calls) runs on, and `:journal-ids
(ids...)` for the calls with no result when the agent has a
[call log](calls.md). Only the ids are kept: the turn and tool calls
reference processes a restore cannot bring back, so a restore does not retry them,
and a detached call's result never reaches a restored agent. The
`:journal-ids` can be [resumed](calls.md#resuming-a-call) to run those calls
again. A tool call with no result yet is recorded as an
`{"error":"interrupted"}` `:tool` message, so the saved conversation can be
sent to a provider as it is; results that had arrived are kept. `restore`
always lands a not-running agent; `(:run :continue t)` carries on from the
restored conversation.

[Forking](forking.md) builds on the same snapshot: it restores a prefix of
one agent's `:messages` onto a new one.

Any service whose state is a plist with a non-nil `:in-flight` is reported
as interrupted.

## The generation file

```lisp
(:miao-generation 2
 :created "2026-09-22T17:16:10Z"
 :label "before edit"
 :services ((:name :tool-fs :class "tool-fs"
             :parent nil :package "MIAO" :symbol "TOOL-FS"
             :restart :transient :shutdown 5 :backoff nil :backoff-max nil
             :initargs "(:root \"/work/\")" :withheld nil
             :state nil)
            (:name :assistant :class "agent" ...
             :state (:messages (...) :turns 3))
            (:name :slow :class "slow-thing" ... :unavailable :timeout)))
```

A service that does not answer within the deadline, or exits or deadlocks
first, is recorded `:unavailable` with the reason and no `:state`.

Each entry also records how it was mounted: the context it was under
(`:parent`, nil at the root), its class by package and symbol name, its mount
options, and its `:initargs` as text[^text]. Services are listed parent first.

Read with `*read-eval*` bound to `nil` — the same guard `tool-eval`'s worker
applies to a submitted form — so a generation can never run code merely by
being read back in. Written through a temporary file and renamed in, so a
torn write never replaces a good one. A list the state shares, such as the tool
schema each call in an agent's history carries, is written once and read back
shared.

## Credentials

A generation never holds a credential. A class names its own with
`secret-initargs`, which a provider answers with `(:api-key)`:

```lisp
(defmethod miao:secret-initargs ((service my-service)) '(:token))
```

Left out of `:initargs`, and named in `:withheld`:

| Left out | Shown in `:withheld` as |
|---|---|
| a key `secret-initargs` names | the key, `:api-key` |
| a value that does not print and read back, such as an agent's `:sink` | the key, `:sink` |
| either of those inside a context's `:children` specs | `"backend-service :api-key"` |
| every initarg of a class that cannot be asked | its keys |

A service mounted again without its key takes it from wherever it would
have: a provider from its environment variable. `rollback`'s `:initargs`
gives it back explicitly.

## Rollback and drift

`rollback` mounts again each service the generation names that is not
mounted now, parent first, then restores state onto every service mounted.
Every restore is sent at once and given `:timeout` seconds (default 30), so
one busy service does not delay the rest. Drift since the checkpoint is
reported rather than silently accepted:

| Key | Meaning |
|---|---|
| `:restored` | names whose state was applied |
| `:failed` | names whose restore got no answer |
| `:failures` | each failed name with its reason: `(name :timeout)`, `(name :down)`, `(name :error)` or `(name :deadlock)` |
| `:interrupted` | restored names that were snapshotted mid-work; the in-flight work is gone |
| `:unavailable` | names the checkpoint could not snapshot — left as they are |
| `:remounted` | names mounted again from the generation |
| `:updated` | declared children of a remounted context that `:initargs` was applied to |
| `:unremounted` | each name that could not be mounted again or updated, with why: `(name "its class NO-PKG::X is not defined")` |
| `:missing` | a generation entry with no service mounted under that name now, `:unremounted` ones and those of a version 1 generation included |
| `:mismatched` | mounted now, but under a different class — not restored |
| `:extra` | mounted now, not named by the generation |

```lisp
(miao:rollback *ctx* path :remount nil)                 ; restore what is mounted, nothing more
(miao:rollback *ctx* path
               :initargs '((:example :api-key "sk-...")))
```

`:remount nil` restores onto what is mounted now. `:initargs` is an alist of
`(name . initargs)`, put ahead of a remounted service's own.

A context mounted again mounts its declared `:children` itself, so those are
not mounted a second time; only what was mounted onto it by hand is. Such a
child comes back without a credential the generation left out. `:initargs`
naming one is applied to it with
[`m:update`](https://github.com/communal-software/meow/blob/trunk/docs/update.md),
which reloads it, and it is listed under `:updated`. A service that was never
gone is not updated.
A version 1 generation records no mount, so a service it names that has gone
is `:missing`.

## `tool-checkpoint`

`:trust :operator`: writing and reverting the harness's own state is not
something the default `:agent` trust level should reach.

| `:op` | Params | Answers |
|---|---|---|
| `:save` | `:label`, `:keep` | `:path`, `:interrupted`, `:unavailable` |
| `:list` | — | `:generations` (each with `:interrupted`, `:unavailable`) |
| `:restore` | `:path` (required) | as `rollback` |

```lisp
(m:mount *ctx* 'miao:tool-checkpoint)
(miao:invoke-tool :tool-checkpoint :op :save :label "before edit")
```

`:save` runs inside the tool's own process, which cannot answer its own
snapshot, so `:tool-checkpoint` is always listed under `:unavailable`.

`:dir` is a mount option (default `*generations-directory*`,
`~/.miao/generations/`), read once at mount time — a caller wanting a
different directory per call goes through `checkpoint`/`rollback` directly
instead, as [`tool-self`](self.md) does before every write it makes.

[^text]: Printed as text so that a generation reads back even when a package
    it names is gone: that entry alone is `:unremounted`, when it is
    remounted, and the rest of the file is unaffected.
