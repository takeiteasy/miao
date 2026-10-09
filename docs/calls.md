# Call records

Every tool call an agent dispatches is recorded in the [journal](journal.md), so
its status outlives the agent that dispatched it. Off by default.

```lisp
(m:mount *ctx* 'miao:agent :name :assistant :model :ollama
                           :tools '(:tool-shell) :journal t)

(miao:call-entries (merge-pathnames ".miao/calls.log" (user-homedir-pathname)))
;; => ((:id "20260925-101500-123456-0" :agent :assistant :call-id "c1"
;;      :name :tool-shell :arguments "{\"command\":\"ls\"}" :turn 1
;;      :status :ok :content "{\"stdout\":\"...\"}" ...))
```

A call that ended `:lost`, `:abandoned` or `:interrupted` can be
[resumed](#resuming-a-call): run again, as a new call.

## `:journal`, the agent mount option

The calls go to the file the [`:journal`](journal.md#journal) option names. A
delegated [sub-agent](agent.md) inherits its parent's, so its calls land in the
same file. A [fork](forking.md) is mounted as its source was.

## Statuses

| `:status` | Meaning |
|---|---|
| `:accepted` | dispatched, waiting for a slot under `:max-parallel-tools` |
| `:running` | handed to its tool, or [detached](agent.md#detached-tool-calls) and still running, until its result lands |
| `:ok` | answered |
| `:error` | the tool failed, or the call was outside the allow-list |
| `:denied` | a [hook](hooks.md) denied the call before it ran |
| `:interrupted` | closed unanswered by `:cancel`, `:deadline`, an interrupting `:steer`, or a [restore](checkpoints.md) |
| `:abandoned` | the agent's process exited with the call outstanding |
| `:lost` | accepted or running, and the process that dispatched it is gone[^liveness] |

A call keeps the first outcome written for it.

## The log

```lisp
(:kind :call    :id "..." :at "iso" :agent :assistant :call-id "c1"
 :name :tool-shell :arguments "<json>" :turn 1 :by (:pid 4242 ...)
 :cut t :resumes "<id>" :raw-arguments "<json>")
(:kind :running :id "..." :at "iso")
(:kind :done    :id "..." :at "iso" :outcome :ok :content "<json>" :raw-content "<json>")
(:kind :input   :id "..." :at "iso" :agent :assistant :input-id "k"
 :digest "<md5>" :by (:pid 4242 ...))
```

An `:input` is a `:run` keyed with an `:input-id`, [recorded to spot a
redelivery](inputs.md). A `:done` entry finishes it as it does a call.
`call-entries` leaves it out.

`:cut t` is on a `:call` whose `:arguments` were cut to the size below, and
`:resumes` on one that [runs another again](#resuming-a-call).

A call's `:arguments` and a `:done` entry's `:content` are what the
[hooks](hooks.md#what-is-recorded) left. With `:log-raw t` on the agent, a
`:call` also holds `:raw-arguments` and a `:done` holds `:raw-content`, the
model's arguments and the tool's result, where a hook changed them.

`:id` is the log's own, one per dispatch. `:call-id` is the provider's, which
a provider may reuse on a later turn. `:arguments` and `:content` are JSON
text, cut to `:max-tool-result` characters or, with none set,
`*journal-max-content*` (16 KiB). The log is read with `*read-eval*` nil and
appended under the same lock as the [vault](vault.md#the-log).

## API

| Function | Answers |
|---|---|
| `(call-entries path)` | each call, oldest first, with its `:status`, `:done-at`, `:content`, `:cut`, `:resumes` and `:resumed-by` |
| `(input-entries path)` | each keyed `:run`, oldest first, with its `:status` and `:done-at` |
| `(journal-compact path :max-age s)` | the calls dropped and kept |

`journal-compact` drops finished calls older than `max-age` seconds (default
`*journal-max-age*`, 7 days; `0` drops every finished one) with their lines, and
[folds the old conversation entries](journal.md#compacting). Calls not finished
are kept. A log with a malformed entry is left as it is and `journal-compact`
answers nil. The journal's [writer](journal.md#writing) also compacts once the
file passes `*journal-compact-size*` (1 MiB) and has doubled since the last
attempt.

## Resuming a call

A call the log holds as `:lost`, `:abandoned` or `:interrupted` is run again on
request. The process that ran it is gone, so nothing reattaches: the logged
tool and arguments are sent again as a new call.

```lisp
(m:call (m:lookup :assistant) '(:resume :ids ("20260925-101500-123456-0")))
;; => (:ok (:resumed (("20260925-101500-123456-0" . "20260925-103000-654321-0"))
;;          :refused nil))

(m:call (m:lookup :assistant)
        '(:run :continue t :resume ("...") :messages ((:role :user :content "go on"))))
```

The resuming agent's [`:before-tool-call` hooks](hooks.md#resumed-calls) decide
whether it runs, but do not rewrite its logged arguments again.

The agent [runs it detached](agent.md#detached-tool-calls): `:tool-resumed` is
emitted, and the result lands as a `:user` message,
`[tool call c1 (tool-x) finished: {...}]`, ahead of a later turn. With no
`:messages`, the run waits on the resumed calls for its first turn. The new call
records `:resumes` with the old id, and the old one reads `:resumed-by` with the
new.

| A call is refused when | Reason given |
|---|---|
| its id is not in the log | `no such call` |
| it is `:accepted`, `:running`, `:ok` or `:error` | `the call is ok, not lost, abandoned or interrupted` |
| a call already resumes it | `already resumed as <id>` (resume that one) |
| it was logged `:cut` | `its arguments were cut when it was logged`[^cut] |
| it is a [sub-agent](agent.md) call | `a sub-agent call cannot be resumed`[^sub] |
| its tool is not in the agent's allow-list | `<tool> is not in this agent's tool allow-list` |
| its tool is not [`:resumable`](tools.md) | `<tool> is not resumable`, unless `:force t` |
| the agent is at its `:max-detached` cap | `max-detached reached` |

A call is resumed once, checked and logged under one lock hold, so two
resumes cannot both succeed. Calls that a cancel, a deadline or an
interrupting `:steer` closed are `:interrupted` too, and resume like the
others: the caller chooses. An agent needs a `:journal` to resume, and a
[checkpoint](checkpoints.md) taken mid-run lists the ids of its calls with no
result under `:in-flight :journal-ids`, ready to pass on after a restore.

## `tool-calls`

`:trust :operator`: resuming runs a tool again.

| `:op` | Params | Answers |
|---|---|---|
| `:list` | `:status` (default `:all`), `:limit` | `:entries`, `:total` |
| `:resume` | `:ids` (required), `:agent`, `:force` | the agent's `:resumed` and `:refused` |
| `:compact` | `:max-age` (seconds, default `*journal-max-age*`) | `:dropped`, `:kept` |

```lisp
(m:mount *ctx* 'miao:tool-calls)
(miao:invoke-tool :tool-calls :op :list :status :lost)
(miao:invoke-tool :tool-calls :op :resume :ids '("20260925-101500-123456-0"))
```

`:resume` sends `(:resume ...)` to the agent that logged the calls, or to the
mounted `:agent` named, which is required when the calls name none or several.
`:path` is a mount option (default nil, the default an agent's `:journal t`
uses), read once at mount time.

## Limitations

- A call that finishes after its agent exited is `:abandoned` with no result
  ([#180](https://todo.sr.ht/~takeiteasy/miao/180)).
- A resumed call runs again from the start; nothing reattaches to one still
  running ([#185](https://todo.sr.ht/~takeiteasy/miao/185)).
- A sub-agent call cannot be resumed
  ([#186](https://todo.sr.ht/~takeiteasy/miao/186)).
- A call with large arguments cannot be resumed
  ([#187](https://todo.sr.ht/~takeiteasy/miao/187)).

[^liveness]: The dispatching process is judged as a vault
    [claim](vault.md#claims) is: alive while this image, another host, or a
    pid started at the recorded time. A call whose agent exited inside a
    running image is `:abandoned`, not `:lost`, because the agent records it
    as it exits.

[^cut]: The arguments are cut to `:max-tool-result` characters or, with none
    set, `*journal-max-content*`, and a cut argument cannot be run again.

[^sub]: A sub-agent is not a tool: the log holds its task, not its conversation.
