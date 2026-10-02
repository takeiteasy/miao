# The run journal

An append-only log of what an agent's runs did: its conversation, kept
exactly, and the events the loop emitted. The conversation at the end of any
run reads back from it.

```lisp
(m:mount *ctx* 'miao:agent :name :assistant :model :provider-ollama :journal t)

(miao:journal-conversation "~/.miao/journal.log" :agent :assistant)
;; => ((:role :user :content "hi") (:role :assistant :content "hello")), 1
```

## `:journal`

| Value | Journals to |
|---|---|
| `nil` (default) | nowhere |
| `t` | `~/.miao/journal.log`, resolved when first used |
| a string or pathname | that file |

A [sub-agent](agent.md#sub-agents) journals to its parent's file under its own
name.

## Entries

Each entry is a plist on one line, read back with `*read-eval*` nil. All carry
`:kind`, `:id`, `:at`, `:agent`, `:parent` (a sub-agent's only) and `:run`, the
key of the root run.

| `:kind` | Written when | Holds |
|---|---|---|
| `:settings` | a run starts | `:settings`, the agent's [metadata](agent.md#mount-options) |
| `:messages` | a run starts, or a [restore](checkpoints.md) | `:messages`; `:reset t` replaces the conversation, otherwise they are added to it |
| `:message` | a message joins the conversation | `:message`, as it is |
| `:event` | the loop [emits an event](agent.md#events) | the event's `:type` and keys |
| `:call`, `:running`, `:done`, `:input` | a tool call or a keyed run is dispatched, starts, ends | [call records](calls.md) |

`:messages` and `:message` are exact. An `:event`'s `:arguments`, `:result`,
`:messages`, `:content` and `:message` are text cut to `:max-tool-result`, or
`*journal-max-content*` (16 KiB).[^cut] Streamed `:text-delta`,
`:tool-call-delta` and `:done` events are not journaled: the `:reply` event
and the `:message` entry hold the turn whole. A [hook's](hooks.md#emitting-events)
own events are not journaled either.

An entry that would not read back is written as `(:kind :unwritable :for kind)`,
so it cannot end the read.

## Writing

| Entries | Written |
|---|---|
| `:settings`, `:messages`, `:message`, `:event`, `:running` | queued, and written in batches by a thread for each log |
| `:call`, `:input`, `:done` | at once, after the queue drains |

A reader in the same process, `journal-entries` and what is built on it, drains
the queue first, so it reads what it wrote. `journal-drain path` waits for the
queue itself. An agent drains when it stops and the process when it exits.[^crash]

A log's thread exits after `*journal-writer-idle*` (5) seconds with nothing to
write, and the next entry starts it again. `journal-retire-writers` drains and
stops every thread, as `save-image` does.

## The `:reply` event

`(:type :reply :ref r :turn n :message m)` is emitted when a turn's reply joins
the conversation, after its streamed events and before the calls it makes.
A sink that did not stream the answer reads it from here.

## Reading

| Function | Answers |
|---|---|
| `journal-entries path &key agent run` | the entries, oldest first |
| `journal-runs path &key agent` | the run keys, oldest first |
| `journal-conversation path &key agent run` | the conversation at the end of `run` (default the last), and its assistant turns |

A last turn left without its tool replies, such as one cut off by a crash, is
closed with `:interrupted` replies, as an abandoned turn is.

## Compacting

`journal-compact path &key max-age` folds each run's conversation entries older
than `max-age` seconds (default `*journal-max-age*`, seven days) into one
`:messages` entry, drops the old events and drops [finished
calls](calls.md#api) that old. The [writer](#writing) runs it after a batch, or
a call finishing, once the log has passed `*journal-compact-size*` (1 MiB). Every run keeps its key and
its conversation reads back the same.

## Limitations

- The writer's queue has no bound
  ([#217](https://todo.sr.ht/~takeiteasy/miao/217)).
- Compaction rewrites the whole log under its lock, so other processes wait
  ([#216](https://todo.sr.ht/~takeiteasy/miao/216)).

[^crash]: A crash loses the entries still queued, never a call record: a `:call`
    is on disk before its tool runs and a `:done` before the call counts as
    finished. A batch the disk refuses is dropped, with a warning on stderr.
[^cut]: The same rule [call records](calls.md) apply to a call's arguments and result.
