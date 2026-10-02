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

`:messages` and `:message` are exact. An `:event`'s `:arguments`, `:result`,
`:messages`, `:content` and `:message` are text cut to `:max-tool-result`, or
`*journal-max-content*` (16 KiB).[^cut] Streamed `:text-delta`,
`:tool-call-delta` and `:done` events are not journaled: the `:reply` event
and the `:message` entry hold the turn whole. A [hook's](hooks.md#emitting-events)
own events are not journaled either.

An entry that would not read back is written as `(:kind :unwritable :for kind)`,
so it cannot end the read.

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

`journal-compact path &key max-age` folds each agent's conversation entries
older than `max-age` seconds (default `*journal-max-age*`, seven days) into one
`:reset :messages` entry and drops the old events. It runs when a run starts and
the log has passed `*journal-compact-size*` (1 MiB). The conversation reads back
the same; the keys of the runs it folds do not.

## Limitations

- Each entry is a synchronous write on the agent's process
  ([#181](https://todo.sr.ht/~takeiteasy/miao/181)).
- Compaction rewrites the whole log under its lock
  ([#216](https://todo.sr.ht/~takeiteasy/miao/216)).

[^cut]: The same rule the [call log](calls.md) applies to a call's arguments and result.
