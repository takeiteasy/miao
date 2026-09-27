# Client state

`miao/ui` folds an agent's [events](ui.md#events) into the state an operator
sees, and sends the operator's [commands](ui.md#commands) back. It draws
nothing: a renderer reads the state and never reads events itself.

```lisp
(ql:quickload :miao/ui)
(m:mount *ctx* 'miao:agent :name :assistant :model :provider-ollama)

(let ((client (miao/ui:attach :assistant :on-change #'redraw)))
  (miao/ui:run client "hi")
  (miao/ui:client-state client))
```

## Attaching

| Call | Effect |
|---|---|
| `(attach name &key on-change registry)` | subscribe to the agent mounted as `name` and return a client |
| `(detach client)` | unsubscribe; events already on their way may still arrive |
| `(client-state client)` | the state so far, an immutable snapshot |

`on-change` is called with each new state, from the thread that delivered the
event. A snapshot is safe to draw from any thread. A client that attaches
mid-run is sent the [run so far](ui.md#subscribing) first, so it ends up in the
same state as one that was there from the start.

## Commands

| Call | Sends |
|---|---|
| `(run client text)` | `(:run :messages ...)` with `text` as the user's message |
| `(continue-run client text)` | `(:run :continue t :messages ...)` |
| `(steer client text &key interrupt)` | `(:steer :content text)` |
| `(cancel client)` | `(:cancel)` |

Each answers what the agent answers. A command waits up to `*command-timeout*`
seconds (5) for an agent that is being restarted or not yet mounted, then
signals `agent-unavailable`.

## State

`fold-event` takes a state and an event and returns the next state. It never
changes the state it is given and never signals, so an event type it does not
know, or one that arrives without those before it, is folded as far as it makes
sense.

| Reader | Meaning |
|---|---|
| `(state-status s)` | `:idle`, `:running` or `:done`, for the root agent |
| `(state-reason s)` | the last `:run-done` reason |
| `(state-turn s)` | the current turn |
| `(state-transcript s)` | the root's entries, oldest first |
| `(state-root s)`, `(state-children s node)` | the agent tree, see [below](#agents) |

Each transcript entry has an `entry-kind`:

| Kind | From | Readers |
|---|---|---|
| `:message` | the messages of `:run-start` | `entry-role`, `entry-text` |
| `:steer` | `:steer` | `entry-text`, `entry-status` is `:interrupt` for an interrupting steer |
| `:text` | `:text-delta`, one entry per turn's run of text | `entry-text` |
| `:call` | tool call events | `entry-id`, `entry-name`, `entry-arguments`, `entry-status`, `entry-result` |
| `:notice` | retries, trims, interrupts, failed turns | `entry-name` is the kind, `entry-result` its detail |

A call's status is `:streaming` (its arguments are still arriving, and
`entry-text` holds them so far), `:running`, `:detached`, `:abandoned` or
`:done`.[^text]

### Agents

The root node holds the agent's own run. A sub-agent has a node of its own,
with its transcript, status and reason, listed by `state-children` under the
node whose call started it.[^tree]

```lisp
(let ((root (miao/ui:state-root state)))
  (dolist (child (miao/ui:state-children state root))
    (format t "~a: ~a~%" (miao/ui:node-call-id child) (miao/ui:node-status child))))
```

## Limitations

- A client that attaches to an idle agent starts with an empty transcript, not
  the conversation the agent holds ([#203](https://todo.sr.ht/~takeiteasy/miao/203)).
- Sub-agent nodes are found by call id, and cannot be steered or cancelled
  ([#200](https://todo.sr.ht/~takeiteasy/miao/200)).
- There is no state for operator approvals
  ([#199](https://todo.sr.ht/~takeiteasy/miao/199)).

[^text]: Streamed text is kept as chunks and joined by `entry-text`, so a long
    answer does not copy itself on every delta.

[^tree]: A sub-agent's events carry the ref `(step . call-id)` and a `:parent`
    key, and its node is keyed on that ref. The parent is the node whose
    transcript holds the call.
