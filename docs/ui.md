# Front-end contract

A front end drives an [agent](agent.md) with messages and draws what it
hears back as events. Nothing else crosses the line.

| Direction | What |
|---|---|
| Front end to agent | [commands](#commands): `:run`, `:steer`, `:cancel`, `:subscribe`, `:unsubscribe` |
| Agent to front end | [events](#events), one plist each, to every subscribed sink |

[Client state](client-state.md) folds these events for a front end to draw.

```lisp
(m:mount *ctx* 'miao:agent :name :assistant :model :provider-ollama)
(m:call (m:lookup :assistant) (list :subscribe #'draw-event))
(m:cast (m:lookup :assistant) '(:run :messages ((:role :user :content "hi"))))
```

## Commands

| Message | Answer | Effect |
|---|---|---|
| `(:run :messages ms)` | `:ok` | start a run |
| `(:run :continue t :messages ms)` | `:ok` | carry on from the agent's conversation, appending `ms` |
| `(:steer :content text)` | `:ok` | fold a `:user` message in before the next turn |
| `(:steer :content text :interrupt t)` | `:ok` | as `:steer`, abandoning the turn or tool calls in flight |
| `(:cancel)` | `:ok`, or the run's result while one is under way | finish the run now, reason `:cancelled` |
| `(:subscribe sink)` | `(:ok (:running t :turn n))` or `(:ok (:running nil))` | hear this agent's events |
| `(:unsubscribe sink)` | `:ok` | stop hearing them |

`:run`, `:steer` and `:cancel` are described in [the agent loop](agent.md#messages),
with `:input-id`, `:resume` and the rest. A `:subscribe` or `:unsubscribe`
that [breaks a rule](#subscribing) answers a `:bad-request` [tool error](tools.md).

A run ends with `:run-done` and the agent stays up, so the next `:run` goes to
the same agent, and `:continue t` carries on from its conversation.[^restart]

## Subscribing

A `sink` is a function, a symbol naming one, or a meow process.

| Rule | Detail |
|---|---|
| The mount `:sink` | is the first subscriber, and `:unsubscribe` refuses it |
| Subscribing twice | once is enough; the second changes nothing |
| Idle agent | the sink is kept and hears the next run from its `:run-start` |
| Running agent | the sink is first sent the run so far, from its `:run-start`, then hears everything live; the answer says which turn |
| Replay | a sub-agent's events are replayed too, and adjacent `:text-delta`s of one agent arrive merged into one |
| Unsubscribing | events already queued for the sink are still delivered; nothing after |
| Snapshots | subscribers are not part of a [checkpoint](checkpoints.md) |

Subscriptions belong to the agent's name, so they outlive a crash restart and
end when the agent is unmounted or exits and is not restarted.[^subscribers] A sub-agent has no
subscribers of its own: its events reach its parent's.

## Events

Every event is a plist. These keys are on all of them:

| Key | Meaning |
|---|---|
| `:type` | the event's kind, below |
| `:ref` | the agent's ref, as [protocols](protocols.md#streaming) echo it |
| `:agent` | the agent's registered name, or nil when it has none |
| `:parent` | on a sub-agent's events only: its parent's name, nil when unnamed |

A front end tells a sub-agent's events from the root's by whether the
`:parent` key is present, not by its value.[^parent]

| `:type` | Extra keys | When |
|---|---|---|
| `:run-start` | `:messages`, `:continue` | a run begins; `:messages` are the ones the `:run` carried |
| `:steer` | `:content`, `:interrupt`, `:input-id` | a steer is folded into the conversation, before that turn's `:turn` |
| `:turn` | `:turn` | a model turn is sent |
| `:turn-retry` | `:turn`, `:attempt`, `:reason` | a failed turn is [sent again](agent.md#failed-turns) |
| `:turn-interrupted` | `:turn` | a turn was abandoned for an interrupting steer |
| `:text-delta` | `:text` | streamed text |
| `:tool-call-delta` | `:id`, `:name`, `:arguments` | a fragment of a call, arguments split across deltas |
| `:done` | `:reason` | a turn ends, with its finish reason or `(:error r)` |
| `:reply` | `:turn`, `:message` | a turn's whole reply joined the conversation, after its `:done` and before its calls |
| `:tool-call` | `:id`, `:name`, `:arguments` | a call is dispatched, with the arguments after any [hooks](hooks.md#what-is-recorded); with before-tool-call hooks, once they have answered |
| `:tool-detached` | `:id`, `:name` | a call [runs on](agent.md#detached-tool-calls) without holding the turn |
| `:tool-resumed` | `:id`, `:name` | a logged call is [run again](calls.md#resuming-a-call) |
| `:tool-result` | `:id`, `:result` | a call answered, `(:ok ...)` or `(:error ...)`, after any hooks. A denied call's is `(:error (:denied hook reason))` |
| `:hook` | `:phase`, `:hook`, `:id`, `:action`, `:reason` | an [interceptor hook](hooks.md) acted: `:action` is `:rewrite`, `:deny` or `:failed`, `:reason` is on the last two, `:id` is the call's and nil before a turn. It never carries the payload |
| `:approval-request`, `:approval-done` | see [approval](approvals.md#events) | an [operator approval](approvals.md) is waiting, and ends. They are answered with `miao:answer-approval`, not an agent command. Any hook may emit events of its own, with `:hook` |
| `:context-trimmed` | see [the loop](agent.md#events) | a request left out or cut something |
| `:run-done` | `:reason` | the run ended, with its stop reason or `(:error r)` |

```lisp
(:type :run-start :ref nil :messages ((:role :user :content "hi")) :continue nil :agent :assistant)
(:type :steer :ref nil :content "shorter" :interrupt t :input-id nil :agent :assistant)
(:type :run-done :ref nil :reason :stop :agent :assistant)
```

A `:steer` event marks where the steer landed. A detached call's result that
folds in as a `:user` message is not a steer: it has its `:tool-result`.

## Ordering and delivery

| Guarantee | Detail |
|---|---|
| Per sink, in order | each sink hears events one at a time, in the order they happened, whichever agent in the tree emitted them |
| `:run-start` first | it precedes every other event of a run, `:tool-resumed` included |
| `:run-done` last | the root's `:run-done` is the last event of a run; a sub-agent's comes earlier, with `:parent` |
| One `:done` per turn | a turn that is interrupted, cancelled or cut short by `:deadline` has none |
| A slow sink | never holds up the agent or another sink; `:steer` and `:cancel` still land |
| A stuck sink | five seconds after the run ends its remaining events are dropped, and the other sinks are unaffected |
| A failing sink | loses that event and carries on |

A run that refuses every [resumed call](calls.md#resuming-a-call) and has no
messages never starts, and emits nothing.

A sink that subscribes mid-run hears the run so far first, then the live
events, each once and in order. The replay covers the current run only.[^replay]

## Limitations

- A run's events are all held until it ends, for a mid-run subscriber's replay
  ([#202](https://todo.sr.ht/~takeiteasy/miao/202)).
- A subscriber that attaches to an idle agent is not sent the conversation it
  holds ([#203](https://todo.sr.ht/~takeiteasy/miao/203)).
- The client has no command that targets a sub-agent. One is reached by name
  with `m:cast`, and `miao:sub-agents` lists a run's live children
  ([agent.md](agent.md#sub-agents)).

[^restart]

## Subscribing

A `sink` is a function, a symbol naming one, or a meow process.

| Rule | Detail |
|---|---|
| The mount `:sink` | is the first subscriber, and `:unsubscribe` refuses it |
| Subscribing twice | once is enough; the second changes nothing |
| Idle agent | the sink is kept and hears the next run from its `:run-start` |
| Running agent | the sink is first sent the run so far, from its `:run-start`, then hears everything live; the answer says which turn |
| Replay | a sub-agent's events are replayed too, and adjacent `:text-delta`s of one agent arrive merged into one |
| Unsubscribing | events already queued for the sink are still delivered; nothing after |
| Snapshots | subscribers are not part of a [checkpoint](checkpoints.md) |

Subscriptions belong to the agent's name, so they outlive a crash restart and
end when the agent is unmounted or exits and is not restarted.[^subscribers] A sub-agent has no
subscribers of its own: its events reach its parent's.

## Events

Every event is a plist. These keys are on all of them:

| Key | Meaning |
|---|---|
| `:type` | the event's kind, below |
| `:ref` | the agent's ref, as [protocols](protocols.md#streaming) echo it |
| `:agent` | the agent's registered name, or nil when it has none |
| `:parent` | on a sub-agent's events only: its parent's name, nil when unnamed |

A front end tells a sub-agent's events from the root's by whether the
`:parent` key is present, not by its value.[^parent]

| `:type` | Extra keys | When |
|---|---|---|
| `:run-start` | `:messages`, `:continue` | a run begins; `:messages` are the ones the `:run` carried |
| `:steer` | `:content`, `:interrupt`, `:input-id` | a steer is folded into the conversation, before that turn's `:turn` |
| `:turn` | `:turn` | a model turn is sent |
| `:turn-retry` | `:turn`, `:attempt`, `:reason` | a failed turn is [sent again](agent.md#failed-turns) |
| `:turn-interrupted` | `:turn` | a turn was abandoned for an interrupting steer |
| `:text-delta` | `:text` | streamed text |
| `:tool-call-delta` | `:id`, `:name`, `:arguments` | a fragment of a call, arguments split across deltas |
| `:done` | `:reason` | a turn ends, with its finish reason or `(:error r)` |
| `:reply` | `:turn`, `:message` | a turn's whole reply joined the conversation, after its `:done` and before its calls |
| `:tool-call` | `:id`, `:name`, `:arguments` | a call is dispatched, with the arguments after any [hooks](hooks.md#what-is-recorded); with before-tool-call hooks, once they have answered |
| `:tool-detached` | `:id`, `:name` | a call [runs on](agent.md#detached-tool-calls) without holding the turn |
| `:tool-resumed` | `:id`, `:name` | a logged call is [run again](calls.md#resuming-a-call) |
| `:tool-result` | `:id`, `:result` | a call answered, `(:ok ...)` or `(:error ...)`, after any hooks. A denied call's is `(:error (:denied hook reason))` |
| `:hook` | `:phase`, `:hook`, `:id`, `:action`, `:reason` | an [interceptor hook](hooks.md) acted: `:action` is `:rewrite`, `:deny` or `:failed`, `:reason` is on the last two, `:id` is the call's and nil before a turn. It never carries the payload |
| `:approval-request`, `:approval-done` | see [approval](approvals.md#events) | an [operator approval](approvals.md) is waiting, and ends. They are answered with `miao:answer-approval`, not an agent command. Any hook may emit events of its own, with `:hook` |
| `:context-trimmed` | see [the loop](agent.md#events) | a request left out or cut something |
| `:run-done` | `:reason` | the run ended, with its stop reason or `(:error r)` |

```lisp
(:type :run-start :ref nil :messages ((:role :user :content "hi")) :continue nil :agent :assistant)
(:type :steer :ref nil :content "shorter" :interrupt t :input-id nil :agent :assistant)
(:type :run-done :ref nil :reason :stop :agent :assistant)
```

A `:steer` event marks where the steer landed. A detached call's result that
folds in as a `:user` message is not a steer: it has its `:tool-result`.

## Ordering and delivery

| Guarantee | Detail |
|---|---|
| Per sink, in order | each sink hears events one at a time, in the order they happened, whichever agent in the tree emitted them |
| `:run-start` first | it precedes every other event of a run, `:tool-resumed` included |
| `:run-done` last | the root's `:run-done` is the last event of a run; a sub-agent's comes earlier, with `:parent` |
| One `:done` per turn | a turn that is interrupted, cancelled or cut short by `:deadline` has none |
| A slow sink | never holds up the agent or another sink; `:steer` and `:cancel` still land |
| A stuck sink | five seconds after the run ends its remaining events are dropped, and the other sinks are unaffected |
| A failing sink | loses that event and carries on |

A run that refuses every [resumed call](calls.md#resuming-a-call) and has no
messages never starts, and emits nothing.

A sink that subscribes mid-run hears the run so far first, then the live
events, each once and in order. The replay covers the current run only.[^replay]

## Limitations

- A run's events are all held until it ends, for a mid-run subscriber's replay
  ([#202](https://todo.sr.ht/~takeiteasy/miao/202)).
- A subscriber that attaches to an idle agent is not sent the conversation it
  holds ([#203](https://todo.sr.ht/~takeiteasy/miao/203)).
- The live list of agents and sub-agents
  ([#121](https://todo.sr.ht/~takeiteasy/miao/121)) is not part of the
  contract yet. Until #121, a sub-agent reports `:agent nil` and cannot be
  steered or cancelled.

[^restart]: A crash brings the agent back as a fresh instance under mount's
    default `:transient` restart, with no conversation.

[^replay]: The run's events are recorded from `:run-start` while it lasts. A
    turn's text is kept as it was streamed, merged, so a replayed `:text-delta`
    may hold several of the original ones.

[^subscribers]: The subscribers of a named agent are kept beside the mount,
    keyed by its registry and name, so a restarted instance finds them. An agent
    with no name, such as one [`run-agent`](agent.md#running-one) starts,
    keeps them in the instance for that one run.

[^parent]: An agent started with `run-agent` has no name, so its
    sub-agents report `:parent nil`. The run is over at the `:run-done` with
    no `:parent` key.
