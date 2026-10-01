# Interceptor hooks

A hook is a service the [agent loop](agent.md) asks before it acts. It can
rewrite what is about to happen, or, before a tool call, refuse it. Name the
hooks an agent runs through `:hooks`:

```lisp
(miao:define-hook :hook-no-shell (:phases (:before-tool-call))
  (:intercept (phase request)
    (if (eq (getf request :name) :tool-shell)
        '(:deny "shell is off in this room")
        :pass)))

(m:mount *ctx* 'hook-no-shell)
(m:mount *ctx* 'miao:agent :model :provider-ollama
                           :tools '(:tool-shell :tool-fs)
                           :hooks '(:hook-no-shell))
```

Observing an event, to log it or notify someone, needs no hook: a
[sink](ui.md) hears every event the loop emits. A hook is for changing what
happens.

## Phases

| Phase | Runs | The hook sees | It may |
|---|---|---|---|
| `:before-turn` | before a turn's request is built | `:messages`, the conversation | rewrite it: redact, inject context |
| `:before-tool-call` | before a call runs | `:id`, `:name`, `:arguments` | rewrite the arguments, or deny |
| `:after-tool-result` | after a call answers | `:id`, `:name`, `:result` | rewrite the result |

Every request also carries `:phase`, `:agent` and, on a sub-agent, `:parent`.
A `:before-turn` rewrite changes the request only; the conversation, and so a
[checkpoint](checkpoints.md), keeps what was said.[^order]

## Answers

| Answer | Effect |
|---|---|
| `:pass` | carry on |
| `(:rewrite value)` | replace the phase's subject: a list of messages, an arguments plist, or `(:ok plist)` / `(:error reason)` |
| `(:deny reason)` | before a tool call only: the model gets `(:error (:denied hook reason))` as the call's `:tool` message, and the tool does not run |

Anything else, or a rewrite of the wrong shape, is a [failure](#failure).

## Several hooks

The hooks of a phase run in `:hooks` order, one at a time. Each sees what the
last one left, so a rewrite builds on the one before. A `:deny` ends the chain:
later hooks do not run.

## Failure

A hook that signals, times out, is down, or answers badly has failed. Each
hook declares what that means, and a `:hooks` entry may override it:

| `:on-error` | A failed hook |
|---|---|
| `:deny` (default) | fails closed. Before a tool call, the call is answered `(:error (:hook-failed hook reason))`. After a result, the result is replaced by that error, so the raw content does not reach the model. Before a turn, the run ends with that error |
| `:pass` | is skipped, and the chain carries on |

```lisp
:hooks '(:hook-audit                          ; as the hook declares
         (:hook-flaky :on-error :pass)        ; override
         (:hook-slow :timeout 60000))
```

A named hook that is not registered when a run starts refuses the run with a
`bad-request`. Each hook's settings are read then, so a hook that dies
mid-run still fails by the policy it declared.

## Defining a hook

`define-hook` takes the hook's keyword name, its options and one `:intercept`
clause, as [`define-tool`](tools.md) does:

| Option | Default | Meaning |
|---|---|---|
| `:phases` | all three | the phases it runs in |
| `:on-error` | `:deny` | [what a failure means](#failure) |
| `:timeout` | 30000 | milliseconds the agent waits for one answer; the run's `:deadline` still bounds it |
| `:summary` | none | a line for introspection |
| `:slots` | none | passed to `defservice`, for state the hook keeps |

The clause binds `phase` and `request`, the whole plist, and `service`
anaphorically. A condition the body signals is a failure; the service stays
up. `(miao:hooks)` lists the registered hooks and `(miao:describe-hook name)`
answers one's metadata.

### A function

A function of `(phase request)` is a hook too, with the same options after it:

```lisp
:hooks (list (list (lambda (phase request)
                     (declare (ignore phase))
                     (list :rewrite (redact (getf request :result))))
                   :phases '(:after-tool-result) :name :redact))
```

The agent mounts a service for it and stops it with the agent.

## Sub-agents

A [sub-agent](agent.md#sub-agents) inherits its parent's hooks, so a permission
hook is not escaped by delegating. The reserved `agent-task` call is itself a
tool call: it passes through `:before-tool-call`, and the child's own calls do
too.

## What is recorded

Events, the [call log](calls.md), the conversation and checkpoints carry the
values after the hooks, so a redacted secret is in none of them.

| Where | Holds |
|---|---|
| `:tool-call` event, `:call` log entry | the arguments the tool ran with. It is emitted once the hooks have answered, so a `:hook` event for a call comes first |
| `:tool-result` event, `:done` log entry | the result after the hooks |
| `:hook` event | that a hook acted: `:phase`, `:hook`, `:id` (the call's, nil before a turn), `:action` (`:rewrite`, `:deny` or `:failed`) and, for the last two, `:reason`. Never the payload |
| a denied call | logged `:denied`, with its `:tool-call` and `:tool-result` events |

With `:log-raw t` on the agent the call log also keeps what the hooks
replaced: `:raw-arguments` on the `:call` entry and `:raw-content` on the
`:done` one. A hook that logs for itself has the raw payload anyway.

## Not blocking the agent

A hook is called as a [tool](tools.md) is, off the agent's process, so a hook
that waits does not hold up `:cancel` or `:steer`. A cancel, a deadline or an
interrupting steer drops the answer that is still to come, and a call whose
hook was still deciding does not run.

## Limitations

- A hook is not told when its run is cancelled, so one waiting on a long
  answer keeps waiting
  ([#210](https://todo.sr.ht/~takeiteasy/miao/210)).
- A [resumed call](calls.md#resuming-a-call) is run again from its logged
  arguments, which the hooks already rewrote once, and they see them again
  ([#211](https://todo.sr.ht/~takeiteasy/miao/211)).

[^order]: The hooks see the whole conversation, and the result is then fitted
    to `:max-context`, so injected text counts against the budget and the
    estimate is taken from what is sent. The indices in
    [`:context-trimmed`](agent.md#events) therefore point into the hooked
    view, and match the conversation only when the hooks rewrite messages in
    place.
