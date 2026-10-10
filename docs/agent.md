# The agent loop

An agent is a [meow](https://github.com/communal-software/meow) agent (`M:AGENT`)
that sends a conversation to a bound [model](providers.md), dispatches the
tool calls that come back through [`invoke-tool`](tools.md), feeds the
results in and goes round again. It registers under `:kind :agent`, the same
convention discovery uses for tools, protocols and providers.

It is message-driven rather than a blocking call: every model turn, tool
call and sub-agent is issued off the agent's own process and reported back as
a message, so the agent is never blocked waiting on one and stays responsive
between turns — `:cancel` and `:steer` land during a run, not just before
one.

## Running one

```lisp
(m:mount *ctx* 'miao:backend-service :name :ollama :model "llama3.2")
(m:mount *ctx* 'miao:tool-shell)

(miao:run-agent *ctx* :model :ollama :tools '(:tool-shell)
                :messages '((:role :user :content "How many .lisp files are here?")))
```

`run-agent` is the one place miao reaches the loop synchronously: it
delegates an agent on `*ctx*` (a mounted context's process), runs it to
completion, and returns `(:ok plist)` or `(:error reason)`.

A mounted, named agent works the same way any tool or provider does, driven
by messages instead:

```lisp
(m:mount *ctx* 'miao:agent :name :assistant :model :ollama)
(m:cast (m:lookup :assistant) '(:run :messages ((:role :user :content "hi"))))
```

A run ends with `:run-done` and the mounted agent stays up, holding its
conversation. `(:run :continue t :messages ms)` carries on from it, appending
`ms`; a `:run` without `:continue` starts a fresh conversation. A steer sent
between runs waits and folds into the next one. A delegated agent, such as a
sub-agent or the one [`run-agent`](#running-one) starts, has a parent and exits
when its run ends, so use `run-agent` for a one-shot run.[^conversation]

## Mount options

| Option | Default | Meaning |
|---|---|---|
| `:model` | required | a protocol or provider service name |
| `:tools` | `:default` | an allow-list of tool names, or `:default` |
| `:system` | none | a system prompt, hoisted into the first message |
| `:max-turns` | 16 | model turns before the run stops itself |
| `:turn-timeout` | 30000 | milliseconds for one `complete` call |
| `:deadline` | 300000 | milliseconds for the whole run |
| `:sub-agents` | nil | whether the model may delegate a task |
| `:max-parallel-tools` | nil | tool calls running at once, sub-agents included; nil is uncapped |
| `:tool-grace` | nil | milliseconds a tool call may run before the turn [goes on without it](#detached-tool-calls); nil waits for every call |
| `:max-detached` | nil | most calls [detached](#detached-tool-calls) at once; a call past it stays attached until a slot frees. nil is uncapped |
| `:max-tool-result` | nil | most characters of a tool result's text that reach the conversation; nil is uncapped |
| `:max-context` | nil | most tokens (estimated) a request carries, conversation and tool schemas; the oldest turns past it are left out of the request. nil is unbounded |
| `:chars-per-token` | 3 | characters per token the estimate starts from; recalibrated from each reply |
| `:turn-retries` | 0 | times a turn that failed transiently is sent again |
| `:retry-backoff` | 1000 | milliseconds before the first retry; each later one waits twice as long, or as long as `Retry-After` asks if longer, plus up to 25% jitter |
| `:hooks` | nil | the [interceptor hooks](hooks.md) a run goes through, in order; a sub-agent inherits them |
| `:log-raw` | nil | true: the journal's call records also keep what the hooks replaced |
| `:sink` | nil | a stream sink, as `complete` takes; the first [subscriber](ui.md#subscribing) |
| `:sampling` | nil | a plist passed through to `complete`, e.g. `:temperature` |
| `:vault` | nil | record steering to the [vault](vault.md): nil is off, `t` the default log, a path to record there instead |
| `:journal` | nil | record each run and each dispatched tool call to the [journal](journal.md): nil is off, `t` the default log, a path to record there instead |

## The allow-list and trust

`:tools` defaults to every discovered tool whose [`:trust`](tools.md) is
`:agent` — `tool-fs`, `tool-plan`, `tool-gated-eval`, `tool-image` and `tool-services` today;
`tool-shell`, `tool-http`, `tool-eval`, `tool-repl`, `tool-checkpoint` and
`tool-self` are all `:operator` and only reach the model when the mount
site names them explicitly:

```lisp
(m:mount *ctx* 'miao:agent :model :ollama :tools '(:tool-shell))
```

Once named, an operator tool runs unasked. The [approval hook](approvals.md)
asks the operator per call.

A call naming a tool outside the allow-list, and a tool error of any kind,
both come back to the model as a `:tool` message rather than ending the run —
the model gets a chance to recover, the same way a backend error does not
end a plain `complete` turn early inside a working conversation.

[Hooks](hooks.md) can go further than the allow-list: rewrite a call's
arguments, deny it, or redact its result.

## Messages

| Message | Effect |
|---|---|
| `(:describe)` | the metadata plist |
| `(:run . plist)` | start a run: `:messages` and any `complete` sampling keys. `:continue t` keeps the agent's current conversation and appends `:messages` to it; `:turns` and `:max-turns` still count from zero. `:input-id` makes a redelivery [a duplicate](inputs.md). `:resume ids` runs those [logged calls](calls.md#resuming-a-call) again, with `:force t` for a tool that is not `:resumable`; the answer is `(:ok (:resumed ((old . new) ...) :refused ((id reason) ...)))` |
| `(:resume :ids ids)` | run those [logged calls](calls.md#resuming-a-call) again in the run under way, or in a run that continues the conversation and waits on them. `:force t` as for `:run` |
| `(:steer :content text)` | queue a `:user` message, folded in before the next turn -- even one queued before `:run`, or while the agent is idle. A steer queued during a turn that would end the run gets a turn of its own, unless `:max-turns` is spent |
| `(:steer :content text :interrupt t)` | as `:steer`, but a model turn or tool calls in flight are abandoned and the steer folds in at once |
| `(:cancel)` | finish the run now, reason `:cancelled` |
| `(:subscribe sink)` | hear this agent's events as well, answering `(:ok (:running t :turn n))` or `(:ok (:running nil))`; see [the front-end contract](ui.md) |
| `(:unsubscribe sink)` | stop hearing them; the mount `:sink` cannot be |

`:steer` takes an optional `:input-id`, which makes a redelivery [a duplicate](inputs.md), and an optional `:vault-id`, naming an entry already in the
[vault](vault.md) -- `tool-vault`'s `:restore` redelivers a steer this way
rather than recording a second entry for the same one. A caller queueing a
fresh steer never needs to pass it; when `:vault` is on, it is recorded and
consumed automatically.

An interrupting steer cancels the turn in flight and issues the next one
straight away. Text the turn had already streamed to the `:sink` is kept as
an assistant message ahead of the steer; a half-streamed tool call is
dropped, and with no sink or subscriber nothing is kept. The abandoned turn counts
against `:max-turns`, so an interrupt on the last allowed turn finishes the
run as `:max-turns` and leaves the steer queued.

With tool calls outstanding instead, an interrupting steer
[cancels](#cancelling-tool-calls) each call still running and closes it with
an `{"error":"interrupted"}` `:tool` message, keeps the results already in,
and issues the next turn with the steer folded in, without waiting on the
slowest call. Between turns or before `:run`, `:interrupt` does nothing extra
and the steer waits for the next turn.

The rest — `:step`, `:detach`, `:call-timeout`, `:deadline` — are internal,
driving the machine between spawned work and the agent's own mailbox.

## The result

```lisp
(:ok (:messages <conversation> :content <blocks> :turns n :stop-reason r))
```

`:messages` is the whole conversation, ready to seed another run.
`:content` is the final assistant turn's content, when the run ended that
way. `:stop-reason` is one of:

| Reason | Meaning |
|---|---|
| `:stop` | the model finished without a tool call |
| `:max-turns` | `:max-turns` model turns were reached |
| `:timeout` | `:deadline` lapsed |
| `:cancelled` | `:cancel` was sent |

`:cancel` and `:deadline` also cancel the model turn in flight, closing its
connection rather than waiting out `:turn-timeout`.

A `:cancelled` or `:timeout` run [cancels](#cancelling-tool-calls) each tool
call still awaiting a result and closes it with an `{"error":"interrupted"}`
`:tool` message, so `:messages` is always well-formed to send back to a
model. Each closed call also emits a `:tool-result` event carrying `(:error
:interrupted)`.

### Cancelling tool calls

Each call is dispatched with a [cancel token](tools.md#cancelling-a-call) of
its own, cancelled when the call is closed unanswered. A call still queued
behind another on the same tool never runs; one already running stops as its
tool allows -- a shell command, HTTP exchange, worker or plan step is
abandoned, and a sub-agent is sent `:cancel`. A reply that arrives after its
call was closed is dropped, even when the next turn reuses its id.

### Capping tool calls

With `:max-parallel-tools`, calls past the cap wait, in order, and each starts
as a running one answers. A sub-agent holds its slot for its whole run; a call
refused by the allow-list answers at once and holds none. A call still waiting
when the calls are closed never runs, and closes as `interrupted` like any
other.

### Detached tool calls

One slow call need not hold up the turn. A call is **detached** when it runs
past `:tool-grace`, or at once when its tool's [metadata](tools.md) says
`:background t`:

| Step | What happens |
|---|---|
| Detach | the call's `:tool` message is the stub `{"status":"running","note":"..."}`, `:tool-detached` is emitted, and the turn goes on. The call keeps running |
| Result lands | it is emitted as `:tool-result` and queued as a `:user` message, `[tool call c1 (tool-x) finished: {...}]`, folded in before the next turn as a [steer](#messages) is |
| The model stops first | with a call still detached, the run stays open and each result that lands gets a turn. `:max-turns`, `:deadline` and `:cancel` still end it |
| At `:max-detached` | a call due to detach stays attached, so the turn waits on it, and detaches when a detached call's result lands[^deferred] |
| The run ends | a call still detached is [cancelled](#cancelling-tool-calls), emitted as `(:error :interrupted)` and logged `:interrupted` |

```lisp
(m:mount *ctx* 'miao:agent :model :ollama :tools '(:tool-shell)
                           :tool-grace 5000)
```

A detached call frees its `:max-parallel-tools` slot. A sub-agent detaches like
any other call. The result is cut to `:max-tool-result` once, when it is
folded in. An interrupting `:steer` leaves detached calls running.[^detached]

An attached call answers `(:error :timeout)` once the tool's `:timeout` plus 5
seconds lapse. A detached call has no such bound: its tool's own `:timeout`,
a [cancel](#cancelling-tool-calls), the run's `:deadline` or its process going
down end it. A run that stopped to wait on one stays open until then.

### Capping a tool result

With `:max-tool-result`, a `:tool` message in the request whose text is longer
is cut there, and a note says how much was dropped, so the model knows it saw
part of it:

```
{"text":"xxxxxxxx... [truncated: 211 characters, first 50 kept]
```

The cut is made on the request only. The conversation, the result's
`:messages` and the `:tool-result` event a sink sees carry the whole result,
and the [`:context-trimmed`](#events) event lists each cut. The cut text is
not valid JSON.

## Fitting the context

With `:max-context`, a request that would carry more than that many tokens
leaves out the oldest turns until it fits, and a note stands in for them:

```
[6 earlier messages omitted to fit the context budget]
```

| Kept | Left out |
|---|---|
| `:system` messages | the oldest turns first |
| the newest turn | an assistant turn with its tool calls and their `:tool` replies, always together |

If the system messages and the newest turn alone pass the budget, the request
is sent anyway and the event says `:over-budget t`.

The conversation is never shortened: the result's `:messages` and a
[checkpoint](checkpoints.md) hold the whole of it, and each request trims a
fresh view. A sink hears what a request left out or cut through
[`:context-trimmed`](#events).

### Counting tokens

The agent counts characters and converts at `:chars-per-token`[^ratio]:

| Step | Detail |
|---|---|
| First turn | the ratio is `:chars-per-token`, 3 unless set |
| After each reply | the ratio becomes the characters the request carried over the reply's prompt-token count (`:meta :usage :prompt-tokens`, see [protocols](protocols.md)) |
| A reply with no count | the last ratio stays |
| Margin | a request fills at most 90% of `:max-context` |
| Counted | messages and the tool schemas sent with them |

So a 4000-token budget at 4 characters per token fits 14,400 characters
(4000 × 0.9 × 4), less the tool schemas.

A sub-agent starts from its parent's ratio. A [checkpoint](checkpoints.md)
does not hold it: an agent restored elsewhere starts from `:chars-per-token`
again and recalibrates after one reply.

## Failed turns

A `complete` failure — `(:backend-error ...)`, `:timeout`, `:unavailable` —
ends the run as that same `(:error reason)`, unwrapped. So does a `:model`
nothing is registered under. With `:turn-retries`, a transient one is sent
again first:

| Failure | Retried |
|---|---|
| `:unavailable` | yes |
| `:backend-error` with status 408, 425, 429 or 5xx | yes |
| `:backend-error` with a 2xx status (a stream or payload cut short) | yes |
| other `:backend-error` statuses, `:timeout`, `:cancelled` | no |

A retry waits `:retry-backoff`, doubled each time, or the backend's
[`Retry-After`](protocols.md#retry-after) when that is longer, plus up to 25%
jitter, and emits `:turn-retry`. It is not a new turn: `:turns` and `:max-turns` do not
count it, and `:deadline` still bounds the run. What the failed attempt
streamed is dropped. A `:cancel`, `:deadline` or `:restore` during the wait
ends it. A `:steer` folds into the retry; with `:interrupt t` the retry
goes out at once.

A tool call is not retried on its own: its side effects may not be safe to
repeat. A caller can [resume](calls.md#resuming-a-call) one the call log holds
as lost.

## Events

`:sink` takes an event of one type per message, echoing `:ref` as
[protocols](protocols.md#streaming) do. The protocol's own `:text-delta`,
`:tool-call-delta` and `:done` pass straight through; the loop adds:

```lisp
(:type :run-start   :ref r :messages ms :continue nil)
(:type :steer       :ref r :content "..." :interrupt nil :input-id nil)
(:type :turn        :ref r :turn n)
(:type :turn-interrupted :ref r :turn n)
(:type :turn-retry  :ref r :turn n :attempt 1 :reason (:backend-error 503 "..."))
(:type :reply       :ref r :turn n :message m)
(:type :tool-call   :ref r :id "c1" :name :tool-shell :arguments (:cmd "ls"))
(:type :tool-detached :ref r :id "c1" :name :tool-shell)
(:type :tool-resumed :ref r :id "c1" :name :tool-shell)
(:type :tool-result :ref r :id "c1" :result (:ok (:out "...")))
(:type :hook        :ref r :phase :before-tool-call :hook :hook-x :id "c1" :action :deny
       :reason "...")
(:type :run-done    :ref r :reason :stop)
(:type :context-trimmed :ref r :turn n :omitted (1 2 3) :truncated ((4 :from 900 :to 50))
       :size 240 :budget 250 :ratio 3.9 :over-budget nil)
```

A `:reply` event carries a turn's whole reply, after the turn's streamed events
and before its calls; it is also what the [journal](journal.md) keeps of a turn.

A `:hook` event says a [hook](hooks.md#what-is-recorded) rewrote, denied or
failed; it comes before the `:tool-call` it concerns, which is emitted once the
before-tool-call hooks have answered.

Every event also carries `:agent`, the agent's registered name (nil when it
has none), and a sub-agent's carries `:parent` as well. `:run-start` opens a
run. `:steer` is emitted as a steer folds in, just ahead of the `:turn` it
joins; a detached call's folded result is not one. The keys, ordering and
delivery of every event are in [the front-end contract](ui.md#events).

`:context-trimmed` precedes a request that left out or cut anything, before
that turn's model call. `:omitted` and `:truncated` index into the whole
conversation, so a listener can tell which messages were affected; `:truncated`
entries are `(index :from characters :to kept)`. `:size` and `:budget` are
tokens, `:size` estimated at `:ratio` characters per token. A retried turn is built again
and reports again.

An interrupted turn ends with `:turn-interrupted` rather than a `:done`:
nothing it streams reaches the sink after it, and the steer's `:steer` and
the next `:turn` follow.

A turn whose `complete` failed emits the protocol's `:done` with
`:reason (:error r)`, then `:turn-retry` if it is retried, or `:run-done` if not. A turn cut short by `:cancel` or
`:deadline` ends at `:run-done`, with no `:done` of its own.

A function sink is called on a pooled thread, one event at a time and in
order, and a sub-agent's events pass through the same queue. Each function
[subscriber](ui.md#subscribing) has a queue of its own. A
sink that blocks never holds up the agent: `:steer` and `:cancel` still land.
`:run-done` is the last event a function sink sees, though the parent can get
`:agent-done`, and `run-agent` return, before the sink has taken it. A sink that has not taken `:run-done` five seconds after
the run ends is stopped, and the events still queued for it are dropped. A
sink that signals an error loses that event and carries on.

## Sub-agents

With `:sub-agents t`, the model gets a reserved tool, `agent-task`, taking one
`:task` string. Calling it delegates a child agent — under meow's own agent
supervisor, via `m:delegate` — with this agent's model, allow-list,
`:max-parallel-tools`, `:vault`, `:journal` and [`:hooks`](hooks.md), runs it to completion, and returns its final answer as the tool
result. The child's `:ref`, echoed on its events, is a cons of an internal
step counter and the call id. A child does not itself get `:sub-agents`, so
delegation does not nest by default.

A child of a named parent is registered as `<parent>/<n>`, counting from 1
(`:boss/1`, `:boss/2`), so it is reached as a mounted agent is:

```lisp
(miao:sub-agents :boss)                              ; => (:BOSS/1 :BOSS/2)
(miao:describe-agent :boss/1)                        ; :parent :boss, :task "..."
(m:cast (m:lookup :boss/1) '(:steer :content "use the v2 API" :interrupt t))
(m:cast (m:lookup :boss/1) '(:cancel))               ; the parent's tool result is {"error":"cancelled"}
```

The name is free for reuse once the child exits. A child of an unnamed parent
(one `run-agent` starts) stays unnamed. Its events go to its parent's sinks
with `:agent` set to its name and `:parent` set, and a steer recorded against
it carries its name (see [the vault](vault.md)). `agents` lists
live children too, and a [checkpoint](checkpoints.md) leaves them out.

Reaching the parent's `handle` from a delegated child needs
meow “A service parent never sees an agent's :agent-done / :agent-down”: meow's
`%dispatch` dropped a service parent's `:agent-done`/`:agent-down` before
that fix, so a mounted or delegated `miao:agent` on an unpatched meow will
never see a sub-agent finish.

Which models and tool sets a child may be given beyond inheriting the
parent's is [#3](https://github.com/communal-software/miao/issues/3)'s
policy (the orchestrator DSL), not this loop's.

## Checkpoints

An agent's `snapshot` keeps `:messages` and `:turns`, not the turn or tool
calls in flight — see [checkpoints](checkpoints.md). A checkpoint taken
mid-run keeps the conversation, closes each unanswered tool call as
`interrupted` and drops the abandoned turn. A detached call keeps its stub and
is listed under `:in-flight :detached`, and the log ids of the calls with no result
under `:in-flight :journal-ids`. A restore ends them, and its result is never
delivered to the restored agent, but a caller can [resume](calls.md#resuming-a-call)
them from those ids. `restore` always lands a not-running agent, ready for
`(:run :continue t)`, and nothing more from the abandoned turn reaches the sink.

## Forking

`fork-agent` continues a prefix of an agent's conversation as a sibling agent,
leaving the original alone -- see [forking](forking.md).

## Limitations

- A request over `:max-context` drops old turns rather than summarising them
  ([#10](https://github.com/communal-software/miao/issues/10)).
- A streamed turn on an OpenAI-style backend reports no prompt-token count,
  so it does not recalibrate the ratio
  (“Ask an OpenAI-style backend for usage on a streamed turn”).
- A detached call still ends with its run, so work longer than the run's
  `:deadline` has no home; that needs a job a tool hands back and the agent
  polls ([#40](https://github.com/communal-software/miao/issues/40)).
- A tool that ignores a cancel and never answers keeps its call pending for
  good ([#39](https://github.com/communal-software/miao/issues/39)).
- A restored agent runs a call detached when it was checkpointed again rather
  than reattaching to it ([#36](https://github.com/communal-software/miao/issues/36)).
- The first turn is measured at the default ratio; an exact count before it
  needs a tokenizer ([#11](https://github.com/communal-software/miao/issues/11)).

[^ratio]: The ratio is per agent, not per content type: code and JSON
    tokenise worse than prose, which the 10% margin absorbs. A reading under 1
    or over 8 characters per token is ignored. `:max-tool-result` stays in
    characters, as do `:from` and `:to` in `:context-trimmed`. `:max-context`
    is the prompt's share of the window; leave room below it for the reply.

[^deferred]: A deferred call is still attached, so its call timeout applies:
    it answers `(:error :timeout)` if no slot frees before then. Deferred calls
    detach in the order they were due. A [resumed](calls.md#resuming-a-call)
    call is detached from the start and counts toward the cap.

[^detached]: The folded result is a plain `:user` message, so it counts toward
    `:max-context` like any other and is not cut again in a request's view.
    A result that lands while a turn is in flight waits for the next turn, and
    one that lands after the last allowed turn is dropped with the run.

[^conversation]: The conversation lives in the agent instance. If the agent
    crashes, mount restarts it as a fresh instance with none, and its
    [subscribers](ui.md#subscribing) are kept. A [checkpoint](checkpoints.md)
    restores one.
