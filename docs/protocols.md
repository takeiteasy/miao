# Protocols

A protocol is a [meow](https://github.com/takeiteasy/meow) service that
translates one neutral model contract onto one wire shape. Everything that
varies per backend — base URL, auth, model catalogue, quirks — is provider data
layered on top, so OpenRouter, Ollama, Groq, vLLM, LM Studio and
llama.cpp-server are provider definitions on one protocol rather than six
adapters.

| Layer | What it is | Where it lives |
|---|---|---|
| protocol | wire shape | a service, one per shape |
| provider | base URL, auth, catalogue, quirks | the [`define-provider`](providers.md) DSL |
| model | a bound provider and model id | named by the orchestrator |

## The convention

A protocol registers under `:protocol-<name>`, and its `metadata` plist carries
`:kind :protocol`, a `:summary`, and optionally `:params`:

```lisp
(miao:define-protocol :protocol-example
    (:summary "One line on the wire shape"
     :params '((:temperature number :doc "sampling temperature")))
    (service request)
  (list :error "not implemented"))
```

`define-protocol` expands to the service class, its `metadata` and a
`define-protocol-handler`, and records the protocol in the
[definitions](tools.md#definitions) table. The body runs for each `:complete`,
with `request` bound to the checked plist.

`:params` is a typed [schema](schema.md) advertising the sampling parameters the
protocol understands. It is advertisement, for a provider to layer defaults on —
not a coercion gate, since a request may carry keys no protocol knows.

It answers two messages:

- `(:describe)` — replies with the metadata plist
- `(:complete . plist)` — checks the request, then performs one turn

Meow intercepts the heads `%update-config`, `%effects` and `%timer-fire` before
`handle`, so a protocol must not use them.

## The request

```lisp
(miao:complete :protocol-openai
  :messages '((:role :system    :content "Be terse.")
              (:role :user      :content ((:type :text :text "list the files")))
              (:role :assistant :content nil
               :tool-calls ((:id "c1" :name :tool-shell :arguments (:cmd "ls"))))
              (:role :tool      :tool-call-id "c1" :content "a.lisp b.lisp"))
  :tools (list (miao:describe-tool :tool-shell))
  :stream sink :ref :turn-3
  :cancel token
  :timeout 30000
  :temperature 0.2)
```

Unknown keys are ignored rather than rejected, so a portable caller may offer a
superset and each protocol takes what it advertises. `:timeout` is in
milliseconds; `complete` waits longer than the protocol does, so the protocol's
own bounded `(:error :timeout)` is what a caller sees.

Roles are a closed set: `:system`, `:user`, `:assistant`, `:tool`.

## Staying neutral

The shape is chosen so both OpenAI chat completions and Anthropic Messages are
expressible:

- **Content is a list of typed blocks.** A flat string is the degenerate
  single-text-block case, which `normalize-content` expands and `content-text`
  flattens back. Only `:text` is defined here.
- **The system prompt is a `:system` role message.** A protocol needing it as a
  top-level parameter hoists leading system messages itself.
- **Tool results are `:tool` role messages** carrying `:tool-call-id`, the id of
  the call they answer. A protocol representing them as blocks inside a user
  message wraps them.
- **Streaming emits a neutral event vocabulary**, never raw provider chunks.

## The reply

```lisp
(:ok (:role :assistant :content <blocks> :tool-calls <calls>
      :done t :meta <plist>))
```

`:tool-calls` are `(:id "c1" :name :tool-shell :arguments (:cmd "ls"))`, ready
for `invoke-tool`. A call to a tool the request offered also carries that
tool's `:schema`, and a protocol renders a replayed call's arguments by it, so
a call reads the same whether or not its tool is offered again. A call built by
hand has none and renders by its shape: a plist of keywords is an object and any
other list an array. `:meta` is protocol-specific — usage counters, finish reason,
backend ids — and informational only. Dispatch on `:content`, `:tool-calls` and
`:done`.

## Streaming

A request's `:stream` sink is a function of one event, or a meow process the
events are sent to. `emit-event` takes either, and drops events when the sink is
nil. Every event echoes the request's `:ref`.

```lisp
(:type :text-delta      :ref r :text "...")
(:type :tool-call-delta :ref r :id "c1" :name :tool-shell :arguments "{\"cmd\"")
(:type :done            :ref r :reason :stop)
```

A streamed turn ends with exactly one `:done`, and nothing follows it. `:reason`
is the finish reason, or, when the exchange failed, the failed result the call
replies with:

```lisp
(:type :done :ref r :reason (:error :timeout))
(:type :done :ref r :reason (:error (:backend-error 429 "rate limited")))
```

`tool-error-p` tells the two apart. Every streaming request that reaches the
backend exchange ends this way — a refused connection, a non-OK status, a
stream cut short, a lapsed deadline — so a consumer of the sink alone can
watch for `:done`. A request rejected before the network with
`(:bad-request ...)` emits nothing.

A function sink is called on a pooled thread, one event at a time and in
order, so a sink that blocks never delays the exchange or its deadline. `complete`
returns once the sink has seen `:done`, except when the deadline lapsed: then
the reply does not wait on the sink. A sink that has not taken `:done` five
seconds past the deadline is stopped, and the events still queued for it are
dropped. A sink that signals an error loses that event and carries on.

Sinks drain on a pool of their own, capped by `*sink-pool-size*` (64) and
reported by `(miao:pool-stats :sink)`. A sink that blocks holds its thread
until it is stopped, so enough of them delay the sinks queued behind.

A tool call's `:arguments` arrive as text split across deltas; the consumer
reassembles them. The `(:ok ...)` reply carries the whole turn regardless, so a
caller may ignore the sink entirely.

The [agent loop](agent.md) is this vocabulary's main consumer: these events
pass through to its own `:sink`, alongside the loop's own `:turn`,
`:tool-call`, `:tool-result` and `:run-done` events. A turn the loop abandons
for an interrupting steer is the one exception to a single closing `:done`:
the loop stops passing its events on and emits `:turn-interrupted` instead.

## Errors

The [tool error vocabulary](tools.md#results) applies, plus one shape protocols
add:

| Reason | Meaning |
|---|---|
| `(:backend-error status detail)` | The backend was reached and the exchange broke down: a non-OK status, a malformed payload, a stream cut short. A non-OK answer that asks for a wait ends in `:retry-after ms`. |
| `:cancelled` | The request's `:cancel` token was cancelled. |

### Retry-After

A non-OK answer's `Retry-After` becomes a `:retry-after` tail on the reason, in
milliseconds; the [agent](agent.md#failed-turns) waits at least that long before
retrying.

```lisp
(:backend-error 429 "rate limited" :retry-after 2000)
```

| Header | Read as |
|---|---|
| `retry-after-ms: 1500` | 1500 ms; wins over `Retry-After` when both are sent |
| `Retry-After: 2` | seconds |
| `Retry-After: Wed, 21 Oct 2026 07:28:00 GMT` | the time until then; 0 once past |

A value it cannot read leaves the tail off. A protocol's opener hands its
response headers back as an optional third value for this.

`(:bad-request msg)` is pre-flight, so a malformed request never reaches the
network and "your ask was wrong" stays distinguishable from "the backend
misbehaved". `check-request` runs in `complete` and again in the handler, so a
protocol reached by a bare `m:call` sees the same checked request.
`tool-error-p` and `tool-error` take a result apart.

A protocol bounds its own work by the request's timeout, as tools do, so a
wedged backend costs a timeout rather than a wedged service. The deadline
closes the connection, so the reader thread unwinds rather than waiting on the
backend.

## Concurrency

Each completion runs as a job on a shared [worker pool](#worker-pools), so a
protocol or provider answers `(:describe)` and further completions while one is
in flight. A service inherits `completion-host` and defines its handler with
`define-protocol-handler`, which runs the body as the job.

`:max-in-flight`, a mount option of every `completion-host`, caps how many of
a service's completions run at once; nil, the default, leaves them uncapped.
Past the cap a completion queues:

```lisp
(meow:mount *context* 'miao:protocol-openai :max-in-flight 4)
```

- Time spent queued counts against the request's `:timeout`: the body sees
  what is left, and one that runs out while queued answers `(:error
  :timeout)` without starting.
- Cancelling a queued completion's token answers `(:error :cancelled)` at
  once, and its body never runs.
- A body that signals answers `(:error (:error "text"))`.

Stopping a service cancels every completion it has in flight or queued: each
caller receives `(:error :cancelled)`.

### The exchange

A completion's exchange runs on the thread of the job that makes it, so it
takes no thread of its own. Its `:timeout`, kept by a timer every service
shares, or a cancel shuts the socket down and unwinds the exchange.

### Worker pools

Waiting completions run on pooled threads rather than one spawned per job. A
pool is keyed by depth: a completion called directly runs at depth 0, and each
completion a job makes runs one deeper, so a job only ever waits on the next
pool down and a full pool never waits on itself. A router or fallback chain
is a protocol whose body calls `complete`:

| Depth | Runs | Waits on |
|---|---|---|
| 0 | a service called directly; a provider's completions, when it rewrites or caps them | its protocol, at depth 1 |
| 1 | what a depth 0 job completes on | the backend, or depth 2 |

Completions nested past `*max-completion-depth*` (8) answer `(:bad-request ...)`
rather than running, which stops a protocol that completes on itself. A thread
a body spawns itself starts outside the job, so wrap its function to make its
`complete` calls one deeper than the body:

```lisp
(bt:make-thread (miao:carry-completion-depth
                 (lambda () (miao:complete :protocol-openai ...))))
```

A job still running `*pool-abandon-grace*` (5) seconds past its `:timeout`, stuck
where neither the socket shutdown nor an interrupt reaches it, is abandoned: its
caller is answered `(:error :timeout)`, and its thread slot and in-flight slot
are freed for the next job. The stuck thread stays where it is, and
`pool-stats` counts it under `:abandoned` and, while it is still stuck, `:stuck`.
A pool holding `*pool-max-abandoned*` (16) stuck threads answers new completions
`(:error :unavailable)` until one returns; nil never refuses.

An [agent](agent.md)'s turns and tool calls hold no thread while they wait: the
reply arrives as a message.

`*pool-size*` (64) caps each pool's threads, read when a pool is first used. A
thread idle for `*pool-idle-seconds*` (30) exits, and one is started again as
work arrives. `(miao:pool-stats depth)` reports a pool's threads, idle threads,
queued and running jobs.

## Cancelling

A caller that no longer wants a completion passes a cancel token in the request
and cancels it from any thread:

```lisp
(let ((token (miao:make-cancel-token)))
  (bt:make-thread (lambda () (sleep 5) (miao:cancel token)))
  (miao:complete :protocol-openai ... :cancel token))
; => (:error :cancelled)
```

Cancelling shuts the connection down and ends the call at once with
`(:error :cancelled)`; a streamed turn ends with `(:type :done :reason (:error
:cancelled))`. A token already cancelled fails the call before it reaches the
network, and cancelling after the reply has arrived changes nothing.
`cancelled-p` reads the token, and `cancel` answers true the first time. A
provider passes `:cancel` through to its protocol. A
[tool call](tools.md#cancelling-a-call) takes the same token.

## The OpenAI protocol

`:protocol-openai` speaks chat completions: `POST <base-url>/chat/completions`,
with SSE when the request carries a `:stream` sink. One mounted service answers
for every backend of that shape — OpenRouter, Ollama, Groq, vLLM, LM Studio,
llama.cpp-server — so base URL, model and auth travel in the request rather than
in a config slot.

```lisp
(miao:complete :protocol-openai
  :base-url "http://127.0.0.1:11434/v1"
  :model "llama3.2"
  :headers '("authorization" "Bearer sk-...")
  :messages '((:role :user :content "hello"))
  :tools (list (miao:describe-tool :tool-shell))
  :stream sink :ref :turn-1
  :temperature 0.2)
```

| Key | Meaning |
|---|---|
| `:base-url` | required; the API root, http or https, with or without a trailing slash |
| `:model` | required; the model id the backend knows |
| `:headers` | a plist of request headers, where the provider's auth goes |

These are checked before the network, so a missing one is `(:bad-request ...)`
rather than a failed exchange. They are not in `:params`, which advertises the
sampling parameters — `:temperature`, `:top-p`, `:max-tokens`, `:stop` and
`:seed` — for a provider to layer defaults on.

`:meta` carries `:finish-reason`, the backend's `:id`, and `:usage` as a plist
of token counts. `:usage` carries `:prompt-tokens`, the prompt's size as the
backend counted it, whenever the backend reports one; every protocol names it
so, and the [agent](agent.md#counting-tokens) calibrates its context budget
from it.

Tool schemas render into the `tools` array through
[`schema->json-schema`](schema.md). A `tool_calls` reply comes back as
`:tool-calls` with a keyword name and a plist of arguments, read by the calling
tool's own schema, ready for `invoke-tool`.

Streaming reassembles what the wire splits: a tool call's id and name reach the
wire once, on the first fragment of an index, and every `:tool-call-delta`
repeats them, so a consumer of the sink alone needs no arrival order. The turn
ends on `data: [DONE]` or on a `finish_reason`; a stream that stops before
either is `(:backend-error ...)`.

Ollama serves this API at `/v1` alongside its native one, so it is testable
end to end against a local backend with no key — see
[providers](providers.md). The offline tests run against the fake HTTP
server; set `MIAO_OLLAMA_URL` (and optionally `MIAO_OLLAMA_MODEL`) to run the
live ones too.

## The Ollama protocol

`:protocol-ollama` speaks Ollama's native chat endpoint: `POST
<base-url>/api/chat`, with NDJSON streaming when the request carries a
`:stream` sink. Distinct from `:protocol-openai` because the wire shape
differs, not just the base URL — this is what carries the usage counters and
generation options the OpenAI-compatible `/v1` route drops.

```lisp
(miao:complete :protocol-ollama
  :base-url "http://127.0.0.1:11434"
  :model "llama3.2"
  :messages '((:role :user :content "hello"))
  :num-ctx 8192
  :temperature 0.2)
```

| Key | Meaning |
|---|---|
| `:num-ctx` | context window in tokens |
| `:num-predict` | cap on generated tokens |
| `:mirostat` | mirostat sampling mode |
| `:format` | response format: `json`, or a JSON schema |
| `:keep-alive` | how long to hold the model resident |

`:temperature`, `:top-p`, `:top-k`, `:stop` and `:seed` are the same sampling
knobs `:protocol-openai` advertises. On the wire, the sampling parameters nest
under an `"options"` object; `:format` and `:keep-alive` stay top-level, as
the native API has them.

`:meta` carries `:finish-reason` (from `done_reason`) and `:usage` — but
unlike OpenAI's single `usage` object, the native reply's counters
(`prompt_eval_count`, `eval_count`, `total_duration`, `load_duration`,
`prompt_eval_duration`, `eval_duration`) sit at the top level, and `:usage`
collects them the same way, with `prompt_eval_count` also as `:prompt-tokens`.

Streaming is newline-delimited JSON, not SSE: one bare object per line, no
`data:` prefix and no `[DONE]`. The turn ends on a line carrying `"done":
true`, which is also where the counters arrive — a stream cut before it is
`(:backend-error ...)`. A `tool_calls` array reaches the wire whole per
chunk rather than split into fragments, so there is no reassembly on the
receiving end. Native tool calls carry no id; one is synthesised per reply
(`call_0`, `call_1`, ...) so `:tool-calls` still satisfies the contract's own
`:id` requirement.

Tool schemas render into the `tools` array through the same renderer
`:protocol-openai` uses. A tool call's `arguments` travel as a JSON object in
both directions here, rather than the stringified form OpenAI's wire uses.

`:provider-ollama` binds this protocol to a local backend with no key — see
[providers](providers.md). Set `MIAO_OLLAMA_NATIVE_URL` (and optionally
`MIAO_OLLAMA_MODEL`) to run its live tests.

## Discovery

```lisp
(miao:protocols)                       ; => (:protocol-ollama :protocol-openai)
(miao:describe-protocol :protocol-openai)
```

`protocols` scans registration props for `:kind :protocol`, the way `tools`
scans for `:kind :tool`. `complete` reaches a [provider](providers.md) by name
the same way, since a provider answers the same messages.

## Limitations

- SBCL cannot reclaim a thread stuck past interrupts, so one stuck for good
  holds its thread until the process restarts.
