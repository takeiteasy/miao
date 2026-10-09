# Protocols

A completion is one turn against a model backend. The contract, the wire
protocols and the [providers](providers.md) live in
[cl-inference/client](https://github.com/takeiteasy/cl-inference/blob/trunk/docs/client.md).
miao mounts each backend as a [meow](https://github.com/takeiteasy/meow)
service, runs its completions on shared worker pools, and delivers streamed
events to a sink.

| Layer | What it is | Where it lives |
|---|---|---|
| protocol | a wire shape: `:protocol-openai`, `:protocol-ollama` | cl-inference/client |
| provider | base URL, auth, catalogue, quirks over a protocol | cl-inference/client |
| `backend-service` | a mounted backend that answers `complete` | miao |

## Mounting a backend

`backend-service` wraps a registered client backend, protocol or provider. It
is mounted under the backend's keyword:

```lisp
(meow:mount *context* 'miao:backend-service :name :ollama :model "llama3.2")
(miao:complete :ollama :messages '((:role :user :content "hello")))
```

| Initarg | Meaning |
|---|---|
| `:name` | the backend's keyword, and the service's name |
| `:backend` | the backend's keyword when it differs from `:name` |
| `:base-url`, `:model` | override a provider's declaration; defaults under each request for a protocol |
| `:api-key` | a provider's key; never written to a [generation](checkpoints.md) |
| `:max-in-flight` | the [cap](#concurrency) on concurrent completions |

`ensure-mounted` mounts a registered backend by name, so the
[CLI](cli.md) needs no mount code. A name no backend is registered under
fails the mount.

The service answers two messages:

- `(:describe)` replies with the backend's metadata plist, whose `:kind` is
  `:protocol` or `:provider`
- `(:complete . plist)` checks the request, then performs one turn

## The request

`complete` takes the client's [request](https://github.com/takeiteasy/cl-inference/blob/trunk/docs/client.md#request):

```lisp
(miao:complete :protocol-openai
  :base-url "http://127.0.0.1:11434/v1" :model "llama3.2"
  :messages '((:role :user :content "list the files"))
  :tools (list (miao:describe-tool :tool-shell))
  :stream sink :ref :turn-3
  :cancel token
  :timeout 30000)
```

`complete` waits longer than the backend does, so the backend's own bounded
`(:error :timeout)` is what a caller sees. The reply, content blocks and
errors are the client's.[^errors]

## Streaming

A request's `:stream` sink is a function of one event. Every event echoes the
request's `:ref`:

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
```

`result-error-p` tells the two apart. A request rejected before the backend
with `(:bad-request ...)` emits nothing.

The service delivers events through an emitter: the sink is called on a pooled
thread, one event at a time and in order, so a sink that blocks never delays
the exchange or its deadline. `complete` returns once the sink has seen `:done`,
except when the deadline lapsed: then the reply does not wait on the sink. A
sink that has not taken `:done` `*emitter-grace*` (5) seconds past the deadline
is stopped, and the events still queued for it are dropped. A sink that
signals an error loses that event and carries on.

Sinks drain on a pool of their own, capped by `*sink-pool-size*` (64) and
reported by `(miao:pool-stats :sink)`.

The [agent loop](agent.md) is the main consumer: these events pass through to
its own `:sink`, alongside the loop's `:turn`, `:tool-call`, `:tool-result` and
`:run-done` events. A turn the loop abandons for an interrupting steer is the
one exception to a single closing `:done`: the loop stops passing its events on
and emits `:turn-interrupted` instead. `deliver-event` sends an event to a
function, a meow process, an emitter or a fanout of those.

## Retry-After

A non-OK answer's `Retry-After` is a `:retry-after` tail on the
`:backend-error` reason, in milliseconds. The [agent](agent.md#failed-turns)
waits at least that long before retrying.

```lisp
(:backend-error 429 "rate limited" :retry-after 2000)
```

## Concurrency

Each completion runs as a job on a shared [worker pool](#worker-pools), so a
service answers `(:describe)` and further completions while one is in flight.
A service inherits `completion-host` and defines its handler with
`define-protocol-handler`, which runs the body as the job.

`:max-in-flight`, a mount option of every `completion-host`, caps how many of
a service's completions run at once; nil, the default, leaves them uncapped.
Past the cap a completion queues:

```lisp
(meow:mount *context* 'miao:backend-service :name :protocol-openai :max-in-flight 4)
```

- Time spent queued counts against the request's `:timeout`: the body sees
  what is left, and one that runs out while queued answers `(:error
  :timeout)` without starting.
- Cancelling a queued completion's token answers `(:error :cancelled)` at
  once, and its body never runs.
- A body that signals answers `(:error (:error "text"))`.

Stopping a service cancels every completion it has in flight or queued: each
caller receives `(:error :cancelled)`.

### Worker pools

Waiting completions run on pooled threads rather than one spawned per job. A
pool is keyed by depth: a completion called directly runs at depth 0, and each
completion a job makes runs one deeper, so a job only ever waits on the next
pool down and a full pool never waits on itself. A router or fallback chain
is a service whose body calls `complete`:

| Depth | Runs | Waits on |
|---|---|---|
| 0 | a service called directly | the backend, or depth 1 |
| 1 | what a depth 0 job completes on | the backend, or depth 2 |

Completions nested past `*max-completion-depth*` (8) answer `(:bad-request ...)`
rather than running, which stops a service that completes on itself. A thread
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
network, and cancelling after the reply has arrived changes nothing. The token
is the client's `make-cancel-token`; a [tool call](tools.md#cancelling-a-call)
takes the same one.

## Discovery

```lisp
(miao:protocols)                       ; => (:protocol-openai)
(miao:describe-protocol :protocol-openai)
(miao:definitions :kind :provider)     ; => (:ollama)
```

`protocols` and `providers` scan registration props for the mounted services of
`:kind :protocol` and `:kind :provider`, the way `tools` scans for `:kind
:tool`. `definitions` lists what the client has registered, mounted or not.

## Limitations

- SBCL cannot reclaim a thread stuck past interrupts, so one stuck for good
  holds its thread until the process restarts.

[^errors]: The error reasons are `:timeout`, `:cancelled`, `:unavailable`,
    `(:bad-request msg)` and `(:backend-error status detail [:retry-after ms])`.
    `check-request` runs in `complete` and again in the handler, so a service
    reached by a bare `m:call` sees the same checked request.
