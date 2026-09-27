# Providers

A provider is data: a [protocol](protocols.md) to speak, a base URL, how to
authenticate, a model catalogue and any quirks. `define-provider` turns that
declaration into a mountable [meow](https://github.com/takeiteasy/meow) service,
so a new backend is a few lines rather than a new adapter.

```lisp
(miao:define-provider :ollama
  :protocol :protocol-ollama
  :base-url "http://127.0.0.1:11434"
  :auth :none
  :models '("llama3.2" "qwen2.5-coder" "gemma3")
  :summary "Local Ollama, native chat endpoint")
```

This defines the service class `provider-ollama`, registered under
`:provider-ollama` with `:kind :provider` metadata. Mounting binds it to a model:

```lisp
(meow:mount *context* 'miao:protocol-ollama)
(meow:mount *context* 'miao:provider-ollama :model "llama3.2")
(miao:complete :provider-ollama :messages '((:role :user :content "hello")))
```

The protocol it names must be mounted too; a provider that cannot find its
protocol answers `(:error :unavailable)`.

## The declaration

| Key | Meaning |
|---|---|
| `:protocol` | required; the protocol or provider service to delegate to, a literal keyword. A provider over a provider layers both declarations' headers and defaults. |
| `:base-url` | required; the API root, http or https |
| `:auth` | `:none` (the default), `(:bearer :env "VAR")`, `(:header "name" :env "VAR")` |
| `:models` | the catalogue, for discovery |
| `:defaults` | sampling parameters layered under each request |
| `:headers` | extra headers every request carries |
| `:rewrite-request` / `:rewrite-response` | quirk hooks |
| `:summary` | one line, for discovery |

Every value is a form evaluated at load time, and the whole declaration is
checked there: a missing `:base-url`, an auth kind outside the three, a
`:defaults` that is not a plist or a key outside the vocabulary is a definition
error rather than a surprise at the first turn.

The provider name and `:protocol` must be literal keywords, since the class and
its service dependency are named from them.

`:models` is advertisement, not a gate. The real catalogue is whatever the
backend has — for Ollama, whatever has been pulled — which only the running
backend knows.

## Mount options

`:base-url`, `:model` and `:api-key` override the declaration at mount time, so
one definition serves a local backend, a remote host and a proxy.
`:max-in-flight` caps the completions it runs at once, queueing the rest, as
[a protocol's](protocols.md#concurrency) does:

```lisp
(meow:mount *context* 'miao:provider-ollama
            :base-url "http://gpu.lan:11434/v1"
            :model "qwen2.5-coder")
```

## Concurrency

A provider layers each request on its own process, then hands the call to
its protocol, which answers the caller directly and holds no thread of the
provider's. A provider with a `:rewrite-response` quirk or a `:max-in-flight`
cap instead waits on its protocol from a [pooled](protocols.md#worker-pools)
job, so it can rewrite the reply or count the completion; only then does
stopping the provider cancel what it has in flight. Otherwise the protocol's
own stop, or the caller's `:cancel`, does.

## Credentials

Keys are BYOK. A key comes from the environment variable the declaration names,
or from an `:api-key` mount option that overrides it. Keys are never read from
the user config file, which is startup code and should not also be a secret
store, and never appear in metadata — `:auth` publishes the kind, the header
name and the variable, nothing more. A [checkpoint](checkpoints.md#credentials)
leaves `:api-key` out of a generation, so a provider mounted again by a
rollback takes its key from the variable unless the rollback is given one.

A provider whose key is absent still mounts, so discovery lists it and the
failure is legible:

```lisp
(getf (miao:describe-provider :provider-example) :status)   ; => :unavailable
(miao:complete :provider-example :messages '(...))
;; => (:error (:bad-request "no API key; set EXAMPLE_API_KEY or mount with :api-key"))
```

`(:bad-request ...)` rather than `:unavailable`: a misconfigured provider and an
unreachable backend are different problems, and only one is worth retrying.

## Layering

A provider layers its data *under* the request, so an explicit key from the
caller always wins:

```lisp
(miao:complete :provider-ollama
  :model "gemma3"            ; beats the mount's :model
  :temperature 0.9           ; beats the declaration's :defaults
  :messages '((:role :user :content "hello")))
```

Headers merge rather than replace, matched without case as HTTP names are:
the declaration's `:headers` first, then the auth header, then the caller's.

## Quirks

`:rewrite-request` takes the layered request plist and returns one;
`:rewrite-response` takes the whole `(:ok ...)` or `(:error ...)` result and
returns one. Both are for a backend that is almost, but not quite, the shape its
protocol describes.

## Discovery

```lisp
(miao:providers)                        ; => (:provider-ollama)
(miao:describe-provider :provider-ollama)
```

`providers` scans registration props for `:kind :provider`, the way `tools` and
`protocols` scan for theirs.

## Ollama

`:provider-ollama` is `http://127.0.0.1:11434` with no key, speaking
[`:protocol-ollama`](protocols.md#the-ollama-protocol) — which makes it the
backend a development machine can run end to end, counters and options
included. The OpenAI-compatible `/v1` route stays reachable with no provider
of its own: `:protocol-openai` takes `:base-url` per request, so one mounted
service already answers for it.

Set `MIAO_OLLAMA_NATIVE_URL` to run the live tests for this provider, and
`MIAO_OLLAMA_MODEL` to name the model. (`MIAO_OLLAMA_URL` is the `/v1` URL the
OpenAI protocol's own live tests use.)

## Limitations

- `:defaults` keys the protocol does not advertise are dropped on the wire
  rather than refused, since a protocol takes only what it knows.
- Auth is BYOK. OAuth and other interactive flows are
  [#24](https://todo.sr.ht/~takeiteasy/miao/24).
