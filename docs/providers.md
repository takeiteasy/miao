# Providers

A provider is data: a protocol to speak, a base URL, how to authenticate, a
model catalogue and any quirks. `define-provider` is
[cl-inference/client](https://github.com/takeiteasy/cl-inference/blob/trunk/docs/client.md#providers)'s
macro, re-exported here. A new backend is a few lines rather than a new adapter.

```lisp
(miao:define-provider :ollama
  :protocol :protocol-ollama
  :base-url "http://127.0.0.1:11434"
  :auth :none
  :models '("llama3.2" "qwen2.5-coder" "gemma3")
  :summary "Local Ollama, native chat endpoint")
```

The client registers the provider under `:ollama`. Mounting it as a
[service](protocols.md#mounting-a-backend) binds it to a model:

```lisp
(meow:mount *context* 'miao:backend-service :name :ollama :model "llama3.2")
(miao:complete :ollama :messages '((:role :user :content "hello")))
```

A provider whose protocol is not registered answers `(:error :unavailable)`.
`ensure-mounted` mounts a registered provider by name, and a
[`init.lisp`](cli.md) that calls `define-provider` makes it a `--model` prefix.

## The declaration

| Key | Meaning |
|---|---|
| `:protocol` | required; the registered protocol or provider to delegate to. A provider over a provider layers both declarations' headers and defaults. |
| `:base-url` | required; the API root, http or https |
| `:auth` | `:none` (the default), `(:bearer :env "VAR")`, `(:header "name" :env "VAR")` |
| `:models` | the catalogue, for discovery |
| `:defaults` | sampling parameters layered under each request |
| `:headers` | extra headers every request carries |
| `:rewrite-request` / `:rewrite-response` | quirk hooks |
| `:summary` | one line, for discovery |

Every value is evaluated when the definition loads, and the whole declaration
is checked there: a missing `:base-url`, an auth kind outside the three, a
`:defaults` that is not a plist or a key outside the vocabulary is a definition
error rather than a surprise at the first turn.

`:models` is advertisement, not a gate. The real catalogue is whatever the
backend has, which only the running backend knows.

## Mount options

`:base-url`, `:model` and `:api-key` override the declaration at mount time, so
one definition serves a local backend, a remote host and a proxy.
`:max-in-flight` caps the completions it runs at once, queueing the rest, as
[any service's](protocols.md#concurrency) does:

```lisp
(meow:mount *context* 'miao:backend-service :name :ollama
            :base-url "http://gpu.lan:11434"
            :model "qwen2.5-coder")
```

## Credentials

Keys are BYOK. A key comes from the environment variable the declaration names,
or from an `:api-key` mount option that overrides it. Keys are never read from
the user config file, which is startup code and should not also be a secret
store, and never appear in metadata — `:auth` publishes the kind, the header
name and the variable, nothing more. A [checkpoint](checkpoints.md#credentials)
leaves `:api-key` out of a generation, so a service mounted again by a rollback
takes its key from the variable unless the rollback is given one.

A provider whose key is absent still mounts, so discovery lists it and the
failure is legible:

```lisp
(getf (miao:describe-provider :example) :status)   ; => :unavailable
(miao:complete :example :messages '(...))
;; => (:error (:bad-request "no API key; set EXAMPLE_API_KEY or pass :api-key"))
```

`(:bad-request ...)` rather than `:unavailable`: a misconfigured provider and an
unreachable backend are different problems, and only one is worth retrying.

## Layering

A provider layers its data *under* the request, so an explicit key from the
caller always wins:

```lisp
(miao:complete :ollama
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
(miao:providers)                        ; => (:ollama)
(miao:describe-provider :ollama)
(miao:definitions :kind :provider)      ; => (:ollama), mounted or not
```

`providers` scans registration props for the mounted services of `:kind
:provider`, the way `tools` and `protocols` scan for theirs.

## Ollama

`:ollama` is `http://127.0.0.1:11434` with no key, speaking `:protocol-ollama`
— the backend a development machine can run end to end, counters and options
included. The OpenAI-compatible `/v1` route has no provider of its own:
`:protocol-openai` takes `:base-url` per request, so one mounted service
answers for it.

Set `CL_INFERENCE_OLLAMA_NATIVE_URL` to run the live test for this provider, and
`CL_INFERENCE_OLLAMA_MODEL` to name the model. The client's own suite uses the
same variables for its live tests.

## Limitations

- `:defaults` keys the protocol does not advertise are dropped on the wire
  rather than refused, since a protocol takes only what it knows.
- Auth is BYOK. OAuth and other interactive flows are
  [#24](https://todo.sr.ht/~takeiteasy/miao/24).
