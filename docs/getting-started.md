# Getting started

## Loading

miao loads through Quicklisp's local projects, alongside
[meow](https://github.com/communal-software/meow),
[cl-inference](https://github.com/communal-software/cl-inference) (the model client)
and its own out-of-dist dependency:

```sh
ln -s ~/git/miao ~/quicklisp/local-projects/miao
ln -s ~/git/meow ~/quicklisp/local-projects/meow
ln -s ~/git/cl-inference ~/quicklisp/local-projects/cl-inference
git clone https://github.com/communal-software/trivial-high-precision-timer \
    ~/quicklisp/local-projects/trivial-high-precision-timer
```

```lisp
(ql:quickload :miao)
```

Dependencies: `meow`, `cl-inference/client` (the contract, wire protocols,
providers and [schema](schema.md)), `alexandria`, [`jzon`](https://github.com/Zulu-Inuoe/jzon)
for JSON, [`drakma`](https://edicl.github.io/drakma/) with `flexi-streams`,
`usocket`, `puri`, `chunga` and `cl+ssl` for HTTP (`tool-http` opens its own
connection so its deadline can close it; see [tools](tools.md)), and
`bordeaux-threads` (bt2 API) for the tool deadlines. meow pulls in
`closer-mop` and `trivial-high-precision-timer`, which is not in a Quicklisp
dist and needs the local project above. `cl+ssl` needs OpenSSL for HTTPS.

The [tools](tools.md) mount into a meow context:

```lisp
(defvar *tools* (meow:start-service (make-instance 'meow:context :name :tools)))
(meow:mount *tools* 'miao:tool-fs :root "/srv/workspace")
(meow:mount *tools* 'miao:tool-shell)
(meow:mount *tools* 'miao:tool-http)
(meow:mount *tools* 'miao:tool-eval)
(meow:mount *tools* 'miao:tool-repl)
(miao:invoke-tool :tool-shell :cmd "echo hello")
```

A [protocol](protocols.md) mounts as a `backend-service`, and carries its
backend in the request:

```lisp
(meow:mount *tools* 'miao:backend-service :name :protocol-openai)
(miao:complete :protocol-openai
  :base-url "http://127.0.0.1:11434/v1"
  :model "llama3.2"
  :messages '((:role :user :content "hello")))
```

A [provider](providers.md) carries that backend for you:

```lisp
(meow:mount *tools* 'miao:backend-service :name :ollama :model "llama3.2")
(miao:complete :ollama :messages '((:role :user :content "hello")))
```

An [agent](agent.md) runs a turn cycle over a model and its tools:

```lisp
(miao:run-agent *tools* :model :ollama :tools '(:tool-shell)
                :messages '((:role :user :content "list the files")))
```

Runs on SBCL.

## Launching

`miao install` builds a recovery image once, and `miao` launches miao from
the newest saved [image generation](images.md) afterwards, falling back to
recovery if it won't load -- see [the launcher](launcher.md):

```sh
miao install
miao
```

## Tests

The suite uses FiveAM and runs through ASDF:

```lisp
(asdf:test-system :miao)
```

From the shell, `tests/test.sh` runs it and exits non-zero on failure:

```sh
tests/test.sh
```

The script runs Roswell's SBCL when `ros` is installed, the runtime the
[launcher](launcher.md) runs cores in, and the `sbcl` on `PATH` otherwise.
`MIAO_TEST_LISP=sbcl` or `MIAO_TEST_LISP=ros` chooses.[^runtime]

Tests that make real network requests are skipped unless `MIAO_LIVE_HTTP` is
set, and the live Ollama test unless `CL_INFERENCE_OLLAMA_NATIVE_URL` (native
`/api/chat`) is:

```sh
MIAO_LIVE_HTTP=1 tests/test.sh
CL_INFERENCE_OLLAMA_NATIVE_URL=http://127.0.0.1:11434 tests/test.sh
```

`CL_INFERENCE_OLLAMA_MODEL` names the model, and defaults to `llama3.2`. A backend that
does not have that model skips the live tests rather than failing them.

[^runtime]: A saved core only loads in the SBCL build that saved it, so the
    image tests save and load cores in the runtime the suite runs under. CI
    has no Roswell and runs `sbcl`.
