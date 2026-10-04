# Getting started

## Loading

miao loads through Quicklisp's local projects, alongside
[meow](https://github.com/takeiteasy/meow) and its own out-of-dist
dependency:

```sh
ln -s ~/git/miao ~/quicklisp/local-projects/miao
ln -s ~/git/meow ~/quicklisp/local-projects/meow
git clone https://github.com/takeiteasy/trivial-high-precision-timer \
    ~/quicklisp/local-projects/trivial-high-precision-timer
```

```lisp
(ql:quickload :miao)
```

Dependencies: `meow`, `alexandria`, [`jzon`](https://github.com/Zulu-Inuoe/jzon)
for JSON, including the [schema](schema.md) rendering, [`drakma`](https://edicl.github.io/drakma/) with `flexi-streams`,
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

A [protocol](protocols.md) mounts the same way, and carries its backend in the
request:

```lisp
(meow:mount *tools* 'miao:protocol-openai)
(miao:complete :protocol-openai
  :base-url "http://127.0.0.1:11434/v1"
  :model "llama3.2"
  :messages '((:role :user :content "hello")))
```

A [provider](providers.md) carries that backend for you:

```lisp
(meow:mount *tools* 'miao:protocol-ollama)
(meow:mount *tools* 'miao:provider-ollama :model "llama3.2")
(miao:complete :provider-ollama :messages '((:role :user :content "hello")))
```

An [agent](agent.md) runs a turn cycle over a model and its tools:

```lisp
(miao:run-agent *tools* :model :provider-ollama :tools '(:tool-shell)
                :messages '((:role :user :content "list the files")))
```

Runs on SBCL and ECL; [image generations](images.md) are SBCL only. The ECL
test suite does not pass yet ([limitations](#limitations)).

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
`MIAO_TEST_LISP=sbcl`, `MIAO_TEST_LISP=ros` or `MIAO_TEST_LISP=ecl` chooses.[^runtime]

Tests that make real network requests are skipped unless `MIAO_LIVE_HTTP` is
set, and the live protocol tests unless `MIAO_OLLAMA_URL` (the `/v1` route) or
`MIAO_OLLAMA_NATIVE_URL` (native `/api/chat`) is:

```sh
MIAO_LIVE_HTTP=1 tests/test.sh
MIAO_OLLAMA_URL=http://127.0.0.1:11434/v1 \
MIAO_OLLAMA_NATIVE_URL=http://127.0.0.1:11434 tests/test.sh
```

`MIAO_OLLAMA_MODEL` names the model, and defaults to `llama3.2`. A backend that
does not have that model skips the live tests rather than failing them.

## Limitations

- The ECL test suite does not pass: a threaded agent run crashes it
  ([#231](https://todo.sr.ht/~takeiteasy/miao/231), [#232](https://todo.sr.ht/~takeiteasy/miao/232)).
- On ECL request bodies and rendered schemas do not keep key order
  ([#230](https://todo.sr.ht/~takeiteasy/miao/230)), and a connect is bounded only by the whole-exchange
  deadline ([#229](https://todo.sr.ht/~takeiteasy/miao/229)).
- iOS is not supported: HTTPS needs a TLS backend ([#226](https://todo.sr.ht/~takeiteasy/miao/226)) and the
  shell and worker tools need a profile without subprocesses
  ([#227](https://todo.sr.ht/~takeiteasy/miao/227)).

[^runtime]: A saved core only loads in the SBCL build that saved it, so the
    image tests save and load cores in the runtime the suite runs under. CI
    has no Roswell and runs `sbcl`.
