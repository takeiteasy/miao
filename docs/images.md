# Image generations

An image generation is a [generation](checkpoints.md) that also carries
the running image itself -- SBCL's `save-lisp-and-die` -- so a rollback
can undo code, not only declared state.

```lisp
(miao:save-image *ctx*)
;; => #P"~/.miao/generations/20260923-105642-113500.core"
```

## `save-image`

`(save-image context &key dir label keep (timeout 5))`:

1. Refuses unless called on the main thread.
2. Refuses if a mounted [provider](providers.md) holds an `:api-key`
   initarg -- a core file is a copy of the whole heap, and keeping a
   credential off it is the same line [providers](providers.md#credentials)
   and [`tool-image`](introspection.md) already hold for published state.
3. Takes a declared-state [checkpoint](checkpoints.md) of `context`.
4. [`m:suspend`](https://github.com/takeiteasy/meow/blob/trunk/docs/suspend.md)s
   `context`'s whole tree, then waits up to `timeout` seconds for any
   thread outside that tree to exit on its own -- a just-stopped
   context's thread still mid-unwind, say
   ([#72](https://todo.sr.ht/~takeiteasy/miao/72)). Past `timeout`,
   refuses, naming the threads still running: `save-lisp-and-die` and
   `fork(2)` both need this one alone.
5. Forks. The child `save-lisp-and-die`s a `.core` next to the
   checkpoint's `.generation`; the parent waits for it and
   [resumes](https://github.com/takeiteasy/meow/blob/trunk/docs/suspend.md)
   the suspended tree.

The calling process is unaffected either way: every service is suspended
for the fork and resumed again before `save-image` returns, whether the
fork succeeded, failed, or never ran because of a stray thread.

## Relaunching

`(relaunch core)` probes `core` in a subprocess and, if it loads cleanly,
kills the process's live `tool-repl` workers and running `tool-shell`
commands, then `execv`s into it. Never
returns on success.

The saved core's own toplevel, on load:

1. Exits at once if `MIAO_IMAGE_PROBE` is set -- `relaunch` and
   [the launcher](launcher.md) both use this to check a core loads without reviving its
   services.
2. Otherwise, `forget-workers` marks every worker the heap holds as stale --
   its pid and pipes belong to the process that saved it, so it is never
   signalled -- then `cl+ssl:reload`s (foreign libraries need re-initialising
   after a reload), `m:resume`s the tree `save-image` suspended -- the
   same process and service instances the core's heap already holds, so
   nothing is rediscovered -- and falls through to SBCL's own toplevel,
   so `--eval`, `--non-interactive` and a plain REPL all work as usual.

## `generations`

[`generations`](checkpoints.md) reports each entry's `:image`, the
sibling `.core`'s path, or nil if none was taken. `:keep` prunes a core
alongside its generation.

## Launching

`miao` starts the newest generation, or the recovery image built by
`miao install` -- see [the launcher](launcher.md).

## Trust posture

Reaches everywhere `tool-self`'s `:eval` already does -- taking an image
is `:operator`-level, done from the REPL
([`self-define`](self.md#self-define-and-require-image)), not a
model-reachable tool op.

## Limitations

- SBCL only. `image-generation.lisp` is loaded only under `#+sbcl`; other
  implementations have declared-state [generations](checkpoints.md) but no
  image ones ([#221](https://todo.sr.ht/~takeiteasy/miao/221),
  [#222](https://todo.sr.ht/~takeiteasy/miao/222)).
- Needs a current SBCL build. 2.2.9 (Debian's `apt` package as of this
  writing) segfaults inside `save-lisp-and-die`'s own C runtime; 2.6.8 is
  known good.
- A thread `context`'s own tree doesn't own -- another mounted tree's,
  meow's hmr watcher -- still blocks `save-image` for up to `timeout`
  seconds, then refuses, naming it.
- A `tool-repl` session does not survive a relaunch: its first call reports
  the session lost and the next starts empty. Sockets never cross a save --
  each is opened and closed within one message.
- Vault claims on an agent's queued steers are taken again by the launched
  core under its own token ([vault](vault.md#claims)); one another running
  process holds is dropped from the queue.
- A core is tens of megabytes; taking one is not free, and the launcher's
  probe launches a whole second SBCL process.
