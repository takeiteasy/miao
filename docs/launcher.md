# The launcher

`miao` starts miao from the newest [image generation](images.md), falling
back to a recovery image if it will not load. It is a [Roswell](https://github.com/roswell/roswell)
script, `miao.ros`.

```sh
miao install                        # build ~/.miao/images/recovery.core
miao                                # newest generation, else recovery
miao --core path/to/some.core       # a specific core
miao -- --eval '(+ 1 2)'            # arguments after -- reach sbcl
miao run "list the files" -v        # one-shot agent run, see [cli.md](cli.md)
miao chat                           # an interactive chat, see [chat.md](chat.md)
miao chats                          # the saved chats, see [chat.md](chat.md#saving-and-resuming)
```

## Installing

```sh
ros install communal-software/miao         # puts miao on ~/.roswell/bin
ln -s ~/git/miao/miao.ros ~/.local/bin/miao   # or from a checkout
```

`install` builds the recovery image with `miao` and its dependencies loaded
through Quicklisp (`$QUICKLISP_SETUP`, default `~/quicklisp/setup.lisp`).

## Choosing a core

| Step | Result |
|---|---|
| `--core F` given, `F` missing | error |
| `--core F`, else the newest `generations/*.core` | probed |
| the probe fails | warning on stderr, the recovery image is used |
| no candidate | the recovery image |
| no recovery image | error naming `miao install` |

The probe runs the core with `MIAO_IMAGE_PROBE` set, as
[`relaunch`](images.md#relaunching) does, so a core that will not load never
replaces a working process. `MIAO_HOME` (default `~/.miao`) holds
`generations/` and `images/recovery.core`.

The launcher `exec`s the core in Roswell's SBCL, the runtime `relaunch` execs
too.[^runtime] It does not load miao itself.

## Limitations

- A core saved by another SBCL build does not load, so it falls back to
  recovery; rebuild it with `miao install`.
- The probe starts a second SBCL process per launch.

[^runtime]: An SBCL core loads only in the runtime build that saved it, so
    cores saved from a `sbcl` on `PATH` of another version fail the probe.
