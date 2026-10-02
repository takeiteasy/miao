# Chat

`miao chat` is a line-by-line chat with one [agent](agent.md). It streams the
answer, shows tool calls and their results, and takes a line typed mid-run as
a steer, and [journals each run](#saving-and-resuming) so a chat can be resumed.
It is the secondary system `miao/cli`, drawn from
[client state](client-state.md).

```sh
miao chat --model ollama:llama3.2 --tools tool-fs
```

```
> how many .lisp files are here?
[fs-list (:PATH ".")]
[result (:OK (:ENTRIES ...))]
There are 41.
> and in tests/?
```

## Options

The options of [`miao run`](cli.md#options), without `PROMPT` and `-v`, and
`--resume`. A prompt argument is a usage error (exit 2).

| Option | Meaning |
|---|---|
| `--resume [ID]` | carry on a [saved chat](#saving-and-resuming): the newest, or the one named `ID`; not with `--model`, `--tools`, `--system-file`, `--system-replace` or `--max-turns` |

## Keys

| Input | While idle | While a run is under way |
|---|---|---|
| a line | starts a run that carries on from the conversation | [steers](ui.md#commands) the run |
| an empty line | ignored | ignored |
| Ctrl-C | leaves the chat | cancels the run, and the chat carries on |
| Ctrl-D | leaves the chat | leaves the chat |

The agent is mounted as `:chat` and holds the conversation, so each line sees
what came before. It is unmounted when the chat ends.

## Output

| Line | Meaning |
|---|---|
| text | the answer, as it streams |
| `[name args]` | a tool call, arguments shortened to 200 characters |
| `[result ...]` | what it answered, shortened the same way |
| `[turn-retry ...]`, `[context-trimmed ...]` and the like | notices from the run |
| `[run ended: reason]` | the run stopped for any reason but `:stop` |
| `> ` | the run is over; the next line goes to the model |

An existing install needs `miao install` again to get `chat` and `chats` into its saved core.

## Saving and resuming

A chat is a folder under `$MIAO_HOME/chats/` holding the agent's
[journal](journal.md) and the options it was started with.[^save] The journal is
written as the chat goes, so a run cut short by Ctrl-D or a crash is kept. A chat
that runs nothing leaves no folder.

```sh
miao chats
# 20260925-191231-123456-482  2026-09-25T18:12:40Z  how many .lisp files are here?
miao chat --resume                        # the newest chat
miao chat --resume 20260925-191231-123456-482
```

`miao chats` prints one line per chat, newest first: its id, when its journal was
last written and its first line. `--resume` prints the saved conversation as it
was drawn, then a prompt; the next line carries on from it. The model, tools,
`--max-turns` and system prompt are the saved ones, and a resumed chat keeps
journaling into its own folder.

| Exit | When |
|---|---|
| 2 | nothing saved, no chat with that id, or `--resume` with an option that describes a new agent |
| 1 | the saved options or journal could not be read, or the provider or a tool could not be mounted again |

A provider's API key is never saved: it comes from the same environment
variable as in a new chat.

## Limitations

- A line that reads as Lisp is sent as a prompt
  ([#204](https://todo.sr.ht/~takeiteasy/miao/204)).
- The chat draws the [text the provider streams](protocols.md#streaming), so an
  answer that is not streamed is not shown
  ([#205](https://todo.sr.ht/~takeiteasy/miao/205)).
- The options of a resumed chat cannot be changed
  ([#207](https://todo.sr.ht/~takeiteasy/miao/207)).
- Saved chats are never deleted or pruned
  ([#208](https://todo.sr.ht/~takeiteasy/miao/208)).

[^save]: `journal.log` and `options.sexp`. The options are `--model`, `--tools`,
    `--max-turns` and the system prompt's text, so a deleted `--system-file`
    does not stop a resume. A resume adds the conversation to the journal again,
    which [compaction](journal.md#compacting) folds.
