# Forking a conversation

`fork-agent` branches an agent's conversation: a new agent starts from a
prefix of the original's history and carries on down its own path. The
original is only read, so it keeps its history and, if running, its run.

```lisp
(m:mount *ctx* 'miao:agent :name :assistant :model :provider-ollama)
;; ... a run has built up a conversation ...

(miao:fork-agent *ctx* :assistant :turn 2 :as :retry)   ; => :retry
(m:cast (m:lookup :retry)
        '(:run :continue t :messages ((:role :user :content "try again, shorter"))))
```

The fork is mounted beside the original, the way it was mounted[^mount], and
is a fresh agent holding the prefix, so `(:run :continue t)` carries on from
the cut. Two forks from one point are independent of each other and of the
original.

## Choosing the cut

| Key | Keeps | Use it to |
|---|---|---|
| `:at n` | the first `n` messages | cut anywhere, e.g. before an assistant reply to regenerate it |
| `:turn n` | everything up to and including the `n`th assistant turn and its tool results | branch after a turn; `:turn 0` keeps what precedes the first |
| neither | the whole conversation | try a different next message |

Give one of `:at` and `:turn`. A cut may not part an assistant turn that made
tool calls from its `:tool` replies: `:at 3` in `system, user, assistant
(with calls), tool, ...` is an error, as is a number outside the conversation.

`fork-conversation` is the same cut without an agent. It takes a list of
messages, such as a `run-agent` result's `:messages`, and returns the prefix
and how many assistant turns it holds:

```lisp
(miao:fork-conversation messages :turn 1)   ; => prefix, 1
```

## A running agent

Forking reads a [snapshot](checkpoints.md), so it works mid-run. A tool call
that has not answered is in the fork closed as `{"error":"interrupted"}`, and
the original's call carries on.

## Limitations

- A fork lives in memory. It is [checkpointed](checkpoints.md) like any
  agent, but the branches of a conversation are not kept as a tree
  ([#177](https://todo.sr.ht/~takeiteasy/miao/177)).
- A model cannot fork; there is no agent message or tool for it
  ([#178](https://todo.sr.ht/~takeiteasy/miao/178)).

[^mount]: Same class, options and restart policy, so `:sink`, `:vault` and
    `:journal` are shared with the original: a fork's events reach the same
    sink, and its steering and tool calls are recorded in the same logs. Mount options are read from
    the original's mount, so a credential-holding option is copied too.
