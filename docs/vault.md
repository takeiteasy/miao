# Vault: the steering message log

An append-only s-expression log of [steering](agent.md#messages) messages,
each marked consumed once it has been folded into a run or discarded. It
backs an agent's own in-memory steer queue, so a steer survives past the
run it was sent to, a crash, or a restart -- not just the next turn. See
[~takeiteasy/miao#14](https://todo.sr.ht/~takeiteasy/miao/14).

```lisp
(m:mount *ctx* 'miao:agent :name :assistant :model :provider-ollama :vault t)
(m:mount *ctx* 'miao:tool-vault)

(m:cast (m:lookup :assistant) '(:steer :content "focus on the tests"))
(miao:invoke-tool :tool-vault :op :list)
;; => (:ok (:entries ((:id "..." :agent :assistant :content "focus on the tests"
;;                      :status :pending ...)) :total 1))
```

## `:vault`, the agent mount option

Off (`nil`) by default: steering stays in-memory only, exactly as before
this landed. `t` records to the default log, `~/.miao/vault.log`, resolved
lazily so mounting an agent never touches the filesystem on its own. A
string or pathname records there instead.

A steer records before it queues, and is marked `:folded` at the point
`issue-turn` actually pushes it onto the conversation -- not when `:steer`
is sent. A steer queued while a turn is already in flight, with no further
turn to fold it into before the run ends, stays `:pending` and is folded on
the agent's next turn; nothing marks it consumed until then. It is
[claimed](#claims) while it waits.

A delegated sub-agent (agent.lisp's `agent-task`) inherits its parent's
`:vault`, the same way it inherits the model and allow-list. A child of a named
parent is registered as `<parent>/<n>`, so a steer recorded against it carries
that name as `:agent` and can be restored to it. A child of an unnamed parent
has none, and restoring one needs an explicit `:agent` at the tool.

`:steer` also now folds in even when it was queued before `:run`: starting
a run no longer clears the queue, only the turn/tool-call state that
belongs to the run itself. A steer sent to an idle agent waits and folds in
after the seed messages, on the first turn.

## The log

```lisp
(:kind :steer    :id "20260923-140501-822931" :at "2026-09-23T14:05:01Z"
 :agent :assistant :content "focus on the tests")
(:kind :consumed :id "20260923-140501-822931" :at "2026-09-23T14:05:03Z"
 :how :folded)
```

A steer sent with an `:input-id` carries it as `:input-id` on its `:steer`
line, and a redelivery is [a duplicate](inputs.md), not a second entry.

`vault-entries` folds the log into current state, the same way
[`generations`](checkpoints.md) derives its list from files on disk rather
than an index. `:how` is `:folded` or `:discarded`. Read with `*read-eval*`
bound to nil, the same guard a generation and
[`tool-self`'s log](self.md#checkpoint-and-log) both apply, through the
same shared `%append-log`/`%read-log` helpers checkpoint.lisp declares -- so
a vault entry can never run code merely by being read back. Appends to one
log file serialise behind a lock of their own, keyed by the file's canonical
name, so different logs never wait on each other. The lock is also an
`flock` on a sidecar `<log>.lock` file, so other miao processes appending to
or compacting the same log serialise too.

## Claims

A steer waiting in an agent's queue, or one `:restore` has cast at an agent,
is claimed: still `:pending` in the log, with `:claimed t` in
`vault-entries`, but it cannot be restored or discarded. Claims are
persisted in the log, so other miao processes see them, and are taken under
the log's lock:

```lisp
(:kind :claimed  :id "..." :at "..." :by (:pid 4242 :host "box" :start 1790178586000000 :token "..."))
(:kind :released :id "..." :at "...")
```

A steer an agent records for itself carries the same owner as `:claimed-by`
on its `:steer` line, so recording and claiming are one append. The latest
claim or release wins, and `:consumed` ends it. `vault-release-all` releases
several claims of this image under one read of the log. A claim is released when the
agent is rolled back or stopped and its queue is dropped; that steer can be
restored again.

The owner is a pid, a host, the process start time and a random per-image
token. A claim is live when its token is this image's, its host is another
machine, or its pid is still running on this one and, when both are known,
started at the recorded time. A claim or process with no readable start time
(an unsupported OS) is judged by its pid alone. A claim whose owner has exited is ignored, so a
crashed process never strands an entry.

A saved image starts each launched core with a fresh token. A claim naming
this process's pid under another token belongs to an image a
[relaunch](images.md) replaced, so it is dead. A launched core claims its
agents' queued steers again as itself, dropping any that another running
process holds or that are already consumed.

## Compaction

`(miao:vault-compact path :max-age seconds)` rewrites the log without the
steers consumed more than `max-age` seconds ago (default `*vault-max-age*`,
7 days; `0` drops every consumed steer) and their `:consumed` lines.
Pending steers are always kept, with their live claim folded onto the
`:steer` line and their `:claimed`/`:released` lines dropped. It answers the steers dropped and kept.

An append also compacts once the file passes `*vault-compact-size*` (1 MiB)
and has doubled since the last attempt. A log with a malformed entry is
never rewritten -- that would lose everything past the entry -- so
`vault-compact` answers nil and the file is left as it is.

## `tool-vault`

`:trust :operator`: restoring injects a `:user`-role message into whichever
agent is named, and listing surfaces operator-written steering text -- the
same posture as [`tool-checkpoint`](checkpoints.md#tool-checkpoint)'s own
writes to harness state.

| `:op` | Params | Answers |
|---|---|---|
| `:list` | `:status` (default `:pending`), `:limit` | `:entries`, `:total` |
| `:restore` | `:id` (required), `:agent` | `:agent`, `:id` |
| `:discard` | `:id` (required) | `:id` |
| `:compact` | `:max-age` (seconds, default `*vault-max-age*`) | `:dropped`, `:kept` |

```lisp
(m:mount *ctx* 'miao:tool-vault)
(miao:invoke-tool :tool-vault :op :restore :id "20260923-140501-822931")
```

`:restore` claims the entry under the log's lock, then casts
`(:steer :content ... :vault-id id :vault-path path)` at the named agent --
the id and log path travel with it, so the agent's own fold marks the
*original* entry consumed, in the log it came from, rather than this tool
recording a second one for the same steer. It is not consumed at the point
of the call; only once the target agent actually folds it in. Concurrent
restores of one id deliver it once, across processes too. `:agent`, a string, picks the target: the entry's own
recorded agent by default, or an override -- required when the entry was
recorded with no agent, as a child of an unnamed parent's is. Restoring or
discarding an id that is unknown, already consumed or claimed is a
`(:bad-request ...)`. `:discard` checks and consumes under the log's lock,
so concurrent discards of one id consume it once.

`:path` is a mount option (default nil, meaning the same default an
agent's `:vault t` uses), read once at mount time.
