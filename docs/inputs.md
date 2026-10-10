# Redelivered inputs

A `:run` or `:steer` given an `:input-id` is accepted once. A redelivery, such
as a client retrying after a dropped response, answers with the original's
status and does not run or queue again.

```lisp
(m:mount *ctx* 'miao:agent :name :assistant :model :ollama
                           :journal t :vault t)

(m:call (m:lookup :assistant) '(:run :messages ((:role :user :content "hi"))
                                     :input-id "req-7f3a"))
;; => :ok
(m:call (m:lookup :assistant) '(:run :messages ((:role :user :content "hi"))
                                     :input-id "req-7f3a"))
;; => (:ok (:duplicate :running))   ; later, (:ok (:duplicate :stop))
```

## What is recorded

| Message | Recorded in | Needs | A redelivery answers |
|---|---|---|---|
| `(:run :messages m :input-id k)` | the [call log](calls.md#the-log), as an `:input` entry | `:journal` | `(:ok (:duplicate status))` |
| `(:steer :content c :input-id k)` | the [vault](vault.md#the-log), on the `:steer` line | `:vault` | `(:ok (:duplicate status))` |

`:input-id` is a string of the caller's choosing, unique across the callers
sharing a log.[^scope] Without it, nothing is checked. With one and no matching
log, the message is a `(:bad-request ...)`.

## Statuses

| Input | `status` | Meaning |
|---|---|---|
| run | `:running` | accepted, not finished |
| run | `:stop`, `:max-turns`, `:timeout`, `:cancelled` | the run's [stop reason](agent.md#the-result) |
| run | `:error` | the run ended with an error |
| run | `:interrupted`, `:abandoned` | closed by a [restore](checkpoints.md), or by the agent's exit |
| run | `:lost` | accepted, and the process that accepted it is gone[^lost] |
| steer | `:pending`, `:folded`, `:discarded` | as [`vault-entries`](vault.md) |

The same `:input-id` with different content (the steer's `:content`, or the
run's `:messages`) is a `(:bad-request ...)`, not a duplicate.

A duplicate `:run` is answered before the agent's "already running"
bad-request, so a redelivery during the run reads `:running`. A new keyed
`:run` on a busy agent is that bad-request and is not recorded.

## `run-agent`

`run-agent` takes `:input-id` too, and needs `:journal`. A redelivery returns
`(:ok (:duplicate status))` and starts nothing.

## `:input-id` and `:vault-id`

`:input-id` is the caller's key for an input. `:vault-id` names an entry
already in the vault, which `tool-vault`'s `:restore` uses to redeliver a
steer; it skips the check.

## Limitations

- A duplicate is answered only to `m:call`; a `m:cast` caller sees no reply.
- An id is remembered until compaction drops it: 7 days after the input
  finished ([`*journal-max-age*`](calls.md#api),
  [`*vault-max-age*`](vault.md#compaction)). A later redelivery is new
  ([#35](https://github.com/communal-software/miao/issues/35)).
- A `:lost` input is reported, not re-run
  (“Resume an in-flight tool call after a crash”).

[^scope]: One id covers one log file. A `:run` and a `:steer` are in different
    logs, so the same string may key one of each.
[^lost]: Judged as a call's is; see the [call log](calls.md#statuses).
