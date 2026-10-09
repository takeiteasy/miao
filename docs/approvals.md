# Operator approval

`:hook-approval` holds a call to an operator tool until the operator answers.
It is a [before-tool-call hook](hooks.md), so the loop is unchanged: a denial
reaches the model as the call's error, and a hook that fails stops the call.

```lisp
(m:mount *ctx* 'miao:hook-approval)
(m:mount *ctx* 'miao:agent :model :ollama
                           :tools '(:tool-shell)
                           :hooks '(:hook-approval)
                           :sink #'draw-event)

;; the sink hears (:type :approval-request :approval 1 :id "c1" :name :tool-shell ...)
(miao:answer-approval 1 :allow)
```

## Options

| Option | Default | Meaning |
|---|---|---|
| `:tools` | none | the tools to ask about; none means every tool of `:operator` [trust](tools.md) |
| `:timeout` | 600000 | milliseconds the call waits; then it fails closed with `(:error (:hook-failed :hook-approval :timeout))` |

## Answering

`(miao:answer-approval approval decision &key hook registry)` answers `:ok`, or
a `bad-request` error when no such approval is waiting.

| Decision | Effect |
|---|---|
| `:allow` | the call runs |
| `:deny` | the model gets `(:error (:denied :hook-approval "denied by the operator"))` |
| `:always` | as `:allow`, and later calls to that tool in the same run, its sub-agents' included, run unasked. It lasts until the run ends |

Approvals are answered in any order. `(m:call (m:lookup :hook-approval) '(:pending))`
lists the open ones: `:approval`, `:id`, `:name`, `:arguments`, `:agent`, `:run`.

## Events

The hook's events reach every [sink](ui.md#events) of the run, tagged with
`:hook`, `:agent` and, from a sub-agent, `:parent`.

| `:type` | Keys | When |
|---|---|---|
| `:approval-request` | `:approval`, `:id`, `:name`, `:arguments` | a call is waiting; `:id` is the call's |
| `:approval-done` | `:approval`, `:answer` | `:allow`, `:deny`, `:always`, or `:withdrawn` |

An approval is withdrawn when the agent stops waiting for it: an interrupting
steer, a restore, or the timeout. A front end closes its prompt at the
`:approval-done`, or at the root's `:run-done`: a cancel or a deadline ends the
run first, and nothing is emitted after it.

The `:approval-done` of an answer comes before the call's `:tool-call`.

## Limitations

- No front end asks yet: the [chat](chat.md) does not prompt
  ([#213](https://todo.sr.ht/~takeiteasy/miao/213)) and
  [client state](client-state.md) does not fold approvals
  ([#199](https://todo.sr.ht/~takeiteasy/miao/199)).
