# Introspection

Two `:agent`-trusted, always-on, read-only tools over the running image:
`tool-image` for the CL image, `tool-services` for the
[meow](https://github.com/takeiteasy/meow) supervision tree it runs under.

```lisp
(m:mount context 'miao:tool-image)
(m:mount context 'miao:tool-services)
```

Neither ever returns a value or a slot — flags and shapes only. That holds
even for a bound special: `tool-eval`, `tool-repl` and `tool-self`, all
`:operator`, stay the way to see one.

## `tool-image`

| `:op` | Params | Answers |
|---|---|---|
| `:describe` | `:symbol` (required), `:package` | `:name`, `:package`, `:fboundp`, `:boundp`, `:kind`, `:lambda-list`, `:documentation`, `:variable-documentation`, `:source` |
| `:apropos` | `:pattern` (required), `:package`, `:external-only` (default `t`), `:limit` (default 100) | `:symbols`, `:total`, `:truncated` |
| `:documentation` | `:symbol` (required), `:doc-type` (default `:function`) | `:documentation` |
| `:source` | `:symbol` (required), `:doc-type` | `:available`, and `:file`/`:position` or `:form`/`:truncated` when it is, and `:methods` for a generic function |
| `:packages` | — | `:packages` — each loaded package's name and nicknames |

`:symbol` is `"miao:complete"` or `"complete"` against `:package` (or
`*package*`), resolved with `find-symbol` — never `read-from-string` or
`intern`, so a lookup cannot grow the image. An unresolvable `:package` or
`:symbol` is a `(:bad-request ...)`.

```lisp
(miao:invoke-tool :tool-image :op :describe :symbol "miao:complete")
;; => (:ok (:name "COMPLETE" :package "MIAO" :fboundp t :boundp nil
;;          :kind :function :lambda-list "(NAME &REST REQUEST)"
;;          :documentation "..." :variable-documentation nil
;;          :source (:available t :file "/.../protocol.lisp" :position 8572)))
```

`:kind` is the most specific of `:special-operator`, `:macro`,
`:generic-function`, `:function`, `:class`, `:constant`, `:variable` or
`:unbound` — a symbol can be several of these; `:describe` picks one.

`:apropos` is capped by `:limit`: a loose pattern against a loaded image is
thousands of symbols, so it reports `:total` and `:truncated` rather than
cutting silently.

`:source` is a `:file` and `:position` for a function loaded from a file.
One defined in the image — at a REPL, or through `tool-self` — answers its
printed lambda expression as `:form`, capped at 4000 characters with
`:truncated` set when it is cut.[^form] A function with neither reports
`:available nil` rather than erroring.

```lisp
(miao:invoke-tool :tool-image :op :source :symbol "my-fn")
;; => (:ok (:available t :form "(LAMBDA (X) (BLOCK MY-FN (1+ X)))" :truncated nil))
```

A generic function also answers `:methods`: each method's `:specializers`,
`:qualifiers` and, when it was loaded from a file, `:file` and `:position`.
It is capped at 100, with `:methods-total` and `:methods-truncated`. A method
defined in the image has no source form, only its specializers. `:describe`
carries the same under its `:source`.

```lisp
(miao:invoke-tool :tool-image :op :source :symbol "my-generic")
;; => (:ok (:available t :methods ((:specializers "(INTEGER)" :qualifiers "NIL"
;;                                   :file "/.../my.lisp" :position 120))
;;          :methods-total 1 :methods-truncated nil))
```

Lambda lists and file locations come from `sb-introspect`; `:form` comes from
`function-lambda-expression`.

## `tool-services`

| `:op` | Params | Answers |
|---|---|---|
| `:registry` | `:kind` | every registered name and its published props, sorted, optionally filtered to one `:kind` |
| `:children` | `:recursive` (default `t`) | the mount tree under this tool's own context: `:name`, `:class`, `:restart`, `:state`, `:status`, `:restart-in`, `:alive` |
| `:describe` | `:name` (required) | that name's props, `:alive`, `:status` and its effect labels |

Props are exactly what each service's `metadata` already publishes — the
same source `describe-tool` reads, key-free by construction.

```lisp
(miao:invoke-tool :tool-services :op :children)
;; => (:ok (:children ((:name :tool-fs :class "tool-fs" :restart :transient
;;                       :state :running :status :ready :restart-in nil :alive t) ...)))
```

`:state` is `m:children`'s restart bookkeeping (`:running` or
`:restarting`). `:status` is the service's own lifecycle status, `:waiting`
(a dependency is missing) or `:ready`. It is nil when the child is
restarting, has stopped or does not answer within a second.[^status] A tool mounted outside a
context — `:children` needs one to walk — answers `(:error "not mounted
under a context")`.

## Trust posture

Both tools are `:agent`-trusted: read-only introspection is the point of
always-on tools, so they carry no operator gate. What they must never do is
leak a slot — a provider's `:api-key` lives in one, and is kept out of
published metadata for exactly that reason (see
[providers](providers.md#credentials)) — so `:describe` reports `:boundp`
and `:fboundp` as flags and nothing more. `tool-services` never reads a
slot at all; everything it answers already travels through `metadata` or
`m:children`.

## Limitations

- `:children` asks each child for its status one after another, so busy
  children add up to a second each
  ([#149](https://todo.sr.ht/~takeiteasy/miao/149)).

[^status]: Asked over a call with `m:service-status`; `:starting`, `:stopping`
    and `:stopped` are never visible from another process. See
    [Limitations](#limitations).

[^form]: `:form` is the definition's own code, so a literal written into it
    (a string, say) travels with it. A variable's value never does.
