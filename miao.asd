(defsystem "miao/launcher"
  :description "Core selection for the miao launcher."
  :author "George Watson"
  :license "GPLv3"
  :depends-on ("uiop")
  :pathname "launcher/"
  :serial t
  :components ((:file "package")
               (:file "launcher")))

(defsystem "miao"
  :description "Model Integration And Orchestration: an agent core built on meow."
  :author "George Watson"
  :license "GPLv3"
  :version "0.1.0"
  :depends-on ("meow" "meow/logger" "alexandria" "com.inuoe.jzon" "drakma" "flexi-streams"
               "usocket" "bordeaux-threads" "uiop" "puri" "chunga" "cl+ssl" "cl-base64" "cffi" "babel" "md5"
               "closer-mop" "trivial-arguments" "atomics" "miao/launcher")
  :serial t
  :components ((:file "package")
               (:file "posix")
               (:file "miao")
               ;; Ahead of worker.lisp and tools/shell.lisp: both launch and
               ;; kill through the process-group helpers declared here.
               (:file "process")
               ;; Ahead of tool.lisp, which tests for a token.
               (:file "cancel")
               ;; Ahead of protocol.lisp and agent.lisp, which submit to it.
               (:file "pool")
               (:file "schema")
               ;; Ahead of tool.lisp, provider.lisp and the protocols: their
               ;; DEFINE- macros record here.
               (:file "definitions")
               (:file "tool")
               ;; Right after tool.lisp: CHECKPOINT and ROLLBACK need only
               ;; the SNAPSHOT/RESTORE convention it declares, and every
               ;; tool, protocol and provider file below can then use them
               ;; without a forward reference. The agent's own SNAPSHOT
               ;; method lives in agent.lisp instead, where its slots are.
               (:file "checkpoint")
               ;; After checkpoint.lisp: the vault's log uses its shared
               ;; %APPEND-LOG/%READ-LOG. Ahead of agent.lisp, which records
               ;; and folds a steer through it.
               (:file "vault")
               (:file "journal")
               ;; Ahead of agent.lisp, which runs the hooks it defines.
               (:file "hook")
               (:file "protocol")
               (:file "worker")
               ;; Ahead of tools/gated-eval.lisp, which checks a form through it.
               (:file "gate")
               (:static-file "worker-program.lisp")
               (:module "tools"
                :components (;; Ahead of "fs": the atomic sandbox walk it
                             ;; uses is declared here.
                             (:file "fs-posix")
                             (:file "fs")
                             (:file "shell")
                             (:file "http")
                             (:file "eval")
                             (:file "gated-eval")
                             (:file "repl")
                             (:file "plan")
                             (:file "image")
                             (:file "services")
                             (:file "checkpoint")
                             (:file "self")
                             (:file "vault")
                             (:file "calls")))
               (:module "hooks"
                :components ((:file "approval")))
               (:module "protocols"
                :components ((:file "openai")
                             (:file "ollama")))
               ;; After the protocols: a provider layers its data onto one,
               ;; and DEFINE-PROVIDER is a macro, so :serial order is what
               ;; makes both available to a definition. The shared helpers
               ;; both protocols use -- name/key conversion, JSON value
               ;; coercion, the tools array, the deadline-bounded exchange --
               ;; live in protocol.lisp, ahead of either.
               (:file "provider")
               (:module "providers"
                :components ((:file "ollama")))
               ;; After providers: the loop reaches a model by name through
               ;; COMPLETE, and defaults its tool allow-list from the
               ;; discovered tools, so both must already be defined.
               (:file "agent")
               ;; Last: SAVE-IMAGE needs CHECKPOINT (checkpoint.lisp),
               ;; M:SUSPEND/M:RESUME, and PROVIDER-API-KEY (provider.lisp)
               ;; to refuse a credentialed mount.
               #+sbcl (:file "image-generation"))
  :in-order-to ((test-op (test-op "miao/tests"))))

(defsystem "miao/cli"
  :description "The miao command line: run and chat."
  :author "George Watson"
  :license "GPLv3"
  :depends-on ("miao" "miao/ui" "alexandria" "bordeaux-threads" "uiop")
  :pathname "cli/"
  :serial t
  :components ((:file "package")
               (:file "run")
               (:file "chat")
               (:file "main")))

(defsystem "miao/ui"
  :description "Headless client state for miao front ends."
  :author "George Watson"
  :license "GPLv3"
  :depends-on ("miao" "alexandria" "bordeaux-threads")
  :pathname "ui/"
  :serial t
  :components ((:file "package")
               (:file "state")
               (:file "client")))

(defsystem "miao/tests"
  :depends-on ("miao" "miao/cli" "miao/ui" "fiveam" "uiop" "usocket" "cffi")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "schema")
               (:file "definitions")
               (:file "launcher")
               (:file "smoke")
               (:file "protocol")
               (:file "fake-http")
               (:file "protocol-openai")
               (:file "protocol-ollama")
               (:file "provider")
               ;; After provider: it runs completions through the echo provider.
               (:file "pool")
               (:file "cli")
               (:file "agent")
               (:file "worker")
               (:file "gate")
               (:file "tools")
               ;; After tools: it uses that file's sandbox fixture.
               (:file "posix")
               (:file "plan")
               (:file "introspect")
               (:file "checkpoint")
               (:file "vault")
               (:file "calls")
               (:file "journal")
               (:file "detach")
               (:file "events")
               (:file "ui")
               (:file "chat")
               (:file "resume")
               (:file "hooks")
               (:file "approval")
               (:file "inputs")
               #+sbcl (:file "image-generation")
               (:file "self"))
  :perform (test-op (o c)
             (unless (symbol-call :fiveam :run! :miao)
               (error "miao tests failed"))))
