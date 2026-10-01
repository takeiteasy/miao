(defpackage #:miao
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:m #:meow)
                    (#:json #:com.inuoe.jzon)
                    (#:bt #:bordeaux-threads-2))
  (:export
   #:*version*
   ;; tool convention
   #:tools #:describe-tool #:invoke-tool
   #:tool-error #:tool-error-p #:tool-trust #:tool-schema
   #:define-tool
   #:define-protocol #:definitions #:ensure-mounted
   ;; protocol convention
   #:protocols #:describe-protocol #:complete #:check-request
   #:define-protocol-handler #:completion-host #:backend-error
   #:normalize-content #:content-text #:text-block
   #:text-delta #:tool-call-delta #:done #:emit-event
   #:make-cancel-token #:cancel #:cancelled-p #:on-cancel
   ;; worker pools
   #:*pool-size* #:*sink-pool-size* #:*max-completion-depth* #:carry-completion-depth
   #:*pool-idle-seconds* #:pool-stats
   ;; provider convention
   #:define-provider #:providers #:describe-provider #:provider
   ;; hook convention
   #:hooks #:describe-hook #:define-hook #:hook-function
   ;; agent convention
   #:agent #:agents #:describe-agent #:run-agent
   #:fork-conversation #:fork-agent
   #:run-start-event #:steer-event #:turn-event #:tool-call-event #:tool-result-event #:run-done-event
   ;; checkpoints
   #:snapshot #:restore #:secret-initargs #:checkpoint #:rollback #:generations #:generation-id
   #:*generations-directory*
   ;; image generations
   #:save-image #:relaunch #:save-recovery-image
   #:self-define
   ;; vault
   #:vault-record #:vault-consume #:vault-consume-pending
   #:vault-claim-pending #:vault-release #:vault-release-all #:vault-entries #:vault-compact #:*vault-log*
   #:*vault-max-age* #:*vault-compact-size*
   ;; call log
   #:call-entries #:input-entries #:call-log-compact #:*call-log* #:*call-log-max-age*
   #:*call-log-compact-size* #:*call-log-max-content*
   ;; parameter schemas
   #:validate-schema #:coerce-args
   #:schema->json-schema #:json-schema->schema
   #:array-of #:object #:map-of #:any
   ;; the allowlist gate
   #:gate-check
   ;; workers
   #:*worker-command*
   ;; protocols
   #:protocol-openai
   #:protocol-ollama
   ;; providers
   #:provider-ollama
   ;; tools
   #:tool-fs #:tool-shell #:tool-http #:tool-eval #:tool-gated-eval #:tool-repl #:tool-plan
   #:tool-image #:tool-services #:tool-checkpoint #:tool-self #:tool-vault #:tool-calls))
