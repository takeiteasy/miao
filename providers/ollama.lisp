(in-package #:miao)

;;; Ollama's native chat endpoint. No key, so it is the provider a development
;;; machine can run end to end -- and this route carries the usage counters
;;; (eval_count, total_duration) and options (num_ctx, format, keep_alive) the
;;; OpenAI-compatible /v1 route drops, in :protocol-ollama's :meta.
;;;
;;; The /v1 route stays reachable with no provider of its own:
;;; :protocol-openai takes :base-url per request, so one mounted service
;;; already answers for it.

(define-provider :ollama
  :protocol :protocol-ollama
  :base-url "http://127.0.0.1:11434"
  :auth :none
  ;; Advertisement, not a gate: the real catalogue is whatever has been
  ;; pulled, which only the running backend knows.
  :models '("llama3.2" "qwen2.5-coder" "gemma3")
  :summary "Local Ollama, native chat endpoint")
