(in-package #:miao)

;;; The agent loop. A meow agent (M:AGENT) that sends a conversation to a
;;; bound model, dispatches the tool calls that come back, feeds the results
;;; in and goes round again. Driven by messages rather than a blocking call,
;;; so it stays responsive between turns -- CANCEL and STEER land during a
;;; run, not just before one -- and a sub-agent delegated under meow's own
;;; agent supervisor reports back into the same machine. See docs/agent.md.
;;;
;;; Every outbound piece of work -- a model turn, a tool call, a sub-agent --
;;; is issued off the agent's process and reported back as a message, so
;;; HANDLE is never blocked waiting on one. A turn and a tool call are sent
;;; with M:CALL-ASYNC and answer as a (:REPLY tag value status) message,
;;; holding no thread meanwhile; a sub-agent is a delegated agent of its own.
;;;
;;; Finishing a run returns (values :done result) from HANDLE for a delegated
;;; agent, which is M:AGENT's own convention: the parent gets :agent-done and
;;; the agent exits, so the run is its whole life. A mounted agent has no parent
;;; and stays up, holding its conversation for the next :run.

(defconstant +sub-agent-tool-name+ :agent-task
  "The reserved tool name a model calls to delegate a task, when :SUB-AGENTS
is on. Never a registered tool, so it is dispatched before the allow-list is
consulted.")

(defclass agent (m:agent)
  ((model :initarg :model :initform nil :reader agent-model
          :documentation "A protocol or provider service name.")
   (tools-spec :initarg :tools :initform :default :reader agent-tools-spec
               :documentation ":DEFAULT is the discovered :TRUST :AGENT
tools -- tool-fs, tool-plan, tool-image and tool-services today.
Otherwise a list of tool names.")
   (system :initarg :system :initform nil :reader agent-system)
   (max-turns :initarg :max-turns :initform 16 :reader agent-max-turns)
   (turn-timeout :initarg :turn-timeout :initform +default-tool-timeout+
                 :reader agent-turn-timeout)
   (deadline :initarg :deadline :initform 300000 :reader agent-deadline
             :documentation "Milliseconds for the whole run.")
   (sub-agents :initarg :sub-agents :initform nil :reader agent-sub-agents)
   (sink :initarg :sink :initform nil :reader agent-sink)
   (max-parallel-tools :initarg :max-parallel-tools :initform nil
                       :type (or null (integer 1)) :reader agent-max-parallel-tools
                       :documentation "The most tool calls, sub-agents included,
running at once. The rest wait their turn.")
   (tool-grace :initarg :tool-grace :initform nil
               :type (or null (real (0))) :reader agent-tool-grace
               :documentation "Milliseconds a tool call may run before the turn
goes on without it: the call is detached, answered with a stub for now, and its
result folded in as a :USER message when it lands. A tool whose metadata says
:BACKGROUND detaches at once. NIL waits for every call.")
   (max-detached :initarg :max-detached :initform nil
                 :type (or null (integer 1)) :reader agent-max-detached
                 :documentation "The most calls detached at once. A call that
would detach past it stays attached, and detaches when a slot frees. NIL is
uncapped.")
   (max-tool-result :initarg :max-tool-result :initform nil
                    :type (or null (integer 1)) :reader agent-max-tool-result
                    :documentation "The most characters of a tool result's
rendered text that reach the conversation; NIL is uncapped.")
   (max-context :initarg :max-context :initform nil
                :type (or null (integer 1)) :reader agent-max-context
                :documentation "The most tokens of conversation and tool
schemas a turn's request carries, estimated from characters at
:CHARS-PER-TOKEN; the oldest turns past it are left out of the request, not the
conversation. NIL is unbounded.")
   (chars-per-token :initarg :chars-per-token :initform 3
                    :type (real (0)) :accessor %chars-per-token
                    :documentation "Characters per token, the estimate
:MAX-CONTEXT is measured with. Starts here and is recalibrated from each
reply's prompt-token count, so it follows the model's own tokenizer.")
   (turn-retries :initarg :turn-retries :initform 0
                 :type (integer 0) :reader agent-turn-retries
                 :documentation "How many times a turn that failed transiently
is sent again before the run ends.")
   (retry-backoff :initarg :retry-backoff :initform 1000
                  :type (real 0) :reader agent-retry-backoff
                  :documentation "Milliseconds before the first retry; each
further one waits twice as long, plus jitter.")
   (hooks :initarg :hooks :initform nil :reader agent-hooks-spec
          :documentation "The interceptor hooks a run goes through, in order: a
hook service name, a function of (phase request), or a list of either followed
by :ON-ERROR, :PHASES, :TIMEOUT and, for a function, :NAME overrides. A
sub-agent inherits its parent's. NIL is none. See docs/hooks.md.")
   (log-raw :initarg :log-raw :initform nil :reader agent-log-raw
            :documentation "True: the call log also keeps the arguments and
result a hook rewrote, as the model and the tool first had them.")
   (sampling :initarg :sampling :initform nil :reader agent-sampling
             :documentation "A plist of sampling parameters passed through
to COMPLETE, e.g. :TEMPERATURE.")
   (vault :initarg :vault :initform nil :reader agent-vault
          :documentation "NIL (the default): steering is in-memory only. T:
record to the default vault log (~takeiteasy/miao#14). A string or
pathname: record there instead.")
   (journal :initarg :journal :initform nil :reader agent-journal
            :documentation "NIL (the default): runs are not journaled. T:
journal to the default log (~takeiteasy/miao#119). A string or pathname: journal
there instead. A sub-agent inherits it.")
   ;; Run state, reset by START-RUN.
   ;; Newest first, so adding one is O(1); CONVERSATION reads it in order.
   (messages :initform nil :accessor %messages)
   (turns :initform 0 :accessor %turns)
   (allow-list :initform nil :accessor %allow-list)
   (pending :initform nil :accessor %pending)
   (pending-order :initform nil :accessor %pending-order)
   (queued :initform nil :accessor %queued)
   ;; ((ref . call-id) :name n :token cancel-token :log-id id), newest first.
   (detached :initform nil :accessor %detached)
   (awaiting-detached :initform nil :accessor %awaiting-detached)
   ;; (ref id name) of calls due to detach once a slot frees, oldest first.
   (detach-deferred :initform nil :accessor %detach-deferred)
   (call-tokens :initform nil :accessor %call-tokens)
   (journal-call-ids :initform nil :accessor %journal-call-ids)
   (input-log-id :initform nil :accessor %input-log-id)
   (steer-queue :initform nil :accessor %steer-queue)
   (step-ref :initform 0 :accessor %step-ref)
   ;; Bumped by BEGIN-RUN. A :STEP or :DEADLINE cast by an earlier run may
   ;; reach the agent after it, and is dropped for carrying an old id.
   (run-id :initform 0 :accessor %run-id)
   (running-p :initform nil :accessor %running-p)
   (cancel-timer :initform nil :accessor %cancel-timer)
   (turn-token :initform nil :accessor %turn-token)
   (turn-in-flight :initform nil :accessor %turn-in-flight)
   (attempt :initform 0 :accessor %attempt)
   (retry-pending :initform nil :accessor %retry-pending)
   (turn-stream :initform nil :accessor %turn-stream)
   (last-request-chars :initform nil :accessor %last-request-chars)
   (fanout :initform nil :accessor %fanout)
   (hooks-resolved :initform nil :accessor %hooks)
   ;; (spec . process) for each function hook this agent mounted a service for.
   (hook-services :initform nil :accessor %hook-services)
   ;; The root run's handle, which a sub-agent is given: what a hook's :RUN and
   ;; :EMIT reach.
   (run-handle :initarg :run-handle :initform nil :accessor %run-handle)
   (chains :initform nil :accessor %chains)
   (chain-seq :initform 0 :accessor %chain-seq)
   ;; The calls of the turn under way, so a result can be matched to its tool.
   (calls :initform nil :accessor %calls)
   ;; (sink . emitter) for each function sink this agent started an emitter for.
   (emitters :initform nil :accessor %emitters)
   (local-subscribers :initform nil :accessor %local-subscribers)
   ;; Unbound for a root agent, so a child's events are told apart by the key
   ;; being present even when its parent is unnamed.
   (parent-name :initarg :parent-name :reader agent-parent-name)
   (task :initarg :task :initform nil :reader agent-task
         :documentation "A delegated child's task, for a listing of a run's children.")
   (child-seq :initform 0 :accessor %child-seq))
  (:default-initargs :name nil))

(defmethod m:metadata ((service agent))
  (list* :kind :agent
         :name (m:service-name service)
         :summary "A turn cycle over a bound model and its tools"
         (append
          (and (slot-boundp service 'parent-name)
               (list :parent (agent-parent-name service) :task (agent-task service)))
          (agent-settings service))))

(defun agent-settings (service)
  (list
        :model (agent-model service)
        :tools (agent-tools-spec service)
        :sub-agents (agent-sub-agents service)
        :max-turns (agent-max-turns service)
        :tool-grace (agent-tool-grace service)
        :max-detached (agent-max-detached service)
        :max-tool-result (agent-max-tool-result service)
        :max-context (agent-max-context service)
        :chars-per-token (%chars-per-token service)
        :turn-retries (agent-turn-retries service)
        :retry-backoff (agent-retry-backoff service)
        :vault (agent-vault service)
        :journal (agent-journal service)
        :hooks (mapcar #'hook-spec-label (agent-hooks-spec service))
        :log-raw (agent-log-raw service)))

(defun agents (&key (registry m:*registry*))
  "Every registered agent name, sorted, a delegated child's included."
  (%registered-of-kind :agent :registry registry))

(defun sub-agents (parent &key (registry m:*registry*))
  "The names of the live delegated children of the agent named PARENT, sorted."
  (sort (loop for name in (agents :registry registry)
              when (and parent (eq parent (getf (nth-value 1 (m:lookup name :registry registry)) :parent)))
                collect name)
        #'string< :key #'symbol-name))

(defun %agent-process (name &key (registry m:*registry*))
  (multiple-value-bind (process props) (m:lookup name :registry registry)
    (unless process (error "No agent registered under ~s." name))
    (values process props)))

(defun describe-agent (name &key (registry m:*registry*))
  "NAME's metadata plist."
  (m:call (%agent-process name :registry registry) '(:describe)))

;;; --- the machine --------------------------------------------------------

(defmethod m:handle ((service agent) message)
  (case (first message)
    (:describe (m:metadata service))
    (:run (start-run service (rest message)))
    (:steer (queue-steer service (rest message)))
    (:resume (resume-request service (rest message)))
    (:cancel (cancel-run service))
    (:subscribe (subscribe service (second message)))
    (:unsubscribe (unsubscribe service (second message)))
    (:step (when (and (%running-p service) (eql (second message) (%run-id service)))
             (step-agent service)))
    (:deadline (when (eql (second message) (%run-id service))
                 (deadline-run service)))
    (:detach (destructuring-bind (ref id name) (rest message)
               (detach-call service ref id name)
               nil))
    ;; Not routed through CALL-RESULT: a detached call has no timeout to hit,
    ;; and its cell holds a stub that TOOL-REPLY leaves alone.
    (:call-timeout (destructuring-bind (ref id) (rest message)
                     (when (and (eql ref (%step-ref service)) (call-pending-p service id))
                       (tool-reply service id (fail :timeout)))
                     nil))
    (:retry (when (and (%retry-pending service) (eql (second message) (%step-ref service)))
              (resend-turn service)
              nil))
    (:reply (destructuring-bind (tag value status) (rest message)
              (route-reply service tag (%call-result value status))))
    ;; Reports from a delegated sub-agent, routed into HANDLE by the meow fix
    ;; for ~takeiteasy/meow#59.
    (:agent-done (sub-agent-done service (second message) (fourth message)))
    (:agent-down (sub-agent-down service (second message) (third message)))
    (:snapshot (snapshot service))
    (:restore (restore service (second message)))
    (t (bad-request "unknown message ~s" (first message)))))

(defun start-run (service args)
  (multiple-value-bind (entries problem) (resolve-hooks service)
    (or problem
        (progn
          (unless (%running-p service)
            (setf (%hooks service) entries))
          (resume-problem service args))
        (let ((keyed (and (getf args :input-id)
                          (accept-input service args :record (not (%running-p service))))))
          (cond ((tool-error-p keyed) keyed)
                ((consp keyed) keyed)
                ((%running-p service) (bad-request "agent is already running"))
                (t (begin-run service args keyed)))))))

(defun messages-digest (messages)
  (format nil "~(~{~2,'0x~}~)"
          (coerce (sb-md5:md5sum-string (json:stringify (untyped->json messages))) 'list)))

(defun accept-input (service args &key (record t))
  "Log a :RUN keyed :INPUT-ID in the call log, unless RECORD is false. Answers
the log id of a new one, nil when not recorded, or the reply for a
redelivery, (:OK (:DUPLICATE status)); a bad-request when there is no call
log or the id was used for other messages."
  (let ((path (journal-of service))
        (input-id (getf args :input-id)))
    (cond ((not (stringp input-id)) (bad-request ":input-id must be a string"))
          ((not path) (bad-request ":input-id needs a :journal"))
          (t (let ((digest (messages-digest (getf args :messages))))
               (multiple-value-bind (id duplicate status prior)
                   (journal-input path (m:service-name service) input-id digest
                                   :record record)
                 (cond ((not duplicate) id)
                       ((not (equal prior digest))
                        (bad-request "input-id ~a was used for other content" input-id))
                       (t (ok :duplicate status)))))))))

(defun record-input-done (service outcome)
  (a:when-let ((path (journal-of service)))
    (a:when-let ((id (%input-log-id service)))
      (journal-call-done path (list (list id outcome nil)))
      (setf (%input-log-id service) nil))))

(defun begin-run (service args input-log-id)
  (let ((continuing (and (getf args :continue) (%messages service) t)))
    (drop-chains service :detached t)
    (setf (%input-log-id service) input-log-id
          (%messages service)
          (revappend (getf args :messages)
                     (if continuing
                         (%messages service)
                         (and (agent-system service)
                              (not (eq :system (getf (first (getf args :messages)) :role)))
                              (list (list :role :system :content (agent-system service))))))
          (%turns service) 0
          (%run-id service) (1+ (%run-id service))
          (%pending service) nil
          (%pending-order service) nil
          (%queued service) nil
          (%detached service) nil
          (%awaiting-detached service) nil
          (%detach-deferred service) nil
          (%call-tokens service) nil
          (%journal-call-ids service) nil
          ;; %STEER-QUEUE is deliberately not cleared here: a steer (or
          ;; a vault :restore) sent while the agent was idle waits in
          ;; the queue rather than being dropped, and folds in on the
          ;; first turn below, after the seed messages.
          (%allow-list service) (resolve-tools service)
          (%running-p service) t)
    (unless (%fanout service)
      (open-fanout service))
    (unless (slot-boundp service 'parent-name)
      (setf (%run-handle service) (make-run-handle (%fanout service))))
    (let* ((ids (getf args :resume))
           (answer nil)
           (resumed nil))
      (when ids
        (multiple-value-setq (answer resumed) (resume-calls service ids (getf args :force))))
      (cond ((and ids (not (getf args :messages)) (null resumed))
             (setf (%running-p service) nil)
             (record-input-done service :error)
             (retire-emitters service)
             answer)
            (t (journal-run-start service args continuing)
               (emit service (run-start-event (m:agent-ref service) (getf args :messages)
                                              (and (getf args :continue) t)))
               ;; The step is cast before the resumed calls are sent, so a result
               ;; that comes back at once folds into a later turn, not the first.
               (if (or (getf args :messages) (null ids))
                   (cast-step service)
                   (setf (%awaiting-detached service) t))
               (dispatch-resumed-calls service resumed)
               (arm-deadline service)
               (or answer :ok))))))

(defun resolve-tools (service)
  "The allow-list for this run: an explicit list, or every discovered tool
whose :TRUST is :AGENT."
  (let ((spec (agent-tools-spec service))
        (registry (m:service-registry service)))
    (if (eq spec :default)
        (remove-if-not (lambda (name)
                          (eq (tool-trust (tool-metadata name :registry registry))
                              :agent))
                        (tools :registry registry))
        spec)))

(defun queue-steer (service args)
  "Queue a :USER message from ARGS' :CONTENT. When the vault is on
(AGENT-VAULT) and ARGS names no :VAULT-ID, this is a fresh steer and gets
recorded there first and claimed; a :VAULT-ID names an entry already in the
vault, claimed by TOOL-VAULT's :RESTORE, which redelivers one this way
rather than double-recording it, with :VAULT-PATH the log it lives in. A
caller's :INPUT-ID, which needs the vault, makes a redelivery answer (:OK
(:DUPLICATE status)) instead of queueing a second time. The
id and path travel in the queue cell, never in the message plist pushed onto
%MESSAGES, so they can never reach a provider's request. :INTERRUPT true
also abandons a model turn in flight (INTERRUPT-TURN) or the tool calls
outstanding (INTERRUPT-TOOLS); otherwise the steer waits for the next turn as
usual."
  (let* ((content (getf args :content))
         (input-id (getf args :input-id))
         (path (or (getf args :vault-path) (%vault-path (agent-vault service)))))
    (cond ((and input-id (not (stringp input-id)))
           (bad-request ":input-id must be a string"))
          ((and input-id (not path))
           (bad-request ":input-id needs a :vault"))
          (t (multiple-value-bind (id duplicate status prior)
                 (or (getf args :vault-id)
                     (and path
                          (vault-record path (m:service-name service) content
                                        :claim t :input-id input-id)))
               (cond ((not duplicate)
                      (push (list id path (list :role :user :content content)
                                  (list :steer t :interrupt (and (getf args :interrupt) t)
                                        :input-id input-id))
                            (%steer-queue service))
                      (wake-if-waiting service)
                      (steer-interrupted service args))
                     ((not (equal prior content))
                      (bad-request "input-id ~a was used for other content" input-id))
                     (t (ok :duplicate status))))))))

(defun steer-interrupted (service args)
  (if (getf args :interrupt)
      ;; An interrupt on the last allowed turn finishes the run, and
      ;; (VALUES :DONE result) is how HANDLE ends it.
      (multiple-value-bind (value result)
          (cond ((%retry-pending service) (resend-turn service))
                ((%turn-in-flight service) (interrupt-turn service))
                ((%pending service) (interrupt-tools service)))
        (if (eq value :done) (values :done result) :ok))
      :ok))

(defun interrupt-turn (service)
  "Abandon the model turn in flight for the steer just queued: its late reply
drops with the step ref, the text it had streamed is kept as an assistant
message, and the next turn folds the steer in. Re-stepping through
STEP-AGENT keeps :MAX-TURNS in force, so the abandoned turn counts."
  (setf (%turn-in-flight service) nil)
  (incf (%step-ref service))
  ;; Superseded before it is cancelled, so the cancelled :DONE the protocol
  ;; then emits meets a closed stream rather than racing :TURN-INTERRUPTED.
  (let ((partial (close-turn-stream
                  service (turn-interrupted-event (m:agent-ref service) (%turns service)))))
    (cancel-turn service)
    (drop-chains service)
    (when (plusp (length partial))
      (push-message service (list :role :assistant :content partial))))
  (step-agent service))

(defun interrupt-tools (service)
  "Close the tool calls outstanding for the steer just queued: each still
running is cancelled and closed as :INTERRUPTED, the results already in are
kept, and the next turn folds the steer in, as INTERRUPT-TURN does."
  (close-pending-calls service)
  (drop-chains service)
  (incf (%step-ref service))
  (step-agent service))

(defun release-steer-claims (service)
  "Release the vault claim of every steer queued at SERVICE."
  (let ((by-path (make-hash-table :test 'equal)))
    (dolist (cell (%steer-queue service))
      (when (car cell) (push (car cell) (gethash (cadr cell) by-path))))
    (maphash #'vault-release-all by-path)))

(defun reclaim-steer-claims (service)
  "Claim, as this image, every steer queued at SERVICE, dropping any that
another process holds or that is already consumed."
  (setf (%steer-queue service)
        (remove-if-not (lambda (cell)
                         (or (null (car cell))
                             (eq :claimed (vault-claim-pending (cadr cell) (car cell)))))
                       (%steer-queue service))))

(defmethod m:dispose ((service agent) reason)
  (release-steer-claims service)
  (drop-chains service :detached t)
  (record-outstanding service :abandoned)
  (close-detached service :abandoned)
  (record-input-done service :abandoned)
  (a:when-let ((path (journal-of service)))
    (journal-drain path))
  (retire-emitters service)
  (stop-hook-services service)
  (when (or (eq reason :shutdown) (not (m:will-restart-p service reason)))
    (setf (subscribers service) nil)))

(defun cancel-run (service)
  (if (%running-p service)
      (progn
        (close-pending-calls service)
        (finish-run service (ok :messages (conversation service) :content nil
                                :turns (%turns service) :stop-reason :cancelled)))
      :ok))

(defun deadline-run (service)
  (when (%running-p service)
    (close-pending-calls service)
    (finish-run service (ok :messages (conversation service) :content nil
                            :turns (%turns service) :stop-reason :timeout))))

(defun step-agent (service)
  (if (>= (%turns service) (agent-max-turns service))
      (finish-run service (ok :messages (conversation service) :content nil
                              :turns (%turns service) :stop-reason :max-turns))
      (issue-turn service)))

(defun fold-steers (service)
  "Steering only folds in between turns, so a message queued mid-turn never
lands ahead of the assistant reply or tool results already owed."
  (dolist (cell (nreverse (shiftf (%steer-queue service) nil)))
    (destructuring-bind (id path message meta) cell
      (push-message service message)
      (when (getf meta :steer)
        (emit service (steer-event (m:agent-ref service) (getf message :content)
                                   (getf meta :interrupt) (getf meta :input-id))))
      (when id
        (vault-consume path id :folded)))))

(defun issue-turn (service)
  (fold-steers service)
  (incf (%turns service))
  (setf (%attempt service) 0)
  (emit service (turn-event (m:agent-ref service) (%turns service)))
  (send-turn service))

(defun send-turn (service)
  (setf (%turn-in-flight service) t
        (%turn-stream service) (and (%fanout service)
                                   (fanout-listening-p (%fanout service))
                                   (make-turn-stream)))
  (let ((ref (incf (%step-ref service))))
    (run-chain service :before-turn (conversation service) nil
               (lambda (status value)
                 (ecase status
                   (:ok (send-turn-request service ref value))
                   (:failed (setf (%turn-in-flight service) nil)
                    (finish-run service (hook-failure value))))))))

(defun send-turn-request (service ref messages)
  "Send the turn for MESSAGES, the conversation as the before-turn hooks left it."
  (let ((request (build-request service
                                (setf (%turn-token service) (make-cancel-token))
                                (%turn-stream service)
                                messages)))
    (send-call service (list :turn ref) #'%completion-call (agent-model service) request)
    nil))

(defun send-call (service tag prepare name args &key unbounded)
  "Send the call PREPARE builds for NAME and ARGS -- %COMPLETION-CALL or
%TOOL-CALL -- without waiting. Its reply reaches HANDLE as (:REPLY TAG value
status). A call that cannot be sent is answered at once.

UNBOUNDED sends it with no meow timeout and answers the seconds it would have
had, for the caller to time only while the call is attached: a detached call
outlasts it. TODO: a tool that ignores cancel and never answers leaves its
meow pending call alive for good, as call-async cannot withdraw it (#188)."
  (multiple-value-bind (process message timeout)
      (handler-case (funcall prepare name args :registry (m:service-registry service))
        (error (e) (values nil (fail (list :error (princ-to-string e))))))
    (cond ((null process)
           (m:cast (m:self) (list :reply tag message nil))
           nil)
          (unbounded
           (m:call-async process message :timeout nil :tag tag)
           timeout)
          (t (m:call-async process message :timeout timeout :tag tag)
             nil))))

(defun route-reply (service tag result)
  "Hand RESULT to the turn or tool call TAG names."
  (destructuring-bind (kind ref &optional id) tag
    (ecase kind
      (:turn (turn-reply service ref result))
      (:tool (call-result service ref id result))
      (:hook (hook-reply service ref id result)))))

(defun call-result (service ref id result)
  "Hand RESULT to call ID dispatched under step REF: a detached call's late
result is folded in, and any other counts only under the step it was
dispatched in."
  (let ((key (cons ref id))
        (entry nil))
    (cond ((setf entry (assoc key (%detached service) :test #'equal))
           (intercept-result service id (getf (cdr entry) :name) result
                             (lambda (hooked) (detached-reply service key hooked result))
                             :detached t))
          ((and (eql ref (%step-ref service)) (call-pending-p service id)
                (phase-hooks service :after-tool-result))
           (setf (cdr (assoc id (%pending service) :test #'equal)) :intercepting)
           (intercept-result service id (call-name service id) result
                             (lambda (hooked) (tool-reply service id hooked result))))
          ((eql ref (%step-ref service))
           (tool-reply service id result))))
  nil)

(defun call-name (service id)
  (getf (find id (%calls service) :key (lambda (call) (getf call :id)) :test #'equal) :name))

(defun intercept-result (service id name result settle &key detached)
  "Pass RESULT through the after-tool-result hooks and give SETTLE what they
leave, or the failure a closed hook ends in. SETTLE runs at once with no hooks."
  (run-chain service :after-tool-result result (list :id id :name name)
             (lambda (status value)
               (funcall settle (if (eq status :ok)
                                   value
                                   (hook-refusal status (first value) (second value)))))
             :detached detached))

(defun build-request (service token stream &optional (conversation (conversation service)))
  (let ((tools (request-tools service)))
    (multiple-value-bind (messages record chars)
        (fit-conversation conversation
                          :max-context (agent-max-context service)
                          :max-tool-result (agent-max-tool-result service)
                          :chars-per-token (%chars-per-token service)
                          :margin +context-margin+
                          :reserved (%printed-size tools))
      (setf (%last-request-chars service) chars)
      (when record
        (emit service
                    (context-trimmed-event (m:agent-ref service) (%turns service) record)))
      (make-request service token stream messages tools))))

(defun make-request (service token stream messages tools)
  (list* :cancel token
         :messages messages
         :tools tools
         :stream (and stream (turn-stream-sink stream service))
         :ref (m:agent-ref service)
         :timeout (agent-turn-timeout service)
         (agent-sampling service)))

(defun request-tools (service)
  (append (mapcar (lambda (name)
                     (tool-metadata name :registry (m:service-registry service)))
                   (%allow-list service))
          (when (agent-sub-agents service) (list (sub-agent-tool-metadata)))))

(defun sub-agent-tool-metadata ()
  (list :kind :tool :name +sub-agent-tool-name+
        :summary "Delegate a task to a sub-agent with this agent's model and
tools, and get back its final answer."
        :params '((:task string :required t :doc "the task to hand off"))))

(defun turn-reply (service ref result)
  ;; A late reply from a turn CANCEL or :DEADLINE already superseded: the
  ;; step ref has moved on, so this one is dropped.
  (when (eql ref (%step-ref service))
    (setf (%turn-in-flight service) nil)
    (cond
      ((and (tool-error-p result) (retry-turn-p service result))
       (schedule-retry service result))
      ((tool-error-p result)
       (finish-run service result))
      (t
       (let ((reply (second result)))
         (calibrate service reply)
         (push-message service reply)
         (emit service (reply-event ref (%turns service) reply))
         (let ((calls (getf reply :tool-calls)))
           (cond
             (calls (dispatch-calls service calls))
             ;; A steer that arrived while this turn was in flight gets a
             ;; turn of its own rather than waiting for the next :RUN.
             ((and (%steer-queue service)
                   (< (%turns service) (agent-max-turns service)))
              (issue-turn service))
             ;; A detached call's result is still owed: the run stays open,
             ;; and DETACHED-REPLY wakes it with a turn for the result.
             ((and (%detached service)
                   (< (%turns service) (agent-max-turns service)))
              (setf (%awaiting-detached service) t)
              nil)
             (t (finish-run service (ok :messages (conversation service)
                                        :content (getf reply :content)
                                        :turns (%turns service)
                                        :stop-reason :stop))))))))))

(defun retryable-p (result)
  "Whether RESULT, an (:error reason), is a failure a fresh attempt could get
past: a backend that could not be reached, or one that answered 408, 425, 429,
a 5xx, or a 2xx whose stream or payload broke. A :TIMEOUT is not, since another
attempt could double a wait the caller bounded."
  (let ((reason (tool-error result)))
    (or (eq reason :unavailable)
        (and (consp reason)
             (eq (first reason) :backend-error)
             (let ((status (second reason)))
               (and (integerp status)
                    (or (member status '(408 425 429))
                        (>= status 500)
                        (<= 200 status 299))))))))

(defun retry-turn-p (service result)
  (and (retryable-p result)
       (< (%attempt service) (agent-turn-retries service))))

(defun retry-after (reason)
  "The milliseconds a failed turn's REASON says to wait, or nil."
  (and (consp reason) (eq (first reason) :backend-error)
       (getf (cdddr reason) :retry-after)))

(defun retry-delay (backoff attempt retry-after)
  "Milliseconds before retry ATTEMPT: the doubled BACKOFF, or RETRY-AFTER when
the backend asked for longer, plus up to 25% jitter."
  (* (max (* backoff (expt 2 (1- attempt))) (or retry-after 0))
     (+ 1 (random 0.25d0))))

(defun schedule-retry (service result)
  "Send the turn that just failed again after a backoff. The retry is not a
new turn: :TURNS and the :TURN event stay as they were. Anything that moves
the step ref before the timer fires -- a cancel, the deadline, a restore, an
interrupt -- leaves the timer's :RETRY unmatchable."
  (let* ((attempt (incf (%attempt service)))
         (delay (retry-delay (agent-retry-backoff service) attempt
                             (retry-after (tool-error result))))
         (ref (%step-ref service))
         (self (m:self)))
    (close-turn-stream service)
    (emit service
                (turn-retry-event (m:agent-ref service) (%turns service) attempt
                                  (tool-error result)))
    (setf (%turn-in-flight service) t
          (%retry-pending service)
          (m:after service (/ delay 1000.0d0)
                   (lambda () (m:cast self (list :retry ref)))))
    nil))

(defun cancel-retry (service)
  (a:when-let ((cancel (shiftf (%retry-pending service) nil)))
    (funcall cancel)))

(defun resend-turn (service)
  "Send the turn again, with any steer that queued during the backoff."
  (cancel-retry service)
  (fold-steers service)
  (send-turn service))

(defun dispatch-calls (service calls)
  "Every call starts :QUEUED and is dispatched, in order, as
:MAX-PARALLEL-TOOLS allows."
  (setf (%pending service) (mapcar (lambda (call) (cons (getf call :id) :queued)) calls)
        (%pending-order service) (mapcar (lambda (call) (getf call :id)) calls)
        (%call-tokens service) (mapcar (lambda (call) (cons (getf call :id) (make-cancel-token)))
                                       calls)
        (%queued service) calls
        (%calls service) calls)
  ;; With before-tool-call hooks a call is announced once they have answered,
  ;; so neither the log nor a sink ever sees the arguments they redact.
  (unless (phase-hooks service :before-tool-call)
    (dolist (call calls)
      (accept-call service call)))
  (pump-calls service)
  nil)

(defun accept-call (service call &optional (raw-arguments (getf call :arguments)))
  "Log CALL as accepted and announce it. RAW-ARGUMENTS, the arguments the model
sent where a hook rewrote them, go to the log under :LOG-RAW."
  (a:when-let ((path (journal-of service)))
    (push (cons (getf call :id)
                (first (journal-call-accept
                        path (m:service-name service) (%turns service)
                        (list (if (and (agent-log-raw service)
                                       (not (equal raw-arguments (getf call :arguments))))
                                  (list* :raw-arguments raw-arguments call)
                                  call))
                        :cap (or (agent-max-tool-result service) *journal-max-content*))))
          (%journal-call-ids service)))
  (emit service (tool-call-event (m:agent-ref service) (getf call :id)
                                 (getf call :name) (getf call :arguments))))

(defun running-calls (service)
  (count-if (lambda (status) (member status '(:pending :intercepting))) (%pending service)
            :key #'cdr))

(defun pump-calls (service)
  "Dispatch queued calls while a slot is free. A call refused outright
answers at once and never holds one."
  (let ((started '()))
    (loop with cap = (agent-max-parallel-tools service)
          while (and (%queued service)
                     (or (null cap) (< (running-calls service) cap)))
          do (let* ((call (pop (%queued service)))
                    (cell (assoc (getf call :id) (%pending service) :test #'equal)))
               (setf (cdr cell) :pending)
               (push call started)))
    (record-running service (mapcar (lambda (call) (getf call :id)) started))
    (dolist (call (nreverse started))
      (dispatch-call service call))))

(defun dispatch-call (service call)
  "A call outside the allow-list, and a tool error of any kind, both come
back as a :TOOL message rather than ending the run: the model gets a chance
to recover. A call the allow-list passes, the reserved sub-agent call included,
goes through the before-tool-call hooks first."
  (let ((name (getf call :name)))
    (cond
      ((not (or (and (agent-sub-agents service) (eq name +sub-agent-tool-name+))
                (member name (%allow-list service))))
       (tool-reply service (getf call :id)
                   (bad-request "~(~a~) is not in this agent's tool allow-list"
                                name)))
      ((phase-hooks service :before-tool-call)
       (run-chain service :before-tool-call (getf call :arguments)
                  (list :id (getf call :id) :name name)
                  (lambda (status value) (call-chained service call status value))))
      (t (run-call service call)))))

(defun run-call (service call)
  (if (and (agent-sub-agents service) (eq (getf call :name) +sub-agent-tool-name+))
      (dispatch-sub-agent service call)
      (dispatch-tool service call)))

(defun call-chained (service call status value)
  "CALL once the before-tool-call hooks have answered: run with the arguments
they left, or answered as the denial or failure they ended in."
  (let ((id (getf call :id)))
    (ecase status
      (:ok (let ((hooked (list* :arguments value (a:remove-from-plist call :arguments))))
             (accept-call service hooked (getf call :arguments))
             (record-running service (list id))
             (run-call service hooked)))
      ((:denied :failed)
       (destructuring-bind (hook reason arguments) value
         (accept-call service (list* :arguments arguments (a:remove-from-plist call :arguments))
                      (getf call :arguments))
         (tool-reply service id (hook-refusal status hook reason)))))))

(defun call-pending-p (service id)
  (eq :pending (cdr (assoc id (%pending service) :test #'equal))))

(defun call-token (service id)
  (cdr (assoc id (%call-tokens service) :test #'equal)))

(defun dispatch-tool (service call)
  "The reply carries the step ref it was dispatched under: a provider may
reuse a call id on the next turn, and a late reply from an interrupted call
must not answer it."
  (let* ((id (getf call :id))
         (name (getf call :name))
         (args (getf call :arguments))
         (token (call-token service id))
         (ref (%step-ref service)))
    (a:when-let ((timeout (send-call service (list :tool ref id) #'%tool-call name
                                     (list* :cancel token args) :unbounded t)))
      (arm-call-timeout service ref id timeout))
    (if (getf (tool-metadata name :registry (m:service-registry service)) :background)
        (m:cast (m:self) (list :detach ref id name))
        (arm-detach service call))
    nil))

(defun arm-call-timeout (service ref id seconds)
  "Answer call ID (:ERROR :TIMEOUT) after SECONDS unless it has settled or
detached by then."
  (let ((self (m:self)))
    (m:after service seconds (lambda () (m:cast self (list :call-timeout ref id))))))

(defun arm-detach (service call)
  "Detach CALL if it is still running after :TOOL-GRACE."
  (a:when-let ((grace (agent-tool-grace service)))
    (let ((message (list :detach (%step-ref service) (getf call :id) (getf call :name)))
          (self (m:self)))
      (m:after service (/ grace 1000.0d0) (lambda () (m:cast self message))))))

(defun dispatch-sub-agent (service call)
  "A tool cannot delegate on the loop's behalf -- the parent would be the
tool's own process, not this agent -- so the reserved call is dispatched
here, directly on this agent's own process, which is what M:DELEGATE reads
its parent from. The child inherits this agent's model and allow-list but
not :SUB-AGENTS, so delegation does not nest by default; which models and
tool sets a child may be given is ~takeiteasy/miao#22's policy, not this
ticket's. The child's ref pairs the step ref with the call id, for the same
reason DISPATCH-TOOL's reply does, and cancelling the call cancels the child.
It is registered as <parent>/<n>, so a steer or cancel reaches it by name; a
child of an unnamed parent is not."
  (let* ((id (getf call :id))
         (task (getf (getf call :arguments) :task))
         (context (m:service-process (m:service-context service)))
         (child (m:delegate context 'agent :ref (cons (%step-ref service) id)
                            :name (%child-name service)
                            :task task
                            :model (agent-model service)
                            :tools (%allow-list service)
                            :parent-name (m:service-name service)
                            :sub-agents nil
                            :max-turns (agent-max-turns service)
                            :max-parallel-tools (agent-max-parallel-tools service)
                            :tool-grace (agent-tool-grace service)
                            :max-detached (agent-max-detached service)
                            :max-tool-result (agent-max-tool-result service)
                            :max-context (agent-max-context service)
                            :chars-per-token (%chars-per-token service)
                            :turn-retries (agent-turn-retries service)
                            :retry-backoff (agent-retry-backoff service)
                            :turn-timeout (agent-turn-timeout service)
                            :deadline (agent-deadline service)
                            :sink (%fanout service)
                            :vault (agent-vault service)
                            :journal (agent-journal service)
                                                :hooks (%hooks service)
                            :run-handle (%run-handle service)
                            :log-raw (agent-log-raw service))))
    (on-cancel (call-token service id) (lambda () (m:cast child '(:cancel))))
    (m:cast child (list :run :messages (list (list :role :user :content task))))
    (arm-detach service call)
    nil))

(defun %child-name (service)
  "The next free <parent>/<n> under SERVICE's name, or nil when it has none. A
restarted parent counts from 1 again, so a name a live child still holds is
skipped."
  (a:when-let ((parent (m:service-name service)))
    (loop for name = (a:make-keyword (format nil "~a/~d" parent (incf (%child-seq service))))
          unless (m:lookup name :registry (m:service-registry service))
            return name)))

(defun sub-agent-done (service ref result)
  (call-result service (car ref) (cdr ref)
               (cond ((tool-error-p result) result)
                     ((eq :cancelled (getf (second result) :stop-reason)) (fail :cancelled))
                     (t (ok :answer (content-text (getf (second result) :content)))))))

(defun sub-agent-down (service ref reason)
  (call-result service (car ref) (cdr ref) (fail (list :sub-agent-down reason))))

(defun outstanding-p (status)
  (member status '(:pending :queued :intercepting)))

(defun tool-reply (service id result &optional (raw result))
  "Settle call ID with RESULT. RAW is what the tool answered where a hook
rewrote it."
  (let ((cell (assoc id (%pending service) :test #'equal)))
    (when (and cell (outstanding-p (cdr cell)))
      (setf (cdr cell) result)
      (record-done service (list (list id result raw)))
      (emit service (tool-result-event (m:agent-ref service) id result))
      (settle-calls service))
    nil))

(defun settle-calls (service)
  "A call's cell just settled: start the next queued one, and once none is
outstanding close the turn's calls and go on to the next turn."
  (pump-calls service)
  (when (notany (lambda (c) (outstanding-p (cdr c))) (%pending service))
    (close-pending-calls service)
    (cast-step service)))

;;; --- detached calls -------------------------------------------------------

;;; A detached call has its cell settled with a stub, so the turn goes on, but
;;; keeps running: it is held in %DETACHED under the step ref it was dispatched
;;; in until its result lands, and is closed with the run.

(defun detach-call (service ref id name)
  "Settle call ID, dispatched under step REF, with a stub while it runs on. A
call that has answered, or belongs to an earlier step, is left alone. Past
:MAX-DETACHED it waits for a slot instead. True when it detached."
  (let ((cell (assoc id (%pending service) :test #'equal)))
    (when (and cell (eql ref (%step-ref service)) (eq (cdr cell) :pending))
      (if (detached-slot-free-p service)
          (progn
            (push (list (cons ref id) :name name :token (call-token service id)
                        :log-id (journal-call-id service id))
                  (%detached service))
            (setf (cdr cell) (ok :status "running" :note "the result follows in a later message"))
            (emit service (tool-detached-event (m:agent-ref service) id name))
            (settle-calls service)
            t)
          (setf (%detach-deferred service)
                (append (%detach-deferred service) (list (list ref id name))))))))

(defun detached-slot-free-p (service)
  (let ((cap (agent-max-detached service)))
    (or (null cap) (< (length (%detached service)) cap))))

(defun detach-deferred (service)
  "Detach the oldest deferred call that still can, now a slot is free."
  (loop while (and (%detach-deferred service) (detached-slot-free-p service))
        do (when (apply #'detach-call service (pop (%detach-deferred service)))
             (return))))

(defun detached-reply (service key result &optional (raw result))
  "Log and announce the result of the detached call KEY, and queue it as a
:USER message for the next turn, as a steer is. Its slot goes to a call
deferred by :MAX-DETACHED. RAW is what the tool answered where a hook rewrote
it."
  (let ((entry (assoc key (%detached service) :test #'equal)))
    (when entry
      (setf (%detached service) (remove entry (%detached service)))
      (destructuring-bind (&key name log-id (call-id (cdr key)) &allow-other-keys) (cdr entry)
        (record-log-done service (list (list log-id result raw)))
        (emit service
                    (tool-result-event (m:agent-ref service) call-id result))
        (push (list nil nil
                    (list :role :user
                          :content (format nil "[tool call ~a (~(~a~)) finished: ~a]"
                                           call-id name
                                           (%cut-text (render-tool-result result)
                                                      (agent-max-tool-result service))))
                    nil)
              (%steer-queue service))
        (detach-deferred service)
        (wake-if-waiting service))))
  nil)

(defun wake-if-waiting (service)
  "Give a run that stopped to wait on detached calls the turn its new message
is owed. Cast rather than stepped in place, so a turn already cast for is not
issued twice."
  (when (shiftf (%awaiting-detached service) nil)
    (cast-step service)))

(defun cast-step (service)
  (m:cast (m:self) (list :step (%run-id service))))

(defun close-detached (service outcome)
  "Cancel every detached call and log it finished as OUTCOME."
  (let ((entries (shiftf (%detached service) nil)))
    (setf (%awaiting-detached service) nil)
    (dolist (entry entries)
      (destructuring-bind (&key name token log-id &allow-other-keys) (cdr entry)
        (declare (ignore name))
        (cancel token)
        (a:when-let ((path (journal-of service)))
          (when log-id (journal-call-done path (list (list log-id outcome nil)))))
        (when (eq outcome :interrupted)
          (emit service
                      (tool-result-event (m:agent-ref service) (cdr (car entry))
                                         (fail :interrupted))))))))

;;; --- resuming calls (~takeiteasy/miao#77) ---------------------------------

;;; A call the call log holds as :LOST, :ABANDONED or :INTERRUPTED is run again
;;; as a new call, which joins %DETACHED, so its result lands as a detached
;;; call's does. The process that ran the first is gone, so nothing reattaches.

(defun resume-problem (service args)
  "The bad-request ARGS' :RESUME earns, if any."
  (let ((ids (getf args :resume)))
    (cond ((null ids) nil)
          ((not (and (listp ids) (every #'stringp ids)))
           (bad-request ":resume must be a list of call log ids"))
          ((not (journal-of service)) (bad-request ":resume needs a :journal"))
          ((not (or (getf args :messages) (getf args :continue)))
           (bad-request ":resume needs :messages or :continue")))))

(defun resume-request (service args)
  "Resume the calls of :IDS in the run under way, or start a run that waits
on them, continuing the conversation."
  (let ((ids (getf args :ids))
        (force (getf args :force)))
    (cond ((null ids) (bad-request ":ids is required"))
          ((resume-problem service (list :resume ids :continue t)))
          ((%running-p service)
           (multiple-value-bind (answer resumed) (resume-calls service ids force)
             (dispatch-resumed-calls service resumed)
             answer))
          (t (start-run service (list :continue t :resume ids :force force))))))

(defun resume-refusal (service entry force)
  "Why the logged call ENTRY may not be run again by SERVICE, or nil and its
arguments."
  (let* ((name (getf entry :name))
         (metadata (and (member name (%allow-list service))
                        (ignore-errors (tool-metadata name :registry (m:service-registry service))))))
    (cond ((eq name +sub-agent-tool-name+) "a sub-agent call cannot be resumed")
          ((null metadata) (format nil "~(~a~) is not in this agent's tool allow-list" name))
          ((not (or force (getf metadata :resumable)))
           (format nil "~(~a~) is not resumable" name))
          (t (handler-case
                 (let ((parsed (json:parse (getf entry :arguments))))
                   (values nil (and (hash-table-p parsed)
                                    (json->arguments parsed (tool-schema metadata)))))
               (error () "its arguments do not parse"))))))

(defun resume-calls (service ids force)
  "Choose and log the calls IDS to run again, and answer (:OK :RESUMED ((old-id
. new-id) ...) :REFUSED ((id reason) ...)) and the chosen calls, for
DISPATCH-RESUMED-CALLS to run, each as a detached call.
FORCE resumes a call to a tool that is not :RESUMABLE. A resumed call is
detached, so it counts toward :MAX-DETACHED, and one past it is refused."
  (multiple-value-bind (resumed refused)
      (journal-call-resume (journal-of service) ids (m:service-name service) (%turns service)
                       (let ((room (a:when-let ((cap (agent-max-detached service)))
                                     (- cap (length (%detached service))))))
                         (lambda (entry)
                           (multiple-value-bind (reason arguments)
                               (resume-refusal service entry force)
                             (cond (reason reason)
                                   ((and room (<= room 0)) "max-detached reached")
                                   (t (when room (decf room))
                                      (values nil arguments)))))))
    (values (ok :resumed (mapcar (lambda (item) (cons (first item) (second item))) resumed)
                :refused refused)
            resumed)))

(defun dispatch-resumed-calls (service resumed)
  (dolist (item resumed)
    (destructuring-bind (old new entry arguments) item
      (declare (ignore old))
      (dispatch-resumed service new entry arguments))))

(defun dispatch-resumed (service log-id entry arguments)
  (let ((name (getf entry :name))
        (call-id (getf entry :call-id))
        (token (make-cancel-token))
        (ref (%step-ref service)))
    (push (list (cons ref log-id) :name name :call-id call-id :token token :log-id log-id)
          (%detached service))
    (journal-call-running (journal-of service) (list log-id))
    (emit service (tool-resumed-event (m:agent-ref service) call-id name))
    (flet ((send (arguments)
             (send-call service (list :tool ref log-id) #'%tool-call name
                        (list* :cancel token arguments) :unbounded t)
             nil))
      (if (phase-hooks service :before-tool-call)
          (run-chain service :before-tool-call arguments (list :id call-id :name name :resumed t)
                     (lambda (status value)
                       (if (eq status :ok)
                           (send arguments)
                           (detached-reply service (cons ref log-id)
                                           (hook-refusal status (first value) (second value)))))
                     :detached t)
          (send arguments)))
    nil))

(defun pending-tool-messages (service)
  "A :TOOL message for each call dispatched this turn, in order: its result,
or an :INTERRUPTED error where none has arrived."
  (mapcar (lambda (id)
            (let ((result (cdr (assoc id (%pending service) :test #'equal))))
              (tool-message id (if (outstanding-p result) (fail :interrupted) result))))
          (%pending-order service)))

(defun cancel-pending-calls (service)
  "Cancel each call dispatched this turn that has no result yet."
  (setf (%queued service) nil
        (%detach-deferred service) nil)
  (dolist (cell (%pending service))
    (when (eq (cdr cell) :pending)
      (cancel (call-token service (car cell))))))

(defun close-pending-calls (service)
  (cancel-pending-calls service)
  (record-outstanding service :interrupted)
  (dolist (cell (%pending service))
    (when (outstanding-p (cdr cell))
      (setf (cdr cell) (fail :interrupted))
      (emit service
                  (tool-result-event (m:agent-ref service) (car cell) (cdr cell)))))
  (dolist (message (pending-tool-messages service))
    (push-message service message))
  (setf (%pending service) nil
        (%pending-order service) nil
        (%call-tokens service) nil
        (%journal-call-ids service) nil))

;;; --- the journal (~takeiteasy/miao#119) -------------------------------------

(defun journal-of (service)
  (%journal-path (agent-journal service)))

(defun record-journal (service kind &rest fields)
  (a:when-let ((path (journal-of service)))
    (apply #'journal-append path
           (a:when-let ((handle (%run-handle service))) (run-handle-key handle))
           (m:service-name service)
           (and (slot-boundp service 'parent-name) (agent-parent-name service))
           kind fields)))

(defun journal-event (service event)
  "Journal EVENT, an event as emitted, unless it is a streamed delta."
  (unless (member (getf event :type) +journal-skipped-events+)
    (apply #'record-journal service :event
           (journal-event-fields (a:remove-from-plist event :agent :parent)
                                 (or (agent-max-tool-result service) *journal-max-content*)))))

(defun journal-run-start (service args continuing)
  "Journal what a run begins from: the agent's settings and its conversation, which
a run that does not continue one replaces."
  (when (journal-of service)
    (record-journal service :settings :settings (agent-settings service))
    (record-journal service :messages :reset (not continuing)
                    :messages (if continuing (getf args :messages) (reverse (%messages service))))))

;;; --- the call records (~takeiteasy/miao#73) ---------------------------------

(defun journal-call-id (service id)
  (cdr (assoc id (%journal-call-ids service) :test #'equal)))

(defun record-running (service ids)
  (a:when-let ((path (journal-of service)))
    (journal-call-running path (remove nil (mapcar (lambda (id) (journal-call-id service id)) ids)))))

(defun record-done (service results)
  "Log each of RESULTS, (provider call id, result, raw result), as finished."
  (record-log-done service (loop for (id result raw) in results
                                 collect (list (journal-call-id service id) result raw))))

(defun record-log-done (service results)
  "Log each of RESULTS, (log id, result, raw result), as finished; one with no
log id is skipped. A raw result that is not RESULT goes to the log under
:LOG-RAW."
  (a:when-let ((path (journal-of service)))
    (let ((cap (or (agent-max-tool-result service) *journal-max-content*)))
      (journal-call-done
       path
       (loop for (log-id result raw) in results
             when log-id
               collect (list log-id
                             (cond ((not (tool-error-p result)) :ok)
                                   ((eq (tool-error result) :interrupted) :interrupted)
                                   ((denial-p result) :denied)
                                   (t :error))
                             (%cut-text (render-tool-result result) cap)
                             (and (agent-log-raw service) raw (not (equal raw result))
                                  (%cut-text (render-tool-result raw) cap))))))))

(defun denial-p (result)
  (let ((reason (tool-error result)))
    (and (consp reason) (eq (first reason) :denied))))

(defun record-outstanding (service outcome)
  "Log every call this turn still awaiting a result as finished with OUTCOME."
  (a:when-let ((path (journal-of service)))
    (journal-call-done
     path
     (loop for cell in (%pending service)
           for log-id = (journal-call-id service (car cell))
           when (and log-id (outstanding-p (cdr cell)))
             collect (list log-id outcome nil)))))

(defun tool-message (id result)
  (list :role :tool :tool-call-id id :content (render-tool-result result)))

(defun render-tool-result (result)
  "RESULT, an (:ok plist) or (:error reason), as JSON text -- more legible to
a model than PRINC-TO-STRING, and jzon is already a dependency."
  (json:stringify
   (if (tool-error-p result)
       (json-object "error" (untyped->json (tool-error result)))
       (untyped->json (second result)))))

;;; --- fitting the conversation to a request --------------------------------

;;; The conversation is kept whole; each request carries a view of it. A tool
;;; result past :MAX-TOOL-RESULT is cut in the view, and when the view is
;;; still past :MAX-CONTEXT the oldest turns are left out of it. What changed
;;; comes back as a record, indexed into the whole conversation, that
;;; BUILD-REQUEST sends the sink as :CONTEXT-TRIMMED.

(defparameter +context-margin+ 9/10
  "The share of :MAX-CONTEXT a request may fill: the ratio drifts with content,
and code and JSON tokenise worse than prose.")

(defparameter +min-chars-per-token+ 1)
(defparameter +max-chars-per-token+ 8)

(defparameter +omitted-note+ "[~d earlier messages omitted to fit the context budget]")

(defun %cut-text (text cap)
  "TEXT, cut at CAP characters with a note of how much was dropped, so the
model knows it saw part of it."
  (if (and cap (> (length text) cap))
      (format nil "~a... [truncated: ~d characters, first ~d kept]"
              (subseq text 0 cap) (length text) cap)
      text))

(defun %printed-size (object)
  (length (let ((*print-pretty* nil)) (prin1-to-string object))))

(defun %message-size (message)
  (+ (length (content-text (getf message :content)))
     (let ((calls (getf message :tool-calls)))
       ;; The schema a call carries never reaches the wire.
       (if calls
           (%printed-size (mapcar (lambda (call) (a:remove-from-plist call :schema)) calls))
           0))))

(defun %conversation-units (messages)
  "The indices of MESSAGES that go together, oldest first: an assistant turn
with tool calls and the :TOOL replies after it, or any other message alone.
A :SYSTEM message is in none."
  (let* ((messages (coerce messages 'vector))
         (units '()) (i 0) (n (length messages)))
    (loop while (< i n)
          do (let ((message (aref messages i)))
               (cond ((eq (getf message :role) :system) (incf i))
                     ((getf message :tool-calls)
                      (let ((end (1+ i)))
                        (loop while (and (< end n) (eq (getf (aref messages end) :role) :tool))
                              do (incf end))
                        (push (loop for k from i below end collect k) units)
                        (setf i end)))
                     (t (push (list i) units) (incf i)))))
    (nreverse units)))

;; TODO: the oldest turns are dropped outright and only a note stands in for
;; them; summarise the dropped span instead, under the policy of the
;; orchestrator DSL (~takeiteasy/miao#139).
(defun fit-conversation (messages &key max-context max-tool-result
                                    (chars-per-token 1) (margin 1) (reserved 0))
  "MESSAGES as a request should carry them, a record of what was changed, or
nil when nothing was, and the characters the request measures. Each :TOOL
message is cut to MAX-TOOL-RESULT characters. MAX-CONTEXT is in tokens, each
CHARS-PER-TOKEN characters, of which the request may fill MARGIN (a fraction);
RESERVED characters, the tool schemas, count against it. If the whole is
still past that, the oldest units -- see %CONVERSATION-UNITS -- are left out
until it fits, a note in their place. A :SYSTEM message and the newest unit are
never left out; if they alone are past the budget the request is sent anyway,
and the record says so. Pure: no I/O, and MESSAGES is not changed.

The record is (:OMITTED indices :TRUNCATED ((index :FROM n :TO m) ...) :SIZE n
:BUDGET b :RATIO r :OVER-BUDGET bool), indices being positions in MESSAGES,
:SIZE and :BUDGET in estimated tokens, and :FROM and :TO in characters."
  (let* ((cut '())
         (view (loop for message in messages
                     for index from 0
                     collect (let* ((text (and max-tool-result
                                               (eq (getf message :role) :tool)
                                               (content-text (getf message :content))))
                                    (kept (and text (%cut-text text max-tool-result))))
                               (cond ((and text (/= (length text) (length kept)))
                                      (push (list index :from (length text) :to max-tool-result)
                                            cut)
                                      (list* :content kept (a:remove-from-plist message :content)))
                                     (t message)))))
         (sizes (map 'vector #'%message-size view))
         (chars (+ reserved (reduce #'+ sizes)))
         (limit (and max-context (* max-context margin chars-per-token)))
         (droppable (butlast (%conversation-units view)))
         (note-size (length (format nil +omitted-note+ (length messages))))
         (omitted '()))
    (when limit
      (loop while (and droppable (> (+ chars (if omitted note-size 0)) limit))
            do (dolist (index (pop droppable))
                 (decf chars (aref sizes index))
                 (push index omitted))))
    (setf omitted (sort omitted #'<))
    (let* ((chars (+ chars (if omitted note-size 0)))
           (over-budget (and limit (> chars limit))))
      (if (not (or omitted cut over-budget))
          (values messages nil chars)
          (values (fit-view view omitted)
                  (list :omitted omitted :truncated (nreverse cut)
                        :size (ceiling chars chars-per-token) :budget max-context
                        :ratio chars-per-token :over-budget (and over-budget t))
                  chars)))))

(defun fit-view (view omitted)
  "VIEW without the messages at the indices OMITTED, a note standing in for
them ahead of the first one kept that is not a :SYSTEM message."
  (let ((noted (null omitted)) (out '())
        (gone (make-hash-table)))
    (dolist (index omitted) (setf (gethash index gone) t))
    (loop for message in view
          for index from 0
          unless (gethash index gone)
            do (unless (or noted (eq (getf message :role) :system))
                 (setf noted t)
                 (push (list :role :user
                             :content (format nil +omitted-note+ (length omitted)))
                       out))
               (push message out))
    (nreverse out)))

(defun calibrate (service reply)
  "Set SERVICE's characters per token from REPLY's prompt-token count over the
characters the request that drew it measured. A reply that reports no count, or
one that reads implausibly, leaves the last ratio."
  (let ((tokens (reply-prompt-tokens reply))
        (chars (%last-request-chars service)))
    (when (and tokens chars)
      (let ((ratio (/ chars tokens 1d0)))
        (when (<= +min-chars-per-token+ ratio +max-chars-per-token+)
          (setf (%chars-per-token service) ratio))))))

(defun conversation (service)
  "SERVICE's messages, oldest first, in a list of its own."
  (reverse (%messages service)))

(defun push-message (service message)
  (record-journal service :message :message message)
  (push message (%messages service)))

;;; --- interceptor hooks (~takeiteasy/miao#117) -----------------------------

;;; A hook is a service the agent calls with M:CALL-ASYNC, as it does a tool, so
;;; HANDLE never waits on one: the answer comes back as a (:REPLY (:HOOK ref
;;; chain) value status) message, under the hook's own timeout. The hooks of a
;;; phase run as a chain, one at a time, each over what the last left. A chain
;;; carries the step ref it was started under, so an interrupt, cancel or
;;; restore, which move it on, drop the answer that is still to come.
;;;
;;; The hooks a run goes through are resolved when it starts, and each one's
;;; metadata is read then, so a hook that dies mid-run still fails by the
;;; policy it declared.

(defstruct (hook-entry (:constructor make-hook-entry (name target phases on-error on-resume timeout)))
  name target phases on-error on-resume timeout)

(defstruct chain id ref phase entries current token subject extra k)

(defun hook-spec-label (spec)
  "SPEC, one of an agent's :HOOKS, as the name it shows under."
  (typecase spec
    (hook-entry (hook-entry-name spec))
    (cons (if (and (keywordp (first spec)) (not (functionp (first spec))))
              (first spec)
              (or (getf (rest spec) :name) :function)))
    (keyword spec)
    (t :function)))

(defun resolve-hooks (service)
  "SERVICE's :HOOKS as hook entries, or nil and the bad-request that refuses
the run: a named hook that is not registered, or a bad option."
  (let ((entries '()))
    (dolist (spec (agent-hooks-spec service) (values (nreverse entries) nil))
      (multiple-value-bind (entry problem) (resolve-hook service spec)
        (when problem (return (values nil problem)))
        (push entry entries)))))

(defun resolve-hook (service spec)
  (if (typep spec 'hook-entry)
      spec
      (let* ((spec (a:ensure-list spec))
             (target (first spec))
             (options (rest spec))
             (named (and (keywordp target) (not (functionp target)))))
        (multiple-value-bind (props problem)
            (if named
                (multiple-value-bind (process props)
                    (m:lookup target :registry (m:service-registry service))
                  (if (and process (eq (getf props :kind) :hook))
                      props
                      (values nil (bad-request "hook ~(~a~) is not registered" target))))
                (values nil nil))
          (if problem
              (values nil problem)
              (let ((phases (getf options :phases (getf props :phases +hook-phases+)))
                    (on-error (getf options :on-error (getf props :on-error :deny)))
                    (on-resume (getf options :on-resume (getf props :on-resume :run)))
                    (timeout (getf options :timeout (getf props :timeout +default-tool-timeout+)))
                    (label (if named target (getf options :name :function))))
                (a:if-let ((bad (%check-hook-options phases on-error on-resume)))
                  (values nil (bad-request "~a" bad))
                  (make-hook-entry label
                                   (if named
                                       target
                                       (hook-function-process service spec label phases
                                                              on-error on-resume timeout))
                                   phases on-error on-resume timeout))))))))

(defun hook-function-process (service spec label phases on-error on-resume timeout)
  "The service running SPEC's function, mounted when first needed and kept for
the agent's life."
  (let ((cell (assoc spec (%hook-services service) :test #'eq)))
    (if (and cell (m:process-alive-p (cdr cell)))
        (cdr cell)
        (let ((process (m:mount (m:service-process (m:service-context service))
                                'hook-function :fn (first spec) :label label :phases phases
                                :on-error on-error :on-resume on-resume
                                :timeout timeout :restart :temporary)))
          (setf (%hook-services service)
                (acons spec process (remove cell (%hook-services service))))
          process))))

(defun stop-hook-services (service)
  (dolist (cell (shiftf (%hook-services service) nil))
    (ignore-errors (m:stop (cdr cell)))))

(defun phase-hooks (service phase)
  (remove-if-not (lambda (entry) (member phase (hook-entry-phases entry)))
                 (%hooks service)))

(defun run-chain (service phase subject extra k &key detached)
  "Pass SUBJECT through PHASE's hooks, in order. EXTRA is the plist the hooks are
told besides it, :ID and :NAME for a tool call. K is called, and its values
returned, with :OK and the subject the hooks left; or with :DENIED or :FAILED
and (hook reason subject), once a hook denies, or one fails that is closed. K
runs at once when no hook applies, and otherwise from HOOK-REPLY.

A chain is dropped when the step it started under passes, unless it is
DETACHED: that of a call that outlives its turn, which only the end of the run
drops."
  (let ((entries (remove-if (lambda (entry)
                              (and (getf extra :resumed) (eq (hook-entry-on-resume entry) :skip)))
                            (phase-hooks service phase))))
    (if (null entries)
        (funcall k :ok subject)
        (let ((chain (make-chain :id (incf (%chain-seq service))
                                 :ref (if detached :detached (%step-ref service))
                                 :phase phase :entries entries :subject subject
                                 :extra extra :k k)))
          (push (cons (chain-id chain) chain) (%chains service))
          (chain-next service chain)))))

(defun chain-next (service chain)
  (let ((entry (pop (chain-entries chain))))
    (cond ((null entry) (chain-done service chain :ok (chain-subject chain)))
          (t (setf (chain-current chain) entry
                   (chain-token chain) (make-cancel-token))
             (send-call service (list :hook (chain-ref chain) (chain-id chain))
                        #'%hook-call entry (hook-request service chain))
             nil))))

(defun chain-done (service chain status value)
  (setf (%chains service) (remove (chain-id chain) (%chains service) :key #'car))
  (funcall (chain-k chain) status value))

(defun hook-request (service chain)
  (append (list :phase (chain-phase chain) :agent (m:service-name service)
                :cancel (chain-token chain)
                :run (run-handle-id (%run-handle service))
                :handle (%run-handle service)
                :emit (hook-emitter service (hook-entry-name (chain-current chain))))
          (and (slot-boundp service 'parent-name) (list :parent (agent-parent-name service)))
          (chain-extra chain)
          (list (getf +hook-subject-keys+ (chain-phase chain)) (chain-subject chain))))

(defun %hook-call (entry request &key (registry m:*registry*))
  "What to send ENTRY's hook to intercept REQUEST: (values process message
timeout), as %TOOL-CALL answers for a tool."
  (let ((target (hook-entry-target entry)))
    (values (if (typep target 'm:process)
                target
                (or (m:lookup target :registry registry)
                    (error "No hook registered under ~s." target)))
            (cons :intercept request)
            (/ (hook-entry-timeout entry) 1000))))

(defun hook-reply (service ref id result)
  "Hand RESULT, a hook's answer, to the chain ID, unless the step it was
started under has passed."
  (let ((chain (cdr (assoc id (%chains service)))))
    (when chain (cancel (chain-token chain)))
    (cond ((null chain) nil)
          ((not (or (eq ref :detached) (eql ref (%step-ref service))))
           (setf (%chains service) (remove id (%chains service) :key #'car))
           nil)
          (t (chain-answer service chain result)))))

(defun drop-chains (service &key detached)
  "Forget the chains in flight and tell the hook each is waiting on to stop. A
DETACHED chain, that of a call that outlives its turn, is dropped only when
DETACHED is true."
  (dolist (cell (%chains service))
    (let ((chain (cdr cell)))
      (when (or detached (not (eq (chain-ref chain) :detached)))
        (cancel (chain-token chain))
        (setf (%chains service) (remove cell (%chains service)))))))

(defun chain-answer (service chain result)
  (let ((entry (chain-current chain))
        (phase (chain-phase chain))
        (id (getf (chain-extra chain) :id)))
    (multiple-value-bind (kind value) (interpret-hook-answer phase result)
      (flet ((announce (action &optional reason)
               (emit service (hook-event (m:agent-ref service) phase (hook-entry-name entry)
                                         id action reason)))
             (finish (status reason)
               (chain-done service chain status
                           (list (hook-entry-name entry) reason (chain-subject chain)))))
        (ecase kind
          (:pass (chain-next service chain))
          (:rewrite (unless (equal value (chain-subject chain))
                      (announce :rewrite)
                      (setf (chain-subject chain) value))
           (chain-next service chain))
          (:deny (announce :deny value)
           (finish :denied value))
          (:failed (announce :failed value)
           (if (eq (hook-entry-on-error entry) :pass)
               (chain-next service chain)
               (finish :failed value))))))))

(defun hook-failure (value)
  "The error a run ends in when a closed before-turn hook fails: VALUE is (hook
reason subject)."
  (fail (list :hook-failed (first value) (second value))))

(defun hook-refusal (status hook reason)
  "The error a tool call or result is answered with when a hook denied the call
or failed closed."
  (fail (if (eq status :denied)
            (list :denied hook reason)
            (list :hook-failed hook reason))))

(defun hook-event (ref phase hook id action &optional reason)
  (list* :type :hook :ref ref :phase phase :hook hook :id id :action action
         (and reason (list :reason reason))))

;;; A hook reaches the front end through :EMIT, which puts an event of its own
;;; into the root's event stream, and tells runs apart by :RUN. Both belong to
;;; the root run, so a sub-agent's hooks share them with its parent's.

(defvar *run-seq-lock* (bt:make-lock))
(defvar *run-seq* 0)

(defstruct (run-handle (:constructor make-run-handle (fanout)))
  (id (bt:with-lock-held (*run-seq-lock*) (incf *run-seq*)))
  (key (%vault-id))
  fanout (live t) (lock (bt:make-lock)))

(defparameter +loop-event-types+
  '(:run-start :steer :turn :turn-retry :turn-interrupted :text-delta :tool-call-delta
    :done :reply :tool-call :tool-detached :tool-resumed :tool-result :hook :context-trimmed
    :run-done)
  "The events the loop emits itself, which a hook may not forge.")

(defun close-run-handle (service)
  "End the root run's handle, once its :RUN-DONE is to go out: a hook's event
after that is dropped."
  (a:when-let ((handle (and (not (slot-boundp service 'parent-name)) (%run-handle service))))
    (bt:with-lock-held ((run-handle-lock handle))
      (setf (run-handle-live handle) nil))))

(defun notify-hooks-run-done (service reason)
  "Tell each named hook of the root run that it ended, as a cast: a hook keeps
state for a run, and drops it here. A sub-agent's hooks are its parent's and
share its handle, so the root's notice covers the tree."
  (a:when-let ((handle (and (not (slot-boundp service 'parent-name)) (%run-handle service))))
    (dolist (target (remove-duplicates
                     (loop for entry in (%hooks service)
                           for target = (hook-entry-target entry)
                           when (and (keywordp target) (not (functionp target)))
                             collect target)))
      (a:when-let ((process (m:lookup target :registry (m:service-registry service))))
        (m:cast process (list :run-done :run (run-handle-id handle) :handle handle
                                        :agent (m:service-name service) :reason reason))))))

(defun hook-emitter (service hook)
  "The :EMIT function of HOOK's calls from SERVICE: it delivers a plist event,
tagged with the hook and the agent, while the root run lasts. Under the
handle's lock, so :RUN-DONE is the last event of the run."
  (let ((handle (%run-handle service))
        (ref (m:agent-ref service))
        (tags (append (list :agent (m:service-name service))
                      (and (slot-boundp service 'parent-name)
                           (list :parent (agent-parent-name service))))))
    (lambda (event)
      (when (and (a:proper-list-p event) (evenp (length event))
                 (keywordp (getf event :type))
                 (not (member (getf event :type) +loop-event-types+)))
        (bt:with-lock-held ((run-handle-lock handle))
          (when (run-handle-live handle)
            (emit-event (run-handle-fanout handle)
                        (append event (list :hook hook :ref ref) tags))))))))

;;; --- the sink -----------------------------------------------------------

;;; Every event reaches the sink through AGENT-EVENTS: a function sink is
;;; called from the agent's emitter, which a sub-agent is handed as its own
;;; sink, so the whole tree calls it one event at a time, in order, and a sink
;;; that blocks never holds up HANDLE. :RUN-DONE is the last event it sees.

(defun agent-events (service)
  "Where SERVICE's events go: its fanout over the :SINK and the subscribers,
which is nil while no run is under way."
  (%fanout service))

(defun emit (service event)
  "Deliver EVENT, tagged with SERVICE's name and, for a child, its parent's, and
journal it."
  (let ((event (append event
                       (list :agent (m:service-name service))
                       (and (slot-boundp service 'parent-name)
                            (list :parent (agent-parent-name service))))))
    (journal-event service event)
    (when (%fanout service)
      (emit-event (%fanout service) event))))

(defvar *subscribers-lock* (bt:make-lock))
(defvar *subscribers* (make-hash-table :test 'eq :weakness :key)
  "Registry -> name -> subscribed sinks, so a named agent's subscribers outlive
the fresh instance mount restarts it as after a crash.")

(defun subscribers (service)
  (a:if-let ((name (m:service-name service)))
    (bt:with-lock-held (*subscribers-lock*)
      (a:when-let ((table (gethash (m:service-registry service) *subscribers*)))
        (gethash name table)))
    (%local-subscribers service)))

(defun (setf subscribers) (sinks service)
  (a:if-let ((name (m:service-name service)))
    (bt:with-lock-held (*subscribers-lock*)
      (let ((table (or (gethash (m:service-registry service) *subscribers*)
                       (setf (gethash (m:service-registry service) *subscribers*)
                             (make-hash-table :test 'equal)))))
        (setf (gethash name table) sinks)))
    (setf (%local-subscribers service) sinks)))

(defun sink-target (service sink)
  "Where SINK's events go: an emitter SERVICE starts for a function, otherwise
SINK itself."
  (a:if-let ((emitter (start-emitter sink)))
    (progn (push (cons sink emitter) (%emitters service))
           emitter)
    sink))

(defun open-fanout (service)
  ;; A sub-agent's fanout sits inside its parent's, which records for both.
  (let ((fanout (make-fanout nil (not (slot-boundp service 'parent-name)))))
    (dolist (sink (append (and (agent-sink service) (list (agent-sink service)))
                          (subscribers service)))
      (fanout-add fanout (sink-target service sink)))
    (setf (%fanout service) fanout)))

(defun retire-emitter (emitter)
  "Stop EMITTER once it has delivered what it was sent, and kill it if the sink
has not taken that within *SINK-GRACE*."
  (stop-emitter emitter)
  (reap-emitter emitter *sink-grace*))

(defun retire-emitters (service)
  "Retire the emitters SERVICE started, and only those: a child's sink is its
parent's fanout, which the parent's own run-end retires."
  (close-run-handle service)
  (setf (%fanout service) nil)
  (dolist (cell (shiftf (%emitters service) nil))
    (retire-emitter (cdr cell))))

(defun subscribe (service sink)
  (cond ((not (and sink (typep sink '(or function symbol m:process))))
         (bad-request ":subscribe takes a function, a symbol or a process"))
        (t (unless (or (eq sink (agent-sink service))
                       (member sink (subscribers service)))
             (setf (subscribers service) (append (subscribers service) (list sink)))
             (a:when-let ((fanout (%fanout service)))
               (fanout-add fanout (sink-target service sink) :replay t)))
           (if (%running-p service)
               (ok :running t :turn (%turns service))
               (ok :running nil)))))

(defun unsubscribe (service sink)
  (cond ((eq sink (agent-sink service))
         (bad-request "the mount :sink cannot be unsubscribed"))
        ((not (member sink (subscribers service)))
         (bad-request "~s is not subscribed" sink))
        (t (setf (subscribers service) (remove sink (subscribers service)))
           (let ((cell (assoc sink (%emitters service)))
                 (fanout (%fanout service)))
             (when fanout
               (fanout-remove fanout (if cell (cdr cell) sink)))
             (when cell
               (setf (%emitters service) (remove cell (%emitters service)))
               (retire-emitter (cdr cell))))
           :ok)))

;;; --- a turn's stream ------------------------------------------------------

;;; The events a turn streams pass through one of these on their way to the
;;; sink. Closing it takes the same lock, so nothing from an abandoned turn
;;; reaches the sink after its :TURN-INTERRUPTED or :RUN-DONE, and the text
;;; kept is exactly the text the sink saw.

(defstruct (turn-stream (:constructor make-turn-stream ()))
  (lock (bt:make-lock :name "miao-turn-stream"))
  (text (make-string-output-stream))
  (superseded nil))

(defun turn-stream-sink (stream service)
  (lambda (event)
    (bt:with-lock-held ((turn-stream-lock stream))
      (unless (turn-stream-superseded stream)
        (when (eq (getf event :type) :text-delta)
          (write-string (getf event :text) (turn-stream-text stream)))
        (emit service event)))))

(defun close-turn-stream (service &optional last-event)
  "Close the turn's stream to further events, emit LAST-EVENT, if given, and
return the text it streamed. With no sink there is no stream, and nothing to
tell or keep."
  (let ((stream (shiftf (%turn-stream service) nil)))
    (if stream
        (bt:with-lock-held ((turn-stream-lock stream))
          (setf (turn-stream-superseded stream) t)
          (when last-event
            (emit service last-event))
          (get-output-stream-string (turn-stream-text stream)))
        "")))

(defun cancel-turn (service)
  "Stop the completion in flight, if any, rather than leave it to its own
timeout."
  (a:when-let ((token (shiftf (%turn-token service) nil)))
    (cancel token)))

(defun finish-run (service result)
  (cancel-deadline service)
  (close-detached service :interrupted)
  (cancel-retry service)
  (close-turn-stream service)
  (cancel-turn service)
  (setf (%running-p service) nil
        (%turn-in-flight service) nil
        (%pending service) nil
        (%pending-order service) nil
        (%queued service) nil
        (%call-tokens service) nil
        (%journal-call-ids service) nil)
  (drop-chains service :detached t)
  (record-input-done service (if (tool-error-p result)
                                 :error
                                 (getf (second result) :stop-reason)))
  ;; Invalidates any turn already in flight, so its late TURN-REPLY is
  ;; dropped rather than reopening a run that has already finished.
  (incf (%step-ref service))
  (let ((reason (if (tool-error-p result)
                    (tool-error result)
                    (getf (second result) :stop-reason))))
    (notify-hooks-run-done service reason)
    (close-run-handle service)
    (emit service (run-done-event (m:agent-ref service) reason)))
  (retire-emitters service)
  (if (m:agent-parent service)
      (values :done result)
      result))

(defun arm-deadline (service)
  (let ((self (m:self))
        (id (%run-id service)))
    (setf (%cancel-timer service)
          (m:after service (/ (agent-deadline service) 1000.0d0)
                   (lambda () (m:cast self (list :deadline id)))))))

(defun cancel-deadline (service)
  (a:when-let ((cancel (%cancel-timer service)))
    (funcall cancel)
    (setf (%cancel-timer service) nil)))

;;; --- events ---------------------------------------------------------

;;; The protocol's own :TEXT-DELTA / :TOOL-CALL-DELTA / :DONE pass through
;;; the turn's stream. These are the loop's own, all echoing :REF as the
;;; protocol events do.

(defun run-start-event (ref messages continue)
  (list :type :run-start :ref ref :messages messages :continue continue))

(defun steer-event (ref content interrupt input-id)
  (list :type :steer :ref ref :content content :interrupt interrupt :input-id input-id))

(defun turn-event (ref n) (list :type :turn :ref ref :turn n))

(defun turn-retry-event (ref n attempt reason)
  (list :type :turn-retry :ref ref :turn n :attempt attempt :reason reason))

(defun turn-interrupted-event (ref n) (list :type :turn-interrupted :ref ref :turn n))

(defun tool-call-event (ref id name arguments)
  (list :type :tool-call :ref ref :id id :name name :arguments arguments))

(defun context-trimmed-event (ref n record)
  (list* :type :context-trimmed :ref ref :turn n record))

(defun tool-detached-event (ref id name)
  (list :type :tool-detached :ref ref :id id :name name))

(defun tool-resumed-event (ref id name)
  (list :type :tool-resumed :ref ref :id id :name name))

(defun tool-result-event (ref id result)
  (list :type :tool-result :ref ref :id id :result result))

(defun reply-event (ref n message) (list :type :reply :ref ref :turn n :message message))

(defun run-done-event (ref reason) (list :type :run-done :ref ref :reason reason))

;;; --- a blocking entry point --------------------------------------------

(defun run-agent (context &rest initargs &key messages timeout input-id &allow-other-keys)
  "Delegate an agent on CONTEXT (a mounted context's process), run it to
completion and return its result. The one place miao reaches the loop
synchronously: a plain process is the parent, since a service parent needs
the meow fix a mounted agent does not (~takeiteasy/meow#59, already applied
here but not assumed of the caller's own services). With :INPUT-ID, which
needs :JOURNAL, a redelivered run returns (:OK (:DUPLICATE status)) rather
than running again."
  (let ((deadline (getf initargs :deadline 300000)))
    (m:with-process (%runner)
      (let* ((child (apply #'m:delegate context 'agent
                           (a:remove-from-plist initargs :messages :timeout :input-id)))
             (accepted (multiple-value-call #'%call-result
                         (m:call child (list* :run :messages messages
                                              (and input-id (list :input-id input-id)))))))
        (cond ((not (eq accepted :ok))
               (m:kill child)
               accepted)
              (t (multiple-value-bind (message received)
                     (m:receive :timeout (or timeout (/ deadline 1000.0d0)))
                   (cond
                     ((not received) (fail :timeout))
                     ((eq (first message) :agent-done) (fourth message))
                     (t (fail (list :error (third message))))))))))))

;;; --- checkpoints (~takeiteasy/miao#11) ----------------------------------

;;; The turn and tool calls in flight are replies a restore cannot bring
;;; back, so only their ids are recorded, under :IN-FLIGHT, for
;;; a caller to see the checkpoint was taken mid-run. RESTORE lands a
;;; not-running agent and ignores it. A call with no result yet is recorded
;;; closed as :INTERRUPTED, so the restored conversation is well-formed.

(defun outstanding-log-ids (service)
  "The call log ids of the calls this run has not had an answer to, detached
ones included, for a caller to resume."
  (append (loop for cell in (%pending service)
                for id = (and (outstanding-p (cdr cell)) (journal-call-id service (car cell)))
                when id collect id)
          (loop for entry in (%detached service)
                for id = (getf (cdr entry) :log-id)
                when id collect id)))

(defmethod snapshot ((service agent))
  (append (list :messages (append (conversation service) (pending-tool-messages service))
                :turns (%turns service))
          (when (%running-p service)
            (list :in-flight (append (list :turn (%turns service)
                                           :tool-calls (copy-list (%pending-order service)))
                                     (a:when-let ((ids (outstanding-log-ids service)))
                                       (list :journal-ids ids))
                                     (when (%detached service)
                                       (list :detached (mapcar (lambda (entry) (cdr (car entry)))
                                                               (%detached service)))))))))

(defmethod restore ((service agent) state)
  (cancel-deadline service)
  (cancel-retry service)
  (close-turn-stream service)
  (cancel-turn service)
  (cancel-pending-calls service)
  (drop-chains service :detached t)
  (when (%running-p service)
    (notify-hooks-run-done service :restored))
  (record-outstanding service :interrupted)
  (close-detached service :interrupted)
  (record-input-done service :interrupted)
  (release-steer-claims service)
  (setf (%messages service) (reverse (getf state :messages))
        (%turns service) (getf state :turns)
        (%pending service) nil
        (%pending-order service) nil
        (%call-tokens service) nil
        (%journal-call-ids service) nil
        (%input-log-id service) nil
        (%steer-queue service) nil
        (%turn-in-flight service) nil
        (%running-p service) nil)
  (record-journal service :messages :reset t :messages (getf state :messages))
  ;; As FINISH-RUN does: the turn and tool calls just cancelled still reply
  ;; to this agent's process, so their late replies are made unmatchable --
  ;; each checks the step ref it was issued against.
  (incf (%step-ref service))
  t)

;;; --- forking (~takeiteasy/miao#74) ----------------------------------------

;;; A conversation is append-only, so a prefix of it is a conversation of its
;;; own. A fork keeps a prefix and continues it as a separate agent; the source
;;; is only ever read.

(defun %cut-inside-unit-p (units cut)
  "Whether CUT falls between the messages of one of UNITS."
  (some (lambda (unit) (and (< (first unit) cut) (<= cut (car (last unit))))) units))

(defun %turn-cut (messages units turn)
  "The index after the unit of MESSAGES' TURNth assistant message, or, for turn
0, the index of the first one."
  (let ((assistants (loop for message in messages for index from 0
                          when (eq (getf message :role) :assistant) collect index)))
    (cond ((not (and (integerp turn) (<= 0 turn (length assistants))))
           (error "turn ~s is outside the conversation's ~d turns" turn (length assistants)))
          ((zerop turn) (or (first assistants) (length messages)))
          (t (let ((index (nth (1- turn) assistants)))
               (1+ (car (last (find-if (lambda (unit) (member index unit)) units)))))))))

(defun fork-conversation (messages &key at turn)
  "The prefix of MESSAGES, oldest first, in a list of its own, and the number of
assistant messages in it. :AT n keeps the first n messages; :TURN n keeps up to
and including the nth assistant turn and the tool results it drew, and 0 keeps
what comes before the first. Neither keeps all of it. A cut that would part an
assistant turn from its tool results is an error, as are both keys together.
MESSAGES is not changed."
  (when (and at turn) (error "give :at or :turn, not both"))
  (let* ((units (%conversation-units messages))
         (cut (cond (turn (%turn-cut messages units turn))
                    (at at)
                    (t (length messages)))))
    (unless (and (integerp cut) (<= 0 cut (length messages)))
      (error ":at ~s is outside the conversation's ~d messages" cut (length messages)))
    (when (%cut-inside-unit-p units cut)
      (error ":at ~d falls between an assistant turn and its tool results" cut))
    (let ((prefix (subseq messages 0 cut)))
      (values prefix (count :assistant prefix :key (lambda (m) (getf m :role)))))))

(defun %fork-source-conversation (spec entry run)
  "The conversation of ENTRY, a mounted agent mounted as SPEC: from the journal it
writes when it has one, else a snapshot of it."
  (let ((path (%journal-path (getf (getf spec :initargs) :journal))))
    (cond ((and path (probe-file path))
           (journal-conversation path :agent (getf entry :name) :run run))
          (run (error ":run needs the agent's :journal, which is off or has no log yet"))
          (t (values (getf (m:call (getf entry :process) '(:snapshot)) :messages))))))

(defun fork-agent (context name &key at turn as run)
  "Mount AS, a new agent beside the one registered as NAME under CONTEXT (or
under a context beneath it), holding the prefix of NAME's conversation that
FORK-CONVERSATION's :AT and :TURN pick, and return AS. It is mounted as NAME
was, its :SINK, :VAULT and :JOURNAL included. A NAME with a :JOURNAL is read
from it, at the end of the run keyed RUN (see JOURNAL-RUNS) or its last; any
other from a snapshot, so NAME may be mid-run and carries on, a call it has not
answered closed in the fork as interrupted. Send the fork (:RUN :CONTINUE T) to
go on from the cut."
  (unless as (error ":as, the fork's name, is required"))
  (let* ((entries (%context-entries context :specs t))
         (entry (find name entries :key (lambda (e) (getf e :name))))
         (parent (a:if-let ((parent-name (getf (getf entry :spec) :parent)))
                   (getf (find parent-name entries :key (lambda (e) (getf e :name))) :process)
                   context)))
    (unless entry (error "No agent registered under ~s." name))
    (when (find as entries :key (lambda (e) (getf e :name)))
      (error "~s is already mounted." as))
    (let ((spec (m:child-spec parent name)))
      (unless (subtypep (getf spec :class) 'agent)
        (error "~s is not an agent." name))
      (multiple-value-bind (prefix turns)
          (fork-conversation (%fork-source-conversation spec entry run) :at at :turn turn)
        (let ((fork (apply #'m:mount parent (getf spec :class)
                           :name as
                           (append (getf spec :initargs)
                                   (loop for key in '(:restart :shutdown :backoff :backoff-max)
                                         when (getf spec key) append (list key (getf spec key)))))))
          (m:call fork (list :restore (list :messages prefix :turns turns)))
          as)))))

(defparameter +journal-fork-settings+
  '(:model :tools :sub-agents :max-turns :tool-grace :max-detached :max-tool-result
    :max-context :chars-per-token :turn-retries :retry-backoff :log-raw)
  "The agent settings a journal records that a fork is mounted with.")

(defun fork-journal (context path &key agent run at turn as)
  "Mount AS, a new agent on CONTEXT, holding the prefix of AGENT's conversation
in the journal at PATH that FORK-CONVERSATION's :AT and :TURN pick, and return
AS. It is mounted with the settings AGENT's run RUN (default its last) began with
and journals to PATH. The source need not be mounted, or alive. A setting the
journal does not record, such as :SYSTEM beyond the conversation, :SINK or
:SAMPLING, is the default; named :HOOKS are kept, and a function hook cannot be."
  (unless (and as agent) (error ":agent and :as are required"))
  (when (find as (%context-entries context) :key (lambda (e) (getf e :name)))
    (error "~s is already mounted." as))
  (multiple-value-bind (messages) (journal-conversation path :agent agent :run run)
    (let* ((settings (getf (find-if (lambda (e) (and (eq (getf e :kind) :settings)
                                                     (or (null run) (equal run (getf e :run)))))
                                    (journal-entries path :agent agent)
                                    :from-end t)
                           :settings))
           (hooks (getf settings :hooks)))
      (unless settings (error "No settings for ~s in ~a." agent path))
      (when (member :function hooks)
        (error "~s ran with a function hook, which cannot be mounted again." agent))
      (multiple-value-bind (prefix turns) (fork-conversation messages :at at :turn turn)
        (let ((fork (apply #'m:mount context 'agent :name as :journal path
                           :hooks hooks
                           (loop for key in +journal-fork-settings+
                                 when (member key settings) append (list key (getf settings key))))))
          (m:call fork (list :restore (list :messages prefix :turns turns)))
          as)))))
