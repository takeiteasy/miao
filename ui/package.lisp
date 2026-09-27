(defpackage #:miao/ui
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:m #:meow)
                    (#:bt #:bordeaux-threads-2))
  (:export
   ;; state
   #:make-state #:fold-event #:fold-events
   #:state-status #:state-reason #:state-turn #:state-transcript
   #:state-root #:state-nodes #:state-children
   #:node-key #:node-name #:node-call-id #:node-parent #:node-status #:node-reason
   #:node-turn #:node-transcript
   #:entry-kind #:entry-role #:entry-turn #:entry-id #:entry-name #:entry-arguments
   #:entry-text #:entry-status #:entry-result
   ;; client
   #:attach #:detach #:client-state #:client-agent
   #:run #:continue-run #:steer #:cancel
   #:agent-unavailable #:*command-timeout*))
