(in-package #:miao/ui)

;;; The state an operator sees, folded from an agent's event stream. Pure: a
;;; state is never changed, FOLD-EVENT returns the next one. docs/client-state.md.

(defstruct (entry (:copier copy-entry))
  "One line of a transcript. KIND is :message, :steer, :text, :call or :notice."
  kind role turn id name arguments chunks status result)

(defstruct (node (:copier copy-node))
  "One agent's run. KEY is nil for the root, else the :ref its events carry."
  key name call-id parent (status :idle) reason (turn 0) entries)

(defstruct (state (:copier copy-state))
  (nodes (list (make-node))))

;;; --- reading --------------------------------------------------------------

(defun entry-text (entry)
  (with-output-to-string (out)
    (dolist (chunk (reverse (entry-chunks entry)))
      (write-string chunk out))))

(defun node-transcript (node)
  "NODE's entries, oldest first."
  (reverse (node-entries node)))

(defun state-root (state) (find nil (state-nodes state) :key #'node-key))

(defun state-status (state) (node-status (state-root state)))
(defun state-reason (state) (node-reason (state-root state)))
(defun state-turn (state) (node-turn (state-root state)))
(defun state-transcript (state) (node-transcript (state-root state)))

(defun state-children (state node)
  "The sub-agent nodes NODE delegated to, in the order they started."
  (remove-if-not (lambda (child) (and (node-key child) (equal (node-parent child) (node-key node))))
                 (state-nodes state)))

;;; --- entries --------------------------------------------------------------

(defun text-of (content)
  (if (stringp content) content (or (miao:content-text content) "")))

(defun find-call (node id)
  (find-if (lambda (entry) (and (eq :call (entry-kind entry)) id (equal id (entry-id entry))))
           (node-entries node)))

(defun replace-entry (node old new)
  (setf (node-entries node) (substitute new old (node-entries node) :count 1)))

(defun add-entry (node &rest initargs)
  (let ((entry (apply #'make-entry :turn (node-turn node) initargs)))
    (push entry (node-entries node))
    entry))

(defun update-call (node id name fn)
  "Apply FN to a copy of the call ID, making one if the run so far did not
show it. Deltas may arrive before the call has an id, so a call still
streaming with no id, or the same NAME, stands in for ID."
  (let ((old (or (find-call node id)
                 (find-if (lambda (entry)
                            (and (eq :call (entry-kind entry))
                                 (eq :streaming (entry-status entry))
                                 (or (null id) (null (entry-id entry)))
                                 (or (null name) (equal name (entry-name entry)))))
                          (node-entries node)))))
    (if old
        (let ((new (copy-entry old)))
          (funcall fn new)
          (replace-entry node old new))
        (funcall fn (add-entry node :kind :call :id id :name name :status :streaming)))))

(defun notice (node type &rest detail)
  (add-entry node :kind :notice :name type :result detail))

(defun append-text (node text)
  (let ((last (first (node-entries node))))
    (if (and last (eq :text (entry-kind last)) (eql (entry-turn last) (node-turn node)))
        (let ((new (copy-entry last)))
          (push text (entry-chunks new))
          (replace-entry node last new))
        (add-entry node :kind :text :chunks (list text)))))

;;; --- nodes ----------------------------------------------------------------

(defun event-key (event)
  "The key of the node EVENT belongs to: nil for the root's, else its :ref."
  (and (member :parent event) (or (getf event :ref) :unknown)))

(defun parent-key (state key)
  "The node whose transcript holds the call a sub-agent with KEY was started
by, or the root."
  (let ((id (and (consp key) (cdr key))))
    (loop for node in (state-nodes state)
          when (and (not (equal key (node-key node))) (find-call node id))
            return (node-key node))))

(defun start-node (state key)
  (make-node :key key :call-id (and (consp key) (cdr key)) :parent (parent-key state key)))

(defun with-node (state event fn)
  "STATE with FN applied to a copy of EVENT's node, made if it is new."
  (let* ((key (event-key event))
         (old (find key (state-nodes state) :key #'node-key :test #'equal))
         (node (copy-node (or old (start-node state key))))
         (next (copy-state state)))
    (a:when-let ((name (getf event :agent)))
      (setf (node-name node) name))
    (funcall fn node)
    (setf (state-nodes next)
          (if old
              (substitute node old (state-nodes state))
              (append (state-nodes state) (list node))))
    next))

;;; --- folding --------------------------------------------------------------

(defun error-reason-p (reason)
  (and (consp reason) (eq :error (first reason))))

(defun fold-event (state event)
  "STATE after EVENT. An event that is not part of the contract, or that
arrives without the ones before it, is folded as far as it makes sense and
never signals."
  (flet ((node-fold (fn) (with-node state event fn)))
    (case (getf event :type)
      (:run-start
       (node-fold
        (lambda (node)
          (setf (node-status node) :running (node-reason node) nil (node-turn node) 0)
          (dolist (message (getf event :messages))
            (add-entry node :kind :message :role (getf message :role)
                            :chunks (list (text-of (getf message :content))))))))
      (:steer
       (node-fold (lambda (node)
                    (add-entry node :kind :steer :chunks (list (text-of (getf event :content)))
                                    :status (and (getf event :interrupt) :interrupt)))))
      (:turn
       (node-fold (lambda (node)
                    (setf (node-status node) :running
                          (node-turn node) (getf event :turn 0)))))
      (:turn-retry
       (node-fold (lambda (node)
                    (notice node :turn-retry :attempt (getf event :attempt)
                                             :reason (getf event :reason)))))
      (:turn-interrupted
       (node-fold (lambda (node)
                    (dolist (entry (node-entries node))
                      (when (and (eq :call (entry-kind entry)) (eq :streaming (entry-status entry)))
                        (let ((new (copy-entry entry)))
                          (setf (entry-status new) :abandoned)
                          (replace-entry node entry new))))
                    (notice node :turn-interrupted))))
      (:text-delta
       (node-fold (lambda (node) (append-text node (text-of (getf event :text))))))
      (:tool-call-delta
       (node-fold (lambda (node)
                    (update-call node (getf event :id) (getf event :name)
                                 (lambda (call)
                                   (a:when-let ((name (getf event :name)))
                                     (setf (entry-name call) name))
                                   (when (and (getf event :id) (null (entry-id call)))
                                     (setf (entry-id call) (getf event :id)))
                                   (let ((fragment (getf event :arguments)))
                                     (when (stringp fragment)
                                       (push fragment (entry-chunks call)))))))))
      (:done
       (node-fold (lambda (node)
                    (when (error-reason-p (getf event :reason))
                      (notice node :turn-failed :reason (getf event :reason))))))
      (:tool-call
       (node-fold (lambda (node)
                    (update-call node (getf event :id) (getf event :name)
                                 (lambda (call)
                                   (setf (entry-name call) (getf event :name)
                                         (entry-id call) (getf event :id)
                                         (entry-arguments call) (getf event :arguments)
                                         (entry-status call) :running))))))
      ((:tool-detached :tool-resumed)
       (let ((status (if (eq :tool-detached (getf event :type)) :detached :running)))
         (node-fold (lambda (node)
                      (update-call node (getf event :id) (getf event :name)
                                   (lambda (call) (setf (entry-status call) status)))))))
      (:tool-result
       (node-fold (lambda (node)
                    (update-call node (getf event :id) nil
                                 (lambda (call)
                                   (setf (entry-result call) (getf event :result)
                                         (entry-status call) :done))))))
      (:context-trimmed
       (node-fold (lambda (node)
                    (notice node :context-trimmed
                            :omitted (getf event :omitted) :truncated (getf event :truncated)
                            :over-budget (getf event :over-budget)))))
      (:run-done
       (node-fold (lambda (node)
                    (setf (node-status node) :done (node-reason node) (getf event :reason)))))
      (t state))))

(defun fold-events (events &optional (state (make-state)))
  (reduce #'fold-event events :initial-value state))
