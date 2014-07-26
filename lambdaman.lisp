(defun main ()
  (cons 0 ai-step-function))

(defstruct world-state
  map
  lambdaman-status
  ghosts-status
  fruits-status)

(defstruct lambdaman-status
  vitality
  location
  direction
  lives
  score)

(defun ai-step-function (ai-state current-world-state)
  (local (status (world-state.lambdaman-status current-world-state)))
  (dbug (lambdaman-status.lives status))
  (cons 0 1))
