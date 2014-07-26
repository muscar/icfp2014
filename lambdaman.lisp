(defstruct world-state
  map
  player-status
  ghosts-status
  fruits-status)

(defstruct player-status
  vitality
  location
  direction
  lives
  score)

(defstruct ghost-status
  vitality
  location
  direction)

(defun nth (n list)
  (dbug n)
  (if (<= n 0)
      (car list)
      (nth (- n 1) (cdr list))))

(defun map-at (map x y)
  (nth x (nth y map)))

(defun ai-step-function (ai-state world-state)
  (local (up 0)
	 (right 1)
	 (down 2)
	 (left 3))
  (local (map (world-state.map world-state))
	 (status (world-state.player-status world-state))
	 (location (player-status.location status)))

  (local (x (car location))
	 (y (cdr location))
	 (row (nth y map)))

  (dbug row)

  (cons ai-state right))

(defun main (initial-state undocumented)
  (cons 0 ai-step-function))
