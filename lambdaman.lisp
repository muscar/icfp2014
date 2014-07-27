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

(defconstant up 0)
(defconstant right 1)
(defconstant down 2)
(defconstant left 3)

(defconstant wall 0)
(defconstant empty 1)
(defconstant pill 2)
(defconstant power-pill 3)
(defconstant fruit-location 4)
(defconstant player-start-pos 5)
(defconstant ghost-start-pos 6)

(defconstant nil 0)

(defun null (thing)
  (and (atom thing) (= thing 0)))

(defun nth (n list)
  (if (<= n 0)
      (car list)
      (nth (- n 1) (cdr list))))

(defun reverse (list acc)
  (if (null list)
      acc
      (reverse (cdr list) (cons (car list) acc))))

(defun find-if (pred list)
  (if (null list)
      nil
      (if (pred (car list))
	  (car list)
	  (find-if pred (cdr list)))))

(defun filter (pred list acc)
  (if (null list)
      (reverse acc nil)
      (if (pred (car list))
	  (filter pred (cdr list) (cons (car list) acc))
	  (filter pred (cdr list) acc))))

(defun foldl (f acc list)
  (if (null list)
      acc
      (foldl f (f acc (car list)) (cdr list))))

(defun map-at (map x y)
  (nth x (nth y map)))

(defun location-for-direction (location direction)
  (local (x (car location))
	 (y (cdr location)))
  (cond ((= direction up) (cons x (- y 1)))
	((= direction down) (cons x (+ y 1)))
	((= direction left) (cons (- x 1) y))
	((= direction right) (cons (+ x 1) y))))

(defun map-at-direction (map location direction)
  (local (new-location (location-for-direction location direction)))
  (map-at map (car new-location) (cdr new-location)))

(defun can-move (map location direction)
  ;; (dbug map)
  (dbug location)
  ;; (dbug direction)
  (> (map-at-direction map location direction) wall))

(defun best-move (map location ai-state possible-moves)
  (foldl (lambda (current-direction candidate-direction)
	   (if (> (map-at-direction map location current-direction)
		  (map-at-direction map location candidate-direction))
	       current-direction
	       candidate-direction))
	 ai-state
	 possible-moves))

(defun foo (x)
  (dbug x))

(defun ai-step-function (ai-state world-state)
  (local (map (world-state.map world-state))
	 (status (world-state.player-status world-state))
	 (location (player-status.location status)))

  ;; (dbug 1)
  ;; (foo location)
  ;; (dbug 2)

  (local (possible-moves (filter (lambda (direction)
				   (dbug location)
  				   (can-move map location direction))
  				 (list right left up down)
  				 nil)))
  (dbug 3)
  (local (moves-iter possible-moves))
  (dbug 4)
  (dbug moves-iter)
  ;; (while (not (null moves-iter))
  ;;   (set! moves-iter (cdr moves-iter)))
  (cons (car possible-moves) (car possible-moves)))

(defun main (initial-state undocumented)
  (cons 0 ai-step-function))

