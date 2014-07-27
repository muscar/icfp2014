;; Standard library

(defconstant nil 0)
(defconstant t 1)

(defun null (thing)
  (and (atom thing) (= thing 0)))

(defun length (list)
  (if (null list)
      0
      (+ 1 (length (cdr list)))))

(defun nth (n list)
  (if (<= n 0)
      (car list)
      (nth (- n 1) (cdr list))))

(defun append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

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
  (cond ((null list) (reverse acc nil))
	((pred (car list)) (filter pred (cdr list) (cons (car list) acc)))
	(t (filter pred (cdr list) acc))))

(defun foldl (f acc list)
  (if (null list)
      acc
      (foldl f (f acc (car list)) (cdr list))))

(defun sort (list)
  (local pivot lefts rights)
  (if (< (length list) 2)
      list
      (begin (set! pivot (car list))
	     (set! list (cdr list))
	     (set! lefts (filter (lambda (x)
				   (<= x pivot))
				 list
				 nil))
	     (set! rights (filter (lambda (x)
				    (> x pivot))
				  list
				  nil))
	     (append (sort lefts) (cons pivot (sort rights))))))

;; AI

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

(defstruct choice
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

(defun location-x (location)
  (car location))

(defun location-y (location)
  (cdr location))

(defun opposite-direction (direction)
  (cond ((= direction up) down)
	((= direction right) left)
	((= direction down) up)
	((= direction left) right)))

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
  (> (map-at-direction map location direction) wall))

(defun best-move (map location ai-state possible-moves)
  (foldl (lambda (current-direction candidate-direction)
	   (if (> (map-at-direction map location current-direction)
		  (map-at-direction map location candidate-direction))
	       current-direction
	       candidate-direction))
	 ai-state
	 possible-moves))

(defun cell-score (cell)
  (cond ((or (= cell player-start-pos) (= cell empty)) 0)
	((or (= cell ghost-start-pos) (= cell wall)) -1)
	(t cell)))

(defun direction-score (map location direction)
  (local (current-location location)
	 cell
	 result)
  (while (can-move map current-location direction)
    (set! cell (map-at-direction map current-location direction))
    (set! result (+ result (cell-score cell)))
    (set! current-location (location-for-direction current-location direction)))
  result)

(defun choose-next-direction (map choices location direction)
  (local (possible-moves (filter (lambda (direction)
				   (can-move map location direction))
				 (list right left up down)
				 nil)))
  ;; (dbug possible-moves)
  
  (local (best-move (foldl (lambda (current-direction candidate-direction)
			     (local (current-score (direction-score map location current-direction))
				    (candidate-score (direction-score map location candidate-direction)))
			     ;; (dbug 11111111)
			     (when (= current-direction (opposite-direction direction))
			       ;; (dbug 18181818)
			       (set! current-score (- current-score 1)))
			     (when (= candidate-direction (opposite-direction direction))
			       ;; (dbug 17171717)
			       (set! candidate-score (- candidate-score 1)))
			     (when (find-if (lambda (choice)
					      (dbug 17171717)
					      (dbug choice)
					      (dbug (choice.location choice))
					      (dbug (location-x (choice.location choice)))
					      (dbug (location-y (choice.location choice)))
					      (dbug (choice.direction choice))
					      (dbug location)
					      (dbug (location-x location))
					      (dbug (location-y location))
					      (dbug (= (location-x (choice.location choice)) (location-x location)))
					      (dbug (= (location-y (choice.location choice)) (location-y location)))
					      (dbug (choice.direction choice))
					      (when (and (= (location-x (choice.location choice)) (location-x location))
					      		 (= (location-y (choice.location choice)) (location-y location)))
					      	(dbug 17171717)
					      	(= (choice.direction choice) candidate-direction))
					      )
					    choices)
			       (dbug 19191919)
			       (set! candidate-score (- candidate-score 1)))
			     ;; (dbug current-direction)
			     ;; (dbug current-score)
			     ;; (dbug 12121212)
			     ;; (dbug candidate-direction)
			     ;; (dbug candidate-score)
			     ;; (dbug 11111111)
			     (if (or (> candidate-score current-score)
				     (not (can-move map location current-direction)))
				 candidate-direction
				 current-direction))
			   direction
			   possible-moves)))
  
  ;; (dbug best-move)
  best-move)

(defun ai-step-function (ai-state world-state)
  (local (map (world-state.map world-state))
  	 (status (world-state.player-status world-state))
  	 (location (player-status.location status))
  	 (direction (player-status.direction status))
  	 (choices ai-state)
  	 next-direction)

  (dbug choices)

  (if (can-move map location direction)
      (cons choices direction)
      (begin (set! next-direction (choose-next-direction map choices location direction))
  	     (when (not (= next-direction direction))
  	       (set! choices (cons (make-struct choice
						location
						next-direction)
				   choices)))
  	     (cons choices next-direction)))

  )

(defun main (initial-state undocumented)
  (cons 0 ai-step-function))
