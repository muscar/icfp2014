(include "stdlib.lisp")

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

(defun split-at (x list)
  (local left-part)
  (while (> x 0)
    (set! left-part (cons (car list) left-part))
    (set! list (cdr list))
    (decf x))
  (list (car list) left-part (cdr list)))

(defun split-line-at (m x y)
  (split-at x (nth y m)))

(defun split-column-at (m x y)
  (split-at y (map (lambda (line) (nth x line)) m)))

(defun split-at-pos (m x y)
  (local (h-split (split-line-at m x y))
	 (v-split (split-column-at m x y)))
  (list (car h-split) (cadr h-split) (caddr h-split) (cadr v-split) (caddr v-split)))

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

(defun get-span-score (span)
  (local score cells)

  (while (and (not (null span))
	      (not (= (car span) wall)))
    (set! score (+ score (cell-score (car span))))
    (set! span (cdr span))
    (incf cells))

  (when (= cells 0)
    (set! score -1000))

  score)

(defun get-direction-scores (map location)
  (local (spans (split-at-pos map (car location) (cdr location)))
	 left-span right-span up-span down-span)
  (set! left-span (cadr spans))
  (set! right-span (caddr spans))
  (set! up-span (cadddr spans))
  (set! down-span (caddddr spans))

  (list (cons left (get-span-score left-span))
	(cons right (get-span-score right-span))
	(cons up (get-span-score up-span))
	(cons down (get-span-score down-span))))

(defun choose-next-direction (map choices location direction)
  ;; (local (possible-moves (filter (lambda (direction)
  ;; 				   (can-move map location direction))
  ;; 				 (list right left up down)
  ;; 				 nil))

  (local (possible-moves (list right left up down))
	 (direction-scores (get-direction-scores map location)))

  (local (best-move (foldl (lambda (current-direction candidate-direction)
			     ;; (local (current-score (direction-score map location current-direction))
			     ;; 	    (candidate-score (direction-score map location candidate-direction)))
			     (local (current-score (cdr (assoc current-direction direction-scores)))
				    (candidate-score (cdr (assoc candidate-direction direction-scores))))

			     (when (= current-direction (opposite-direction direction))
			       ;; (dbug 18181818)
			       (set! current-score (- current-score 1)))
			     (when (= candidate-direction (opposite-direction direction))
			       ;; (dbug 17171717)
			       (set! candidate-score (- candidate-score 1)))
			     (when (not (null (find-if (lambda (choice)
			     				 (if (and (= (location-x location) (location-x location))
			     					  (= (location-y (choice.location choice)) (location-y location)))
			     				     (= (choice.direction choice) candidate-direction)
			     				     0))
			     			       choices)))
			       (set! candidate-score (- candidate-score 1)))
			     (if (> candidate-score current-score)
				 ;; (dbug (cons current-score candidate-score))
				 ;; (if (or (> candidate-score current-score)
				 ;; 	     (not (can-move map location current-direction)))
				 candidate-direction
				 current-direction))
			   direction
			   possible-moves)))
  
  ;; (dbug best-move)
  best-move)

(defstruct path-node
  cost
  path
  location)

(defun manhattan (l1 l2)
  (+ (abs (- (location-x l2) (location-x l1)))
     (abs (- (location-y l2) (location-y l1)))))

(defun neighbours (map location)
  (local (directions (list up right down left))
	 acc)
  (while (not (null directions))
    (when (can-move map location (car directions))
      (set! acc (cons (location-for-direction location (car directions))
		      acc)))
    (set! directions (cdr directions)))
  (reverse acc nil))

(defun path (map l1 l2)
  (local (queue (make-priority-queue))
	 node
	 key
	 neighbour-locations
	 neighbour-location
	 neighbour
	 closed)

  (set! queue (priority-queue-insert queue (manhattan l1 l2) (make-struct path-node 0 nil l1)))
  (while (not (eql (path-node.location (cdr (priority-queue-top queue))) l2))
    (set! node (cdr (priority-queue-top queue)))
    (set! queue (priority-queue-pop queue))
    (set! neighbour-locations (neighbours map (path-node.location node)))
    (set! closed (cons (path-node.location node) closed))
    (while (not (null neighbour-locations))
      (set! neighbour-location (car neighbour-locations))
      (when (null (find-if (lambda (closed-location)
			     (eql neighbour-location closed-location))
			   closed))
	(set! neighbour (make-struct path-node
				     (+ 1 (path-node.cost node))
				     (cons node (path-node.path node))
				     neighbour-location))
	(set! key (+ (path-node.cost neighbour)
		     (manhattan neighbour-location l2)))
	(set! queue (priority-queue-insert queue key neighbour)))
      (set! neighbour-locations (cdr neighbour-locations))))
  (reverse (cons l2
		 (map (lambda (node)
			(path-node.location node))
		      (path-node.path (cdr (priority-queue-top queue)))))
	   nil))

(defun ai-step-function (ai-state world-state)
  (local (map (world-state.map world-state))
  	 (status (world-state.player-status world-state))
  	 (location (player-status.location status))
  	 (direction (player-status.direction status))
  	 (choices ai-state)
  	 next-direction)

  ;; (dbug choices)

  (if (can-move map location direction)
      (cons choices direction)
      (begin (set! next-direction (choose-next-direction map choices location direction))
  	     (when (not (= next-direction direction))
  	       (set! choices (cons (make-struct choice
						location
						next-direction)
				   choices)))
  	     (cons choices next-direction))))

(defun main (initial-state undocumented)
  (cons 0 ai-step-function))
