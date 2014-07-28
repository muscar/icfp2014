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

(defconstant standard 1)
(defconstant frightened 2)
(defconstant invisible 3)

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

(defun offsets-for-direction (direction)
  (cond ((= direction up) (cons 0 -1))
	((= direction down) (cons 0 1))
	((= direction left) (cons -1 0))
	((= direction right) (cons 1 0))))

(defun move-to-direction (location offsets)
  (cons (+ (car location) (car offsets))
	(+ (cdr location) (cdr offsets))))

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

(defun get-ghost-vitality-at-cell (ghosts-status location)
  (local vitality ghost-status)
  (dbug 1001)
  (dbug location)
  (while (and (not (null ghosts-status))
	      (= vitality 0))
    (set! ghost-status (car ghosts-status))
    (dbug ghost-status)
    (dbug (ghost-status.location ghost-status))
    (when (eql (ghost-status.location ghost-status) location)
      (set! vitality (+ 1 (ghost-status.vitality ghost-status))))
    (dbug vitality)
    (set! ghosts-status (cdr ghosts-status)))
  vitality)

(defun get-ghost-score-adjustment (vitality distance)
  (local score)
  (cond ((= vitality frightened) (set! score 3))
	((or (= vitality standard)
	     (= vitality invisible)) (set! score -3)))
  (if (< distance 4)
      (begin
       (dbug 1234)
       (set! score (* score 2))
       (dbug 1235)))
  score)

(defun get-span-score (span starting-location direction-offsets ghosts-status)
  (local score cells vitality)

  (while (and (not (null span))
	      (not (= (car span) wall)))
    (incf cells)
    (set! starting-location (move-to-direction starting-location direction-offsets))
    (set! vitality (get-ghost-vitality-at-cell ghosts-status starting-location))

    (set! score (+ score (cell-score (car span)) (get-ghost-score-adjustment vitality cells)))

    (set! span (cdr span)))

  (when (= cells 0)
    (set! score -1000))

  score)

(defun get-direction-scores (map location ghosts-status)
  (local (spans (split-at-pos map (car location) (cdr location)))
	 left-span right-span up-span down-span)
  (set! left-span (cadr spans))
  (set! right-span (caddr spans))
  (set! up-span (cadddr spans))
  (set! down-span (caddddr spans))

  (list (cons left (get-span-score left-span location (offsets-for-direction left) ghosts-status))
	(cons right (get-span-score right-span location (offsets-for-direction right) ghosts-status))
	(cons up (get-span-score up-span location (offsets-for-direction up) ghosts-status))
	(cons down (get-span-score down-span location (offsets-for-direction down) ghosts-status))))

(defun choose-next-direction (map choices location direction ghosts-status)
  ;; (local (possible-moves (filter (lambda (direction)
  ;; 				   (can-move map location direction))
  ;; 				 (list right left up down)
  ;; 				 nil))

  (local (possible-moves (list right left up down))
	 (direction-scores (get-direction-scores map location ghosts-status)))

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
			       (dbug (cons -10 candidate-direction))
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
     (abs (- (location-y l2) (location-x l1)))))

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
	 neighbour)

  (set! queue (priority-queue-insert queue (manhattan l1 l2) (make-struct path-node 0 nil l1)))
  (dbug (priority-queue-top queue))
  (dbug (path-node.location (cdr (priority-queue-top queue))))
  (while (not (eql (path-node.location (cdr (priority-queue-top queue))) l2))
    (dbug 1337)
    (set! node (cdr (priority-queue-top queue)))
    (set! queue (priority-queue-pop queue))
    (dbug node)
    (set! neighbour-locations (neighbours map (path-node.location node)))
    (dbug neighbour-locations)
    (while (not (null neighbour-locations))
      (dbug 1001001)
      (set! neighbour-location (car neighbour-locations))
      (dbug neighbour-location)
      (set! neighbour (make-struct path-node
				   (+ 1 (path-node.cost node))
				   (cons node (path-node.path node))
				   neighbour-location))
      (dbug neighbour)
      (when (null (find-if (lambda (n)
			     (eql (path-node.location n) neighbour-location))
			   (path-node.path node)))
	(dbug 2002002)
	(set! key (+ (path-node.cost neighbour)
		     (manhattan neighbour-location l2)))
	(set! queue (priority-queue-insert queue key neighbour)))
      (set! neighbour-locations (cdr neighbour-locations)))
    (dbug -1337)
    (dbug queue))
  (reverse (path-node.path (cdr (priority-queue-top queue))) nil))

(defun ai-step-function (ai-state world-state)
  (local (map (world-state.map world-state))
  	 (status (world-state.player-status world-state))
	 (ghosts-status (world-state.ghosts-status world-state))
  	 (location (player-status.location status))
  	 (direction (player-status.direction status))
  	 (choices ai-state)
  	 next-direction)

  ;; (dbug choices)

  (if (can-move map location direction)
      (cons choices direction)
      (begin (set! next-direction (choose-next-direction map choices location direction ghosts-status))
  	     (when (not (= next-direction direction))
  	       (set! choices (cons (make-struct choice
						location
						next-direction)
				   choices)))
  	     (cons choices next-direction)))
  ;; (dbug (path map location (cons 0 0)))
  )

(defun main (initial-state undocumented)
  (cons 0 ai-step-function))
