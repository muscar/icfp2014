(include "stdlib.lisp")

;; AI

(defstruct world-state
  map
  player-status
  ghosts-status
  fruits-status)

(defstruct ai-state
  choices
  path)

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
(defconstant fruit 4)
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
  (cond ((= cell player-start-pos) 0)
	((= cell empty) -1)
	((or (= cell ghost-start-pos) (= cell wall)) -2)
	(t cell)))

(defun get-cell-score (cell location)
  (cell-score cell))

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
  (while (and (not (null ghosts-status))
	      (= vitality 0))
    (set! ghost-status (car ghosts-status))
    ;; (dbug (ghost-status.location ghost-status))
    (when (eql (ghost-status.location ghost-status) location)
      (set! vitality (+ 1 (ghost-status.vitality ghost-status))))
    (set! ghosts-status (cdr ghosts-status)))
  vitality)

(defun get-ghost-score-adjustment (vitality distance)
  (local score)
  (cond ((= vitality frightened) (set! score 3))
	((or (= vitality standard)
	     (= vitality invisible)) (set! score -3)))
  (if (< distance 4)
       (set! score (* score 2)))
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

;; (defun choose-next-direction (map choices location direction)
;;   (local (possible-moves (list right left up down))
;; 	 (direction-scores (get-direction-scores map location ghosts-status)))

;;   (local (best-move (foldl (lambda (current-direction candidate-direction)
;; 			     (local (current-score (cdr (assoc current-direction direction-scores)))
;; 				    (candidate-score (cdr (assoc candidate-direction direction-scores))))

;; 			     (when (= current-direction (opposite-direction direction))
;; 			       (set! current-score (- current-score 1)))
;; 			     (when (= candidate-direction (opposite-direction direction))
;; 			       (set! candidate-score (- candidate-score 1)))
;; 			     (when (not (null (find-if (lambda (choice)
;; 			     				 (if (and (= (location-x location) (location-x location))
;; 			     					  (= (location-y (choice.location choice)) (location-y location)))
;; 			     				     (= (choice.direction choice) candidate-direction)
;; 			     				     0))
;; 			     			       choices)))
;; 			       (set! candidate-score (- candidate-score 1)))
;; 			     (if (> candidate-score current-score)
;; 				 candidate-direction
;; 				 current-direction))
;; 			   direction
;; 			   possible-moves)))
  
;;   ;; (dbug best-move)
;;   best-move)

(defstruct path-node
  cost
  path
  location
  direction)

(defun manhattan (l1 l2)
  (+ (abs (- (location-x l2) (location-x l1)))
     (abs (- (location-y l2) (location-y l1)))))

(defun neighbours (map location ghosts-status dist)
  (local (directions (list up right down left))
	 nl
	 gv
	 ga
	 acc cell)
  (while (not (null directions))
    (set! nl (location-for-direction location (car directions)))
    (set! cell (map-at-direction map location (car directions)))
    (set! cell (get-cell-score cell nl))
    (set! gv (get-ghost-vitality-at-cell ghosts-status location))
    (set! ga (get-ghost-score-adjustment gv dist))
    (set! acc (cons
	       (list cell (car directions) nl)
	       acc))
    (set! directions (cdr directions)))
  acc)

(defun path (map l1 l2 ghosts-status)
  (local (queue (make-priority-queue))
	 node
	 key
	 neighbour-locations
	 neighbour-location
	 neighbour-direction
	 neighbour
	 nn
	 closed)

  (set! queue (priority-queue-insert queue (manhattan l1 l2) (make-struct path-node 0 nil l1 0)))
  (while (not (eql (path-node.location (cdr (priority-queue-top queue))) l2))
    (set! node (cdr (priority-queue-top queue)))
    (set! queue (priority-queue-pop queue))
    (set! neighbour-locations (neighbours map (path-node.location node) ghosts-status (+ 1 (path-node.cost node))))
    (set! closed (cons (path-node.location node) closed))
    (while (not (null neighbour-locations))
      (set! nn (car neighbour-locations))
      (set! neighbour-direction (car (cdr nn)))
      (set! neighbour-location (car (cdr (cdr nn))))
      (when (null (find-if (lambda (closed-location)
			     (eql neighbour-location closed-location))
			   closed))
	(set! neighbour (make-struct path-node
				     (+ (- 0 (car nn)) (path-node.cost node))
				     (cons node (path-node.path node))
				     neighbour-location
				     neighbour-direction))
	(set! key (+ (path-node.cost neighbour)
		     (manhattan neighbour-location l2)))
	(set! queue (priority-queue-insert queue key neighbour)))
      (set! neighbour-locations (cdr neighbour-locations))))
  (reverse (cons (path-node.direction (cdr (priority-queue-top queue)))
		 (map (lambda (node)
			(path-node.direction node))
		      (path-node.path (cdr (priority-queue-top queue)))))
	   nil))

(defstruct spans
  left-span right-span up-span down-span)

(defun get-spans (map x y)
  (local (spans (split-at-pos map x y)))
  (list (cadr spans) (caddr spans) (cadddr spans) (caddddr spans)))

(defun ai-step-function (ai-state world-state)
  (local (map (world-state.map world-state))
  	 (status (world-state.player-status world-state))
	 (ghosts-status (world-state.ghosts-status world-state))
  	 (location (player-status.location status))
  	 (direction (player-status.direction status))
  	 ;; (choices (ai-state.choices ai-state))
	 current-path
	 previous-lives
  	 ;; (next-direction (choose-next-direction map nil location direction))
	 next-direction
	 (corner-idx 0)
	 )

  (when (not (null ai-state))
    (set! current-path (car (car ai-state)))
    (set! previous-lives (cdr (car ai-state)))
    (set! corner-idx (cdr ai-state)))

  (when (> corner-idx 3)
    (set! corner-idx 0))

  (when (< (player-status.lives status) previous-lives)
    (set! current-path nil))

  (local x y
	 (map-width (- (length (car map)) 2))
	 (map-height (- (length map) 2))
	 (spans (get-spans map 1 1))
	 corner
	 search-location
	 span)

  (local (corners (list (cons 1 1)
			(cons map-width 1)
			(cons 1 map-height)
			(cons map-width map-height))))

  (when (null current-path)
    (set! corner (nth corner-idx corners))
    (set! corner-idx (+ corner-idx 1))
    ;; (dbug corner)
    (set! x (car corner))
    (set! y (cdr corner))
    (while (null search-location)
      (set! span (find-if (lambda (span)
			    (find-if (lambda (cell)
				       (or (= cell pill)
					   (= cell power-pill)
					   (= cell fruit)))
				     span))
			  spans))
      (when (not (null span))
	(set! search-location (cons x y))))
    ;; (dbug (cons location search-location))
    (set! current-path (path map location search-location ghosts-status)))
    ;; (dbug current-path))
  (set! next-direction (car current-path))
  (cons (cons (cons (cdr current-path) (player-status.lives status)) corner-idx) next-direction)

  ;; (if (not (null path))
  ;;     (begin (set! next-direction (car path))
  ;; 	     (set! (struct-field ai-state path ai-state) (cdr path)))
  ;;     (if (can-move map location direction)
  ;; 	  (set! next-direction direction)
  ;; 	  (begin (set! next-direction (choose-next-direction map choices location direction))
  ;; 		 (when (not (= next-direction direction))
  ;; 		   (set! (struct-field ai-state choices ai-state) (cons (make-struct choice
  ;; 										     location
  ;; 										     next-direction)
  ;; 									choices)))))
  ;;     )
  ;; (cons ai-state next-direction)
  )

(defun main (initial-state undocumented)
  (cons 0 ai-step-function))
