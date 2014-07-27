(defpackage #:search
  (:use #:cl))

(in-package #:search)

(defparameter *map* (list (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 3 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 3 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 1 (cons 0 (cons 1 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 1 (cons 1 (cons 1 (cons 0 (cons 2 (cons 0 (cons 1 (cons 1 (cons 1 (cons 1 (cons 6 (cons 1 (cons 1 (cons 1 (cons 1 (cons 0 (cons 2 (cons 0 (cons 1 (cons 1 (cons 1 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 1 (cons 0 (cons 0 (cons 0 (cons 1 (cons 0 (cons 0 (cons 0 (cons 1 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 1 (cons 1 (cons 1 (cons 1 (cons 2 (cons 1 (cons 1 (cons 0 (cons 1 (cons 6 (cons 6 (cons 6 (cons 1 (cons 0 (cons 1 (cons 1 (cons 2 (cons 1 (cons 1 (cons 1 (cons 1 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 1 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 1 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 1 (cons 1 (cons 1 (cons 0 (cons 2 (cons 0 (cons 1 (cons 1 (cons 1 (cons 1 (cons 4 (cons 1 (cons 1 (cons 1 (cons 1 (cons 0 (cons 2 (cons 0 (cons 1 (cons 1 (cons 1 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 1 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 1 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 3 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 5 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 3 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 (cons 2 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 2 (cons 0 0)))))))))))))))))))))))
			  (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 0)))))))))))))))))))))))))

(defstruct path-node
  path
  cost
  location)

;; Priority queues

(defun make-priority-queue ()
  (list))

(defun priority-queue-insert (queue key value)
  (cond
    ((null queue) (list (cons key value)))
    ((< key (caar queue)) (cons (cons key value) queue))
    (t (cons (car queue) (priority-queue-insert (cdr queue) key value)))))

(defun priority-queue-top (queue)
  (cond
    ((null queue) nil)
    (t (car queue))))

(defun priority-queue-pop (queue)
  (cond
    ((null queue) nil)
    (t (cdr queue))))

(defun priority-queue-search (queue key)
  (assoc key queue))

;; Search

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

(defun map-at (map x y)
  (nth x (nth y map)))

(defun location-for-direction (location direction)
  (let ((x (car location))
	(y (cdr location)))
    (cond ((= direction up) (cons x (- y 1)))
	  ((= direction down) (cons x (+ y 1)))
	  ((= direction left) (cons (- x 1) y))
	  ((= direction right) (cons (+ x 1) y)))))

(defun map-at-direction (map location direction)
  (let ((new-location (location-for-direction location direction)))
    (map-at map (car new-location) (cdr new-location))))

(defun can-move (map location direction)
  (> (map-at-direction map location direction) wall))

(defun manhattan (l1 l2)
  (+ (abs (- (location-x l2) (location-x l1)))
     (abs (- (location-y l2) (location-y l1)))))

(defun neighbours (map location)
  (let ((directions (list up right down left))
	acc)
    (dolist (direction directions)
      (when (can-move map location direction)
	(setf acc (cons (location-for-direction location direction)
			acc))))
    (reverse acc)))

(defun path (map l1 l2)
  (let ((queue (make-priority-queue))
	key
	neighbour-locations
	neighbour
	closed)

    (setf queue (priority-queue-insert queue (manhattan l1 l2) (make-path-node :path nil :cost 0 :location l1)))
    (setf closed '())

    (do ((node #1=(cdr (priority-queue-top queue)) #1#))
	((equal (path-node-location (cdr (priority-queue-top queue))) l2)
	 (mapcar #'path-node-location (reverse (path-node-path (cdr (priority-queue-top queue))))))

      (setf queue (priority-queue-pop queue))
      (setf neighbour-locations (neighbours map (path-node-location node)))
      (push (path-node-location node) closed)

      (dolist (neighbour-location neighbour-locations)
	(unless (position neighbour-location closed)
	  (setf neighbour (make-path-node :path (cons node (path-node-path node))
					  :cost (+ 1 (path-node-cost node))
					  :location neighbour-location))
	    (setf key (+ (path-node-cost neighbour)
			 (manhattan neighbour-location l2)))
	    (setf queue (priority-queue-insert queue key neighbour))))

      (format t "queue: ~a~%" (mapcar (lambda (e)
					(cons (car e) (length (path-node-path (cdr e)))))
				      queue))
      (format t "expanded ~a nodes~%" (length closed)))))

(defun test ()
  (path *map* (cons 11 6) (cons 5 15)))
