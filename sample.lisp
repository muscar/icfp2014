;; (defstruct point
;;   x y z)

;; (defun test-make-struct ()
;;   (local p)
;;   (set! p (make-struct point 10 20 30))
;;   (test-struct-field p))

;; (defun test-struct-field (p)
;;   (struct-field point z p))

;; (defun min (x y)
;;   (local p)
;;   (set! p
;; 	(if (< x y)
;; 	    x
;; 	    y))
;;   p)

;; (defun locals (x y)
;;   (local (x (+ x 1)) (y (+ y 1)))
;;   ;; (set! x 10)
;;   (set! y 20)
;;   (+ x y))

;; (defun test-loop (n)
;;   (local x)
;;   (while (> n 0)
;;     (set! n (- n 1))
;;     (set! x (+ x 2)))
;;   x)

;; (defun test-rec (n)
;;   (if (= n 0)
;;       (dbug 42)
;;       (test-rec (- n 1))))

;; (defun add (x y)
;;   (+ x y))

;; (defun log ()
;;   (dbug 42))

;; (defun test-closure (f)
;;   (f)
;;   (f))

;; (defun test-lambda ()
;;   (local (f (lambda (x) (+ x 1))))
;;   (dbug (f 10)))


(defun null (l)
  (if (atom l)
      (= l 0)
      0))

;; (defun map (f xs)
;;   (if (null xs)
;;       xs
;;       (cons (f (car xs)) (map f (cdr xs)))))

;; (defun fold (f xs)
;;   (if (atom xs)
;;       xs
;;       (f (car xs) (fold f (cdr xs)))))

;; (defun nth (n list)
;; ;  (dbug n)
;;   (if (or (<= n 0) (atom list))
;;       (car list)
;;       (nth (- n 1) (cdr list))))

;; (defconstant one 1)
;; (defconstant two 2)

;; (defconstant nil 0)

;; (defun null (thing)
;;   (and (atom thing) (= thing 0)))

;; (defun nth (n list)
;;   (if (<= n 0)
;;       (car list)
;;       (nth (- n 1) (cdr list))))

;; (defun find-if (pred list)
;;   (if (null list)
;;       nil
;;       (if (pred (car list))
;; 	  (car list)
;; 	  (find-if pred (cdr list)))))

;; (defun foldl (f acc list)
;;   (if (null list)
;;       acc
;;       (foldl f (f acc (car list)) (cdr list))))

(defconstant nil 0)
(defconstant t 1)

;; (defun null (thing)
;;   (and (atom thing) (= thing 0)))

(defun reverse (list acc)
  (if (null list)
      acc
      (reverse (cdr list) (cons (car list) acc))))

(defun filter (pred list acc)
  (if (null list)
      (reverse acc nil)
      (if (pred (car list))
	  (filter pred (cdr list) (cons (car list) acc))
	  (filter pred (cdr list) acc))))

;; (defun foo ()
;;   1)

;; (defconstant wall 0)
;; (defconstant empty 1)
;; (defconstant pill 2)
;; (defconstant power-pill 3)
;; (defconstant fruit-location 4)
;; (defconstant player-start-pos 5)
;; (defconstant ghost-start-pos 6)

;; (defun cell-score (cell)
;;   (cond ((= cell player-start-pos) 0)
;; 	((= cell ghost-start-pos) -1)
;; 	(t cell)))

;; (defun for-each (f xs)
;;   (if (not (null xs))
;;       (begin
;;        (f (car xs))
;;        (for-each f (cdr xs)))))

(defun append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(defun length (list)
  (if (null list)
      0
      (+ 1 (length (cdr list)))))

(defun sort (list)
  (dbug list)
  (local pivot left right)
  (if (< (length list) 2)
      list
      (begin (set! pivot (car list))
	     (set! list (cdr list))
	     (dbug pivot)
	     (dbug list)
	     (set! left (filter (lambda (x)
				  (<= x pivot))
				list
				nil))
	     (dbug left)
	     (dbug 222222)
	     (set! right (filter (lambda (x)
				   (> x pivot))
				 list
				 nil))
	     (dbug right)
	     (dbug 222222)
	     (append (sort left) (cons pivot (sort right))))))

(defun main ()
  ;; (dbug (fold (lambda (x y)
  ;; 		(+ x y))
  ;; 	      (list 1 2 3)))
  ;; (min (cons 10 20))
  ;; (locals 2 3)
  ;; (test-loop 12)
  ;; (test-make-struct)
  ;; (test-rec 3)
  ;; (test-lambda)
  ;; (local (t 40))
  ;; (dbug t)
  ;; (map (lambda (x) (+ x t)) (list 1 2 3))
  ;; (nth 2 (list 1 2 3 4 5))
  ;; (if (= 1 1)
  ;;      (dbug 1)
  ;;     (if (+ 1 1)
  ;; 	  (dbug 3)
  ;; 	  (dbug 4)))
  ;; (cond
  ;;   ((= 1 2) (dbug 1) (dbug 2))
  ;;   (1 (dbug 3)))
  ;; (dbug one)
  ;; (dbug two)
  ;; (find-if (lambda (x) (= x 7)) (list 1 2 3 4 5))
  ;; (local (y 42))
  ;; (local (f (lambda (x)
  ;; 	      (dbug y)
  ;; 	      (+ 1 1))))
  ;; (local (xs (filter f (list 1 2 3) 0)))
  ;; (dbug xs)
  ;; (dbug (foo))
  ;; (while (not (null x))
  ;;   (dbug x)
  ;;   (set! x (cdr x)))
  ;; (local (x (foldl (lambda (x y) (+ x y)) 0 (list 1 2 3))))
  ;; (dbug x)

  ;; (dbug (cell-score wall))
  ;; (dbug (cell-score empty))
  ;; (dbug (cell-score pill))
  ;; (dbug (cell-score power-pill))
  ;; (dbug (cell-score fruit-location))
  ;; (dbug (cell-score player-start-pos))
  ;; (dbug (cell-score ghost-start-pos))
  ;; (local (cell-types (list wall empty pill power-pill fruit-location player-start-pos ghost-start-pos)))
  ;; (for-each (lambda (cell)
  ;; 	      (dbug (cell-score cell)))
  ;; 	    cell-types)
  ;; (dbug (map cell-score cell-types))

  ;; (when (= 1 1)
  ;;   (dbug 0)
  ;;   (dbug 1))

  ;; (when (= 1 2)
  ;;   (dbug 0)
  ;;   (dbug 2))

  ;; (unless (= 1 1)
  ;;   (dbug 0)
  ;;   (dbug 3))

  ;; (unless (= 1 2)
  ;;   (dbug 0)
  ;;   (dbug 4))

  ;; (local (x 3))
  ;; (incf x)
  ;; (dbug x)
  ;; (decf x)
  ;; (dbug x)
  (dbug (sort (list 5 1 4 2 3)))
  ;; (while (not (null cell-types))
  ;;   (dbug (cell-score (car cell-types)))
  ;;   (set! cell-types (cdr cell-types)))
  )
