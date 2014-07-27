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


;; (defun null (l)
;;   (if (atom l)
;;       (= l 0)
;;       0))

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

(defun null (thing)
  (and (atom thing) (= thing 0)))

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

(defun foldl (f acc list)
  (if (null list)
      acc
      (foldl f (f acc (car list)) (cdr list))))

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
  (local (x (foldl (lambda (x y) (+ x y)) 0 (list 1 2 3))))
  (dbug x)
  )
