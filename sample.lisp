(defstruct point
  x y z)

(defun test-make-struct ()
  (local p)
  (set! p (make-struct point 10 20 30))
  (test-struct-field p))

(defun test-struct-field (p)
  (struct-field point z p))

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

(defun fold (f xs)
  (if (atom xs)
      xs
      (f (car xs) (fold f (cdr xs)))))

;; (defun log ()
;;   (dbug 42))

;; (defun test-closure (f)
;;   (f)
;;   (f))

;; (defun test-lambda ()
;;   (local (f (lambda (x) (+ x 1))))
;;   (dbug (f 10)))

(defun main (state)
  (dbug (fold (lambda (x y)
  		(+ x y))
  	      (list 1 2 3)))
  ;; (min (cons 10 20))
  ;; (locals 2 3)
  ;; (test-loop 12)
  ;; (test-make-struct)
  ;; (test-rec 3)
  ;; (test-lambda)
  )
