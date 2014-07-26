(defstruct point
  x y z)

(defun test-make-struct ()
  (local p)
  (test-struct-field (make-struct point 10 20 30)))

(defun test-struct-field (p)
  (struct-field point z p))

;; (defun min (cell)
;;   (if (< (car cell) (cdr cell))
;;       (car cell)
;;       (cdr cell))
;;   1337)

;; (defun locals ()
;;   (local x y)
;;   (set! x 10)
;;   (set! y 20)
;;   (+ x y))

;; (defun test-loop (n)
;;   (local x)
;;   (while (> n 0)
;;     (set! n (- n 1))
;;     (set! x (+ x 2)))
;;   x)

(defun main (state)
  ;; (min (cons 10 20))
  ;; (test-loop 12)
  (test-make-struct))
