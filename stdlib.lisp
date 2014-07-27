;; Standard library

(defconstant nil 0)
(defconstant t 1)

(defun eql (x y)
  (if (atom x)
      (if (atom y)
	  (= x y)
	  nil)
      (if (atom y)
	  nil
	  (and (eql (car x) (car y))
	       (eql (cdr x) (cdr y))))))

(defun null (thing)
  (and (atom thing) (= thing 0)))

(defun caar (xs)
  (car (car xs)))

(defun cadr (xs)
  (car (cdr xs)))

(defun caddr (xs)
  (car (cdr (cdr xs))))

(defun assoc (key list)
  (cond
    ((null list) nil)
    ((= key (caar list)) (car list))
    (t (assoc key (cdr list)))))

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

(defun map (f list)
  (if (null list)
      nil
      (cons (f (car list)) (map f (cdr list)))))

(defun foldl (f acc list)
  (if (null list)
      acc
      (foldl f (f acc (car list)) (cdr list))))

(defun transpose (m)
  (if (null (car m))
      nil
      (cons (map (lambda (x) (car x)) m)
            (transpose (map (lambda (x) (cdr x)) m)))))

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
