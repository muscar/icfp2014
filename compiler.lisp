(defpackage :icfp
  (:use :cl))

(in-package :icfp)

(defparameter *gcc-program* '())

;; GCC assembly

(defun mark-label-p (instr)
  (eq (car instr) '$MARK-LABEL))

(defun fresh-label (prefix)
  (gensym prefix))

(defun compile-gcc (program)
  (multiple-value-bind (program label-addrs) (collect-labels program)
    (write-gcc (patch-program program label-addrs))))

(defun collect-labels (program)
  (let ((idx 0)
	(new-program '())
	(label-addrs '()))
    (dolist (instr program (values (nreverse new-program)
				   (nreverse label-addrs)))
      (if (mark-label-p instr)
	  (push (cons (cadr instr) idx) label-addrs)
	  (progn
	    (incf idx)
	    (push instr new-program))))))

(defun patch-program (program label-addrs)
  (mapcar (lambda (instr)
	    (case (first instr)
	      (ldf `(ldf ,(cdr (assoc (cadr instr) label-addrs))))
	      ((sel tsel) `(,(first instr)
			     ,(cdr (assoc (cadr instr) label-addrs))
			     ,(cdr (assoc (caddr instr) label-addrs))))
	      (t instr)))
	  program))

(defun write-gcc (program)
  (loop for instr in program
     for idx from 0
     do (format t "~{~a ~}~10,25@t; ~a~%" instr idx)))

;; Lang 0

(defparameter *lang0-program* '((defun main ()
				  ;; (min (cons 10 20)))
				  (test-loop 12))
				  ;; (test-rec 7)
				;; (defun min (cell)
				;;   (if (> (car cell) (cdr cell))
				;;       (cdr cell)
				;;       (car cell)))
				;; (defun locals ()
				;;   (local x y)
				;;   (set! x 10)
				;;   (set! y 20)
				;;   (+ x y))
				(defun test-loop (n)
				  (local x)
				  (while (> n 0)
				    (dbug n)
				    (set! n (- n 1))
				    (set! x (+ x 2)))
				  x)))
				;; (defun test-rec (n)
				;;   (if (> n 0)
				;;       (begin
				;;        (dbug n)
				;;        (test-rec (- n 1)))))))

(defparameter *functions* '())
(defparameter *l0-current-function* nil)
(defparameter *l0-delayed-code* nil)

(defstruct l0-function
  name
  args
  locals
  body)

(defun collect-functions (program)
  (dolist (fun program)
    (destructuring-bind (name args &rest body) (cdr fun)
      (assert (eq (car fun) 'defun) () "expecting function definition")
      (push (cons name (make-l0-function :name name
					 :args args
					 :locals (collect-locals body)
					 :body body)) *functions*)))
  (setf *functions* (nreverse *functions*)))

(defun collect-locals (body)
  (loop for instr in body
     when (and (consp instr) (eq (first instr) 'local))
     nconc (rest instr)))

(defun compile-lang0 (program)
  (let ((*gcc-program* '())
	(*functions* '()))
    (collect-functions program)
    (dolist (*l0-current-function* (mapcar #'cdr *functions*) (compile-gcc (nreverse *gcc-program*)))
      (let ((*l0-delayed-code* '()))
	(push `($mark-label ,(l0-function-name *l0-current-function*)) *gcc-program*)
	(compile-l0-begin (l0-function-body *l0-current-function*))
	(push '(RTN) *gcc-program*)
	(dolist (codegen *l0-delayed-code*)
	  (funcall codegen))))))

(defun compile-lang0-instruction (instr)
  (cond ((consp instr) (compile-lang0-call instr))
	((numberp instr) (push `(LDC ,instr) *gcc-program*))
	((symbolp instr) (push `(LD 0 ,(symbol-index instr)) *gcc-program*))
	(t (error "unknown instruction ~a" instr))))

(defun compile-lang0-call (instr)
  (assert (symbolp (first instr)) () "call operator is not a symbol ~a" (first instr))
  (unless (compile-lang0-prim instr)
    (let ((fun (assoc (first instr) *functions*)))
      (unless fun
	(error "unknown function ~a" (first instr)))
      (let ((fun-arity (length (l0-function-args (cdr fun)))))
	(unless (= (length (rest instr)) fun-arity)
	  (error "wrong arity; expecting ~a args but got ~a"
		 fun-arity
		 (length (rest instr))))
	(dolist (arg (rest instr))
	  (compile-lang0-instruction arg))
	(dotimes (idx (length (l0-function-locals (cdr fun))))
	  (push `(LDC 0) *gcc-program*))
	(push `(LDF ,(l0-function-name (cdr fun))) *gcc-program*)
	(push `(AP ,(+ fun-arity (length (l0-function-locals (cdr fun))))) *gcc-program*)))))

(defun symbol-index (sym)
  (or (position sym (l0-function-args *l0-current-function*))
      (+ (length (l0-function-args *l0-current-function*))
	 (position sym (l0-function-locals *l0-current-function*)))))

(defun compile-lang0-prim (instr)
  (destructuring-bind (op &rest args) instr
    (case op
      (local t)
      (set! (compile-lang0-instruction (second args))
	    (push `(st 0 ,(symbol-index (first args))) *gcc-program*)
	    t)
      ((atom car cdr dbug) (compile-lang0-instruction (first args))
                           (push `(,op) *gcc-program*)
                           t)
      (if (compile-l0-if (first args) (second args) (third args)))
      (while (compile-l0-while (first args) (rest args)))
      (begin (compile-l0-begin args))
      ((+ - * / = > >= cons) (compile-l0-binop op (first args) (second args)))
      (< (compile-l0-binop '> (second args) (first args)))
      (<= (compile-l0-binop '>= (second args) (first args))))))

(defun compile-l0-goto (label)
  (push '(ldc 1) *gcc-program*)
  (push `(tsel ,label ,label) *gcc-program*))

(defun compile-l0-binop (op first-arg second-arg)
  (let* ((op-map '((+ . add)
		   (- . sub)
		   (* . mul)
		   (/ . div)
		   (= . ceq)
		   (> . cgt)
		   (>= . cgte)
		   (cons . cons)))
	 (opcode (assoc op op-map)))
    (compile-lang0-instruction first-arg)
    (compile-lang0-instruction second-arg)
    (push (list (cdr opcode)) *gcc-program*)
    t))

(defun compile-l0-if (condition then-body else-body)
  (let ((then-label (fresh-label "then"))
	(else-label (fresh-label "else")))
    (compile-lang0-instruction condition)
    (push `(sel ,then-label ,else-label) *gcc-program*)

    (push (lambda ()
	    (push `($mark-label ,then-label) *gcc-program*)
	    (compile-lang0-instruction then-body)
	    (push '(join) *gcc-program*))
	  *l0-delayed-code*)

    (push (lambda ()
	    (push `($mark-label ,else-label) *gcc-program*)
	    (when else-body
	      (compile-lang0-instruction else-body))
	    (push '(join) *gcc-program*))
	  *l0-delayed-code*)
    t))

(defun compile-l0-while (condition body)
  (let ((head-label (fresh-label "while-head-label"))
	(body-label (fresh-label "while-body-label"))
	(end-label (fresh-label "while-end-label")))
    (push `($mark-label ,head-label) *gcc-program*)
    (compile-lang0-instruction condition)
    (push `(tsel ,body-label ,end-label) *gcc-program*)
    (push `($mark-label ,body-label) *gcc-program*)
    (dolist (instr body)
      (compile-lang0-instruction instr))
    (compile-l0-goto head-label)
    (push `($mark-label ,end-label) *gcc-program*))
  t)

(defun compile-l0-begin (body)
  (dolist (instr body)
    (compile-lang0-instruction instr))
  t)
