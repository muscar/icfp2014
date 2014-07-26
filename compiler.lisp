(defpackage :icfp
  (:use :cl))

(in-package :icfp)

(defun partition (pred sequence)
  (let (left right)
    (dolist (e sequence (values (nreverse left) (nreverse right)))
      (if (funcall pred e)
	  (push e left)
	  (push e right)))))

(defparameter *gcc-program* '())

;; GCC assembly

(defun mark-label-p (instr)
  (eq (car instr) '$MARK-LABEL))

(defun fresh-label (prefix)
  (gensym prefix))

(defun compile-gcc (program)
  (multiple-value-bind (program label-addrs) (collect-labels program)
    (write-gcc (patch-program program label-addrs) label-addrs)))

(defun collect-labels (program)
  (let ((idx 0)
	(new-program '())
	(label-addrs '()))
    (dolist (instr program (values (nreverse new-program)
				   (nreverse label-addrs)))
      (cond ((mark-label-p instr) (push (cons (cadr instr) idx) label-addrs))
	    ((remp instr) (push instr new-program))
	    (t (incf idx)
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

(defun rem-instr-p (instr)
  (and (consp instr) (eq (car instr) 'rem)))

(defun ldf-instr-p (instr)
  (and (consp instr) (eq (car instr) 'ldf)))

(defun write-gcc (program label-addrs)
  (loop for instr in program
     with pending-label = nil
     do (cond ((ldf-instr-p instr) (let ((name (car (find-if (lambda (entry)
							       (= (cdr entry) (second instr)))
							     label-addrs))))
				     (format t "~{~a ~}~10,25@t; address of ~a~%" instr name)))
	      ((remp instr) (setf pending-label (rest instr)))
	      (t (if pending-label
		     (progn
		       (format t "~{~a ~}~10,25@t; ~{~a ~}~%" instr pending-label)
		       (setf pending-label nil))
		     (format t "~{~a ~}~%" instr))))))

;; Lang 0

;; BUG Locals don't work in the first frame -- no space

(defparameter *lang0-program* '((defstruct point
				  x y z)
				(rem "The netry point")
				(defun main (state)
				  ;(min (cons 10 20))
				  ;(test-loop 12)
				  (test-make-struct)
				  )
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
				))

(defparameter *functions* '())
(defparameter *structs* '())
(defparameter *l0-current-function* nil)
(defparameter *l0-delayed-code* nil)

(defstruct l0-function
  name
  args
  locals
  frame-size
  body)

(defstruct l0-struct
  name
  fields)

(defun fundefp (form)
  (and (consp form) (eq (first form) 'defun)))

(defun collect-functions (functions)
  (setf *functions* (mapcar #'analyze-function functions)))

(defun analyze-function (fun)
  (assert (fundefp fun) () "expecting function definition")
  (destructuring-bind (name args &rest body) (cdr fun)
    (let ((locals (collect-locals body)))
      (cons name (make-l0-function :name name
				   :args args
				   :locals locals
				   :frame-size (+ (length args) (length locals))
				   :body body)))))

(defun collect-locals (body)
  (loop for instr in body
     when (and (consp instr) (eq (first instr) 'local))
     nconc (rest instr)))

(defun collect-structs (structs)
  (setf *structs* (mapcar #'analyze-struct structs)))

(defun analyze-struct (struct)
  (destructuring-bind (name &rest fields) (cdr struct)
    (cons name (make-l0-struct :name name
			       :fields (collect-fields fields)))))

(defun collect-fields (fields)
  (loop for field in fields
     for idx from 0
     collect (cons field idx)))

(defun field-index (struct name)
  (let ((field (assoc name (l0-struct-fields struct))))
    (unless field
      (error "struct ~a does not have a field ~a" (l0-struct-name struct) name))
    (cdr field)))

(defun compile-lang0 (program)
  (let ((*gcc-program* '())
	(*functions* '())
	(*structs* '()))
    (multiple-value-bind (functions structs) (partition #'fundefp program)
      (collect-functions functions)
      (collect-structs structs)
      ;; Emit entry point
      (let ((main-function (cdr (assoc 'main *functions*))))
	(unless main-function
	  (error "no main function defined"))
	(dotimes (idx (l0-function-frame-size main-function))
	  (lang0-emit 'ldc 0))
	(lang0-emit 'ldf 'main)
	(lang0-emit 'ap (l0-function-frame-size main-function))
	(lang0-emit 'rtn))
      
      (dolist (*l0-current-function* (mapcar #'cdr *functions*) (compile-gcc (nreverse *gcc-program*)))
	(let ((*l0-delayed-code* '()))
	  (lang0-emit 'rem (l0-function-name *l0-current-function*))
	  (push `($mark-label ,(l0-function-name *l0-current-function*)) *gcc-program*)
	  (dolist (instr (l0-function-body *l0-current-function*))
	    (compile-lang0-instruction instr))
	  (push '(RTN) *gcc-program*)
	  (dolist (codegen *l0-delayed-code*)
	    (funcall codegen)))))))

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

;; Lang0 primitives

(defun lang0-emit (opcode &rest operands)
  (push (cons opcode operands) *gcc-program*))

(defun lang0-prim-make-struct (name values)
  (let ((struct (cdr (assoc name *structs*))))
    (unless struct
      (error "unknown structure ~a" name))
    (unless (= (length (l0-struct-fields struct)) (length values))
      (error "trying to construct ~a struct wrong number of values; expecting ~a, got ~a"
	     name
	     (length (l0-struct-fields struct))
	     (length values)))
    (dolist (value values)
      (compile-lang0-instruction value))
    ;; Emit NIL
    (lang0-emit 'ldc 0)
    (dotimes (idx (length values))
      (lang0-emit 'cons))))

(defun lang0-prim-struct-field (name field-name instance)
  (let ((struct (cdr (assoc name *structs*))))
    (unless struct
      (error "unknown structure ~a" name))
    (let ((field-index (cdr (assoc field-name (l0-struct-fields struct)))))
      (unless field-index
	(error "unknown field ~a" field-name))
      (compile-lang0-instruction instance)
      (dotimes (idx field-index)
	(lang0-emit 'cdr))
      (lang0-emit 'car))))

(defun compile-lang0-prim (instr)
  (destructuring-bind (op &rest args) instr
    (case op
      (local t)
      (rem (lang0-emit 'rem (first args)))
      (dbug (push '(dbug) *gcc-program*)
	    t)
      (set! (compile-lang0-instruction (second args))
	    (push `(st 0 ,(symbol-index (first args))) *gcc-program*)
	    t)
      ((atom car cdr) (compile-lang0-instruction (first args))
                      (push (list op) *gcc-program*)
                      t)
      (make-struct (lang0-prim-make-struct (first args) (rest args))
		   t)
      (struct-field (lang0-prim-struct-field (first args) (second args) (third args))
		    t)
      (if (let ((then-label (fresh-label "then"))
		(else-label (fresh-label "else"))
		(else-branch (third args)))
	    (compile-lang0-instruction (first args))
	    (push `(sel ,then-label ,else-label) *gcc-program*)

	    (push (lambda ()
		    (push `($mark-label ,then-label) *gcc-program*)
		    (compile-lang0-instruction (second args))
		    (push '(join) *gcc-program*))
		  *l0-delayed-code*)

	    (push (lambda ()
		    (push `($mark-label ,else-label) *gcc-program*)
		    (when else-branch
		      (compile-lang0-instruction else-branch))
		    (push '(join) *gcc-program*))
		  *l0-delayed-code*)
	    t))
      (while (let ((head-label (fresh-label "while-head-label"))
		   (body-label (fresh-label "while-body-label"))
		   (end-label (fresh-label "while-end-label")))
	       (push `($mark-label ,head-label) *gcc-program*)
	       (compile-lang0-instruction (first args))
	       (push `(tsel ,body-label ,end-label) *gcc-program*)
	       (push `($mark-label ,body-label) *gcc-program*)
	       (dolist (instr (rest args))
		 (compile-lang0-instruction instr))
	       (lang0-compile-goto head-label)
	       (push `($mark-label ,end-label) *gcc-program*)))
      ((+ - * / = > >= cons) (let* ((op-map '((+ . add)
					      (- . sub)
					      (* . mul)
					      (/ . div)
					      (= . ceq)
					      (> . cgt)
					      (>= . cgte)
					      (cons . cons)))
				    (opcode (assoc op op-map)))
			       (compile-lang0-instruction (first args))
			       (compile-lang0-instruction (second args))
			       (push (list (cdr opcode)) *gcc-program*)
			       t))
      (< (compile-lang0-instruction (second args))
	 (compile-lang0-instruction (first args))
	 (push '(cgt) *gcc-program*)
	 t)
      (<= (compile-lang0-instruction (second args))
	  (compile-lang0-instruction (first args))
	  (push '(cge) *gcc-program*)
	  t))))

(defun lang0-compile-goto (label)
  (push '(ldc 1) *gcc-program*)
  (push `(tsel ,label ,label) *gcc-program*))
