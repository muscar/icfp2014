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
	    ((rem-instr-p instr) (push instr new-program))
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
	      ((rem-instr-p instr) (setf pending-label (rest instr)))
	      (t (if pending-label
		     (progn
		       (format t "~{~a ~}~10,25@t; ~{~a ~}~%" instr pending-label)
		       (setf pending-label nil))
		     (format t "~{~a ~}~%" instr))))))

;; Lang 0

(defparameter *functions* '())
(defparameter *lambdas* '())
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

(defun fresh-var (prefix)
  (gensym prefix))

(defun fundefp (form)
  (and (consp form) (eq (first form) 'defun)))

(defun collect-functions (functions)
  (setf *functions* (mapcar #'analyze-function functions)))

(defun analyze-function (fun)
  (assert (fundefp fun) () "expecting function definition, but got ~a" fun)
  (destructuring-bind (name args &rest body) (cdr fun)
    (cons name (make-l0-function :name name
				 :args args
				 :locals (make-array 16 :adjustable t :fill-pointer 0)
				 :frame-size (length args)
				 :body (collect-lambdas body)))))

(defun collect-lambdas (body)
  (loop for instr in body
     collect (cond ((atom instr) instr)
		   ((eq (first instr) 'lambda) (let* ((lambda-name (fresh-var "$l"))
						      (lifted-lambda `(defun ,lambda-name ,@(rest instr))))
						 (push (analyze-function lifted-lambda) *lambdas*)
						 lambda-name))
		   (t (collect-lambdas instr)))))

(defun add-local (fun name)
  (vector-push-extend name (l0-function-locals fun))
  (incf (l0-function-frame-size fun)))

(defun arg-index (fun name)
  (position name (l0-function-args fun)))

(defun local-index (fun name)
  (let ((local-index (position name (l0-function-locals fun))))
    (when local-index
      (+ (length (l0-function-args fun))
	 local-index))))

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

(defun lang0-emit (opcode &rest operands)
  (push (cons opcode operands) *gcc-program*))

(defun lang0-mark-label (label)
  (lang0-emit '$mark-label label))

(defun lang0-compile-file (path)
  (with-open-file (in path)
    (do (program
	 (form #1=(read in nil nil) #1#))
	((null form) (compile-lang0 (nreverse program)))
      (push form program))))

(defun compile-l0-function (function)
  (let ((*l0-delayed-code* '())
	(function-body-label (intern (concatenate 'string (symbol-name (l0-function-name function)) "-BODY"))))
    (lang0-emit 'rem function-body-label)
    (lang0-mark-label function-body-label)
    (dolist (instr (l0-function-body function))
      (compile-lang0-instruction instr))
    (push '(rtn) *gcc-program*)
    (lang0-generate-function-stub function function-body-label)
    (dolist (codegen *l0-delayed-code*)
      (funcall codegen))))

(defun lang0-generate-function-stub (function function-body-label)
  (lang0-emit 'rem (l0-function-name function))
  (push `($mark-label ,(l0-function-name function)) *gcc-program*)
  (dotimes (idx (length (l0-function-locals function)))
    (lang0-emit 'ldc 0))
  (lang0-emit 'ldf function-body-label)
  (lang0-emit 'tap (length (l0-function-locals function))))

(defun compile-lang0 (program)
  (let ((*gcc-program* '())
	(*functions* '())
	(*lambdas* '())
	(*structs* '()))
    (multiple-value-bind (functions structs) (partition #'fundefp program)
      (collect-functions functions)
      (collect-structs structs)

      (let ((main-function (cdr (assoc 'main *functions*))))
	(unless main-function
	  (error "no main function defined"))
	(lang0-emit-program-start main-function))
      
      (dolist (*l0-current-function* (mapcar #'cdr (append *functions* *lambdas*)) (compile-gcc (nreverse *gcc-program*)))
	(compile-l0-function *l0-current-function*)))))

(defun lang0-emit-program-start (main-function)
  (dotimes (idx (l0-function-frame-size main-function))
    (lang0-emit 'ldc 0))
  (lang0-emit 'ldf 'main)
  (lang0-emit 'ap (l0-function-frame-size main-function))
  (lang0-emit 'rtn))

(defun compile-lang0-instruction (instr)
  (cond ((consp instr) (compile-lang0-call instr))
	((numberp instr) (push `(LDC ,instr) *gcc-program*))
	((symbolp instr) (compile-l0-symbol-ref instr))
	(t (error "unknown instruction ~a" instr))))

(defun compile-l0-symbol-ref (sym)
  (or (compile-l0-variable-ref sym)
      (compile-l0-function-ref sym)
      (error "Undefined symbol ~a" sym)))

(defun compile-l0-function-ref (sym)
  (let ((fun (assoc sym (append *functions* *lambdas*))))
    (when fun
      (push `(ldf ,(l0-function-name (cdr fun))) *gcc-program*)
      t)))

(defun get-variable-ref (sym)
  (let ((local-idx (position sym (l0-function-locals *l0-current-function*))))
    (if local-idx
	(values 0 local-idx)
	(let ((arg-idx (position sym (l0-function-args *l0-current-function*))))
	  (when arg-idx
	      (values 1 arg-idx))))))

(defun compile-l0-variable-ref (sym)
  (multiple-value-bind (level idx) (get-variable-ref sym)
    (when level
      (lang0-emit 'ld level idx)
      t)))

(defun compile-lang0-call (instr)
  (unless (symbolp (first instr))
    (error "call operator ~a is not a symbol" (first instr)))
  (unless (compile-lang0-prim instr)
    (let ((fun (assoc (first instr) *functions*)))
      (if fun
	  (let ((fun-arity (length (l0-function-args (cdr fun)))))
	    (unless (= (length (rest instr)) fun-arity)
	      (error "wrong argument count for ~a; expecting ~a args but got ~a"
		     (car fun)
		     fun-arity
		     (length (rest instr))))
	    (dolist (arg (rest instr))
	      ;(print arg)
	      (compile-lang0-instruction arg))
	    (push `(LDF ,(l0-function-name (cdr fun))) *gcc-program*)
	    (push `(AP ,(length (l0-function-args (cdr fun)))) *gcc-program*))
	  (progn
	    (dolist (arg (rest instr))
	      (compile-lang0-instruction arg))
	    (unless (compile-l0-variable-ref (first instr))
	      (error "unknown function ~a" (first instr)))
	    (lang0-emit 'ap (length (rest instr))))))))

;; Lang0 primitives

(defun lang0-prim-make-struct (name values)
  (let ((struct (cdr (assoc name *structs*))))
    (unless struct
      (error "unknown structure ~a" name))
    (unless (= (length (l0-struct-fields struct)) (length values))
      (error "trying to construct ~a struct wrong number of values; expecting ~a, got ~a"
	     name
	     (length (l0-struct-fields struct))
	     (length values)))
    (lang0-prim-list values)))

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

(defun lang0-struct-field-set (name field-name value instance)
  (let ((struct (cdr (assoc name *structs*))))
    (unless struct
      (error "unknown structure ~a" name))
    (let* ((fields (l0-struct-fields struct))
	   (fields-count (length fields))
	   (field-index (cdr (assoc field-name fields))))
      (unless field-index
	(error "unknown field ~a" field-name))
      (dotimes (idx fields-count)
	(cond ((= idx field-index) (compile-lang0-instruction value))
	      (t (compile-lang0-instruction instance)
		 (dotimes (_ idx)
		   (lang0-emit 'cdr))
		 (lang0-emit 'car))))
      ;; Emit NIL
      (lang0-emit 'ldc 0)
      (dotimes (idx fields-count)
	(lang0-emit 'cons)))))

(defun lang0-prim-set! (place value)
  (cond ((symbolp place) (multiple-value-bind (level idx) (get-variable-ref place)
			   (compile-lang0-instruction value)
			   (unless level
			     (error "undefined set place ~a" place))
			   (lang0-emit 'st level idx)))
	((and (consp place)
	      (eq (first place) 'struct-field))
	 (multiple-value-bind (level idx) (get-variable-ref (fourth place))
	   (unless level
	     (error "can only set fields of structs that are sotred in local variables"))
	   (lang0-struct-field-set (second place) (third place) value (fourth place))
	   (lang0-emit 'st level idx)))))

(defun lang0-prim-list (values)
  (dolist (value values)
    (compile-lang0-instruction value))
  ;; Emit NIL
  (lang0-emit 'ldc 0)
  (dotimes (idx (length values))
    (lang0-emit 'cons)))

(defun compile-lang0-prim (instr)
  (destructuring-bind (op &rest args) instr
    (case op
      (local (dolist (var args)
	       (cond ((symbolp var) (add-local *l0-current-function* var))
		     ((consp var) (add-local *l0-current-function* (first var))
		                  (lang0-prim-set! (first var) (second var)))
		     (t (error "malformed local binding ~a" var))))
	     t)
      (list (lang0-prim-list args)
	    t)
      (rem (lang0-emit 'rem (first args)))
      (make-struct (lang0-prim-make-struct (first args) (rest args))
		   t)
      (struct-field (lang0-prim-struct-field (first args) (second args) (third args))
		    t)
      (set! (lang0-prim-set! (first args) (second args))
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
  (lang0-emit 'tsel label label))

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
    (lang0-emit 'sel then-label else-label)

    (push (lambda ()
	    (lang0-mark-label then-label)
	    (compile-lang0-instruction then-body)
	    (push '(join) *gcc-program*))
	  *l0-delayed-code*)

    (push (lambda ()
	    (lang0-mark-label else-label)
	    (when else-body
	      (compile-lang0-instruction else-body))
	    (push '(join) *gcc-program*))
	  *l0-delayed-code*)
    t))

(defun compile-l0-while (condition body)
  (let ((head-label (fresh-label "while-head-label"))
	(body-label (fresh-label "while-body-label"))
	(end-label (fresh-label "while-end-label")))
    (lang0-mark-label head-label)
    (compile-lang0-instruction condition)
    (lang0-emit 'tsel body-label end-label)
    (lang0-mark-label body-label)
    (dolist (instr body)
      (compile-lang0-instruction instr))
    (compile-l0-goto head-label)
    (lang0-mark-label end-label))
  t)

(defun compile-l0-begin (body)
  (dolist (instr body)
    (compile-lang0-instruction instr))
  t)
