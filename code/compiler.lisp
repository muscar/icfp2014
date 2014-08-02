(defpackage :icfp
  (:use :cl)
  (:export :lang0-compile-file))

(in-package :icfp)

(defun split-at (pos sequence)
  (values (subseq sequence 0 pos)
	  (subseq sequence (1+ pos))))

(defparameter *gcc-program* '())
(defparameter *gcc-out-stream* '())

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
				     (format t "~{~a ~}~10,25@t; address of ~a~%" instr name)
				     (format *gcc-out-stream* "~{~a ~}~10,25@t; address of ~a~%" instr name)))
	      ((rem-instr-p instr) (setf pending-label (rest instr)))
	      (t (if pending-label
		     (progn
		       (format t "~{~a ~}~10,25@t; ~{~a ~}~%" instr pending-label)
		       (format *gcc-out-stream* "~{~a ~}~10,25@t; ~{~a ~}~%" instr pending-label)
		       (setf pending-label nil))
		     (progn
		       (format t "~{~a ~}~%" instr)
		       (format *gcc-out-stream* "~{~a ~}~%" instr)))))))

;; Lang 0

(defparameter *constants* '())
(defparameter *functions* '())
(defparameter *lambdas* '())
(defparameter *structs* '())
(defparameter *l0-current-function* nil)

(defstruct l0-function
  name
  env
  body)

(defun l0-function-args (fun)
  (cadr (l0-function-env fun)))

(defun l0-function-locals (fun)
  (car (l0-function-env fun)))

(defstruct l0-struct
  name
  fields)

(defun fresh-var (prefix)
  (gensym prefix))

(defun defun-p (form)
  (and (consp form) (eq (first form) 'defun)))

(defun defstruct-p (form)
  (and (consp form) (eq (first form) 'defstruct)))

(defun defconstant-p (form)
  (and (consp form) (eq (first form) 'defconstant)))

(defun collect-functions (functions)
  (let ((funs (mapcar (lambda (fun) (analyze-function fun '())) functions)))
    (setf *functions* funs)))

(defun analyze-function (fun env)
  (assert (defun-p fun) () "expecting function definition, but got ~a" fun)
  (destructuring-bind (name args &rest body) (cdr fun)
    (let ((function (make-l0-function :name name
				      :env (cons '() (cons args env))
				      :body '())))
      (setf (l0-function-body function) (collect-lambdas function body))
      (cons name function))))

(defun add-constant (name value)
  (cond
    ((not (numberp value)) (error "constant ~a must be constant" name))
    ((assoc name *constants*) (error "duplicate definition for constant ~a" name))
    (t (push (cons name value) *constants*))))

(defun add-local (fun local)
  (let* ((env (l0-function-env fun))
	 (new-env (cons (reverse (cons local (reverse (car env)))) (cdr env))))
    (setf (l0-function-env fun) new-env)))

(defun collect-locals (fun locals)
  (loop for local in locals collect
       (cond ((atom local) (add-local fun local)
	                   local)
	     (t (add-local fun (car local))
		(collect-lambdas fun local)))))

(defun collect-lambdas (fun body)
  (loop for instr in body
     collect (cond ((atom instr) instr)
		   ((eq (first instr) 'lambda) (let* ((lambda-name (fresh-var "$l"))
						      (lifted-lambda `(defun ,lambda-name ,@(rest instr))))
						 (push (analyze-function lifted-lambda (l0-function-env fun)) *lambdas*)
						 lambda-name))
		   ((eq (first instr) 'local) (cons 'local (collect-locals fun (rest instr))))
		   (t (collect-lambdas fun instr)))))

(defun collect-structs (structs)
  (setf *structs* (mapcar #'analyze-struct structs)))

(defun collect-constants (constants)
  (dolist (constant constants)
    (add-constant (second constant) (third constant))))

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

(defun l0-emit-const (value)
  (lang0-emit 'ldc value))

(defun l0-emit-nil ()
  (l0-emit-const 0))

(defun l0-emit-t ()
  (l0-emit-const 1))

(defun read-file (path)
  (with-open-file (in path)
    (do (program
	 (form #1=(read in nil nil) #1#))
	((null form) (nreverse program))
      (push form program))))

(defun process-includes (program)
  (let ((instrs))
    (dolist (instr program)
      (if (and (consp instr)
	       (eq (car instr) 'include))
	  (setf instrs (append (reverse (process-includes (read-file (cadr instr)))) instrs))
	  (push instr instrs)))
    (reverse instrs)))

(defun lang0-compile-file (path)
  (compile-lang0 (process-includes (read-file path))))

(defun lookup-env (sym)
  (let ((level 0)
	(depth 0))
    (loop for env in (l0-function-env *l0-current-function*) do
	 (unless (or (car env) (= (mod depth 2) 1))
	   (decf level))

	 (let ((idx (position sym env)))
	   (when idx
	     (return (cons level idx))))

	 (incf level)
	 (incf depth))))

(defun compile-l0-function (function)
  (let ((function-body-label (intern (concatenate 'string (symbol-name (l0-function-name function)) "-BODY"))))
    (when (lang0-generate-function-stub function function-body-label)
      (lang0-emit 'rem function-body-label)
      (lang0-mark-label function-body-label))
    (dolist (instr (l0-function-body function))
      (compile-lang0-instruction instr))
    (lang0-emit 'rtn)))

(defun lang0-generate-function-stub (function function-body-label)
  (lang0-emit 'rem (l0-function-name function))
  (lang0-mark-label (l0-function-name function))
  (let ((num-locals (length (l0-function-locals function))))
    (when (> num-locals 0)
      (dotimes (idx num-locals)
	(lang0-emit 'ldc 0))
      (lang0-emit 'ldf function-body-label)
      (lang0-emit 'tap num-locals)
      t)))

(defun compile-lang0 (program)
  (let ((*gcc-program* '())
	(*constants* '())
	(*functions* '())
	(*lambdas* '())
	(*structs* '()))

    (let ((functions (remove-if (complement #'defun-p) program))
	  (structs (remove-if (complement #'defstruct-p) program))
	  (constants (remove-if (complement #'defconstant-p) program)))
      (collect-functions functions)
      (collect-structs structs)
      (collect-constants constants)

      ;; (format t "~a~%" *functions*)

      (with-open-file (*gcc-out-stream* #p"out.gcc"
					:direction :output
					:if-exists :supersede
					:if-does-not-exist :create)
	(let ((main-function (cdr (assoc 'main *functions*))))
	  (unless main-function
	    (error "no main function defined"))
	  (lang0-emit-program-start main-function))

	(let ((functions (mapcar #'cdr (append *functions* *lambdas*))))
	  (dolist (*l0-current-function* functions (compile-gcc (nreverse *gcc-program*)))
	    (compile-l0-function *l0-current-function*)))))))

(defun lang0-emit-program-start (main-function)
  (compile-l0-goto (l0-function-name main-function)))

(defun compile-lang0-instruction (instr)
  (cond ((consp instr) (compile-lang0-call instr))
	((numberp instr) (lang0-emit 'ldc instr))
	((symbolp instr) (compile-l0-symbol-ref instr))
	(t (error "unknown instruction ~a" instr))))

(defun compile-l0-symbol-ref (sym)
  (or (compile-l0-constant-ref sym)
      (compile-l0-variable-ref sym)
      (compile-l0-function-ref sym)
      (error "Undefined symbol ~a" sym)))

(defun get-constant-ref (sym)
  (cdr (assoc sym *constants*)))

(defun compile-l0-constant-ref (sym)
  (let ((constant (get-constant-ref sym)))
    (when constant
	  (lang0-emit 'ldc constant)
	  t)))

(defun compile-l0-function-ref (sym)
  (let ((fun (assoc sym (append *functions* *lambdas*))))
    (when fun
      (lang0-emit 'ldf (l0-function-name (cdr fun)))
      t)))

(defun get-variable-ref (sym)
  (let ((pos (lookup-env sym)))
    (when pos
      (values (car pos) (cdr pos)))))

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
	      (compile-lang0-instruction arg))
	    (lang0-emit 'ldf (l0-function-name (cdr fun)))
	    (lang0-emit 'ap (length (rest instr))))
	  (progn
	    (dolist (arg (rest instr))
	      (compile-lang0-instruction arg))
	    (unless (compile-l0-symbol-ref (first instr))
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
			   (lang0-emit 'rem place)
			   (lang0-emit 'st level idx)))
	((and (consp place)
	      (eq (first place) 'struct-field))
	 (multiple-value-bind (level idx) (get-variable-ref (fourth place))
	   (unless level
	     (error "can only set fields of structs that are sotred in local variables"))
	   (lang0-struct-field-set (second place) (third place) value (fourth place))
	   (lang0-emit 'st level idx))))
  t)

(defun lang0-prim-list (values)
  (dolist (value values)
    (compile-lang0-instruction value))
  ;; Emit NIL
  (lang0-emit 'ldc 0)
  (dotimes (idx (length values))
    (lang0-emit 'cons))
  t)

(defun compile-lang0-prim (instr)
  (destructuring-bind (op &rest args) instr
    (case op
      (local (dolist (var args)
	       (cond ((symbolp var) (when (assoc var *constants*)
				      (error "cannot redefine constant ~a as variable~%" var)))
		     ((consp var) (when (assoc (first var) *constants*)
				      (error "cannot redefine constant ~a as variable~%" var))
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
                           (lang0-emit op)
                           t)
      (if (compile-l0-if (first args) (second args) (third args)))
      (cond (compile-l0-cond args))
      (while (compile-l0-while (first args) (rest args)))
      (when (compile-l0-when (first args) (rest args)))
      (unless (compile-l0-unless (first args) (rest args)))
      (begin (compile-l0-begin args))
      ((= > >= cons) (compile-l0-binop op (first args) (second args)))
      (+ (l0-prim-arith 'add args))
      (- (l0-prim-arith 'sub args))
      (* (l0-prim-arith 'mul args))
      (/ (l0-prim-arith 'div args))
      (< (compile-l0-binop '> (second args) (first args)))
      (<= (compile-l0-binop '>= (second args) (first args)))
      (and (l0-prim-and args))
      (or (l0-prim-or args))
      (not (compile-l0-if (first args) 0 1))
      (incf (compile-l0-incf (first args)))
      (decf (compile-l0-decf (first args)))
      (t (let ((pos (position #\. (symbol-name op))))
	   (when pos
	     (multiple-value-bind (struct field) (split-at pos (symbol-name op))
	       (lang0-prim-struct-field (intern struct) (intern field) (first args))
	       t)))))))

(defun compile-l0-goto (label)
  (lang0-emit 'ldc 1)
  (lang0-emit 'tsel label label)
  t)

(defun compile-l0-binop (op first-arg second-arg)
  (let* ((op-map '((= . ceq)
		   (> . cgt)
		   (>= . cgte)
		   (cons . cons)))
	 (opcode (assoc op op-map)))
    (compile-lang0-instruction first-arg)
    (compile-lang0-instruction second-arg)
    (lang0-emit (cdr opcode))
    t))

(defun fold-constant (expr)
  (cond ((numberp expr) expr)
	((symbolp expr) (let ((constant (get-constant-ref expr)))
			  (if constant
			      (fold-constant constant)
			      expr)))
	(t expr)))

(defun is-constant (expr)
  (let ((e (fold-constant expr)))
    (when (numberp e)
      e)))

(defun compile-l0-if (condition then-body else-body)
  (let ((constant (is-constant condition)))
    (if constant
	(progn
	  (compile-lang0-instruction (if (= 0 constant) else-body then-body))
	  t)
	(let ((then-label (fresh-label "then"))
	      (else-label (fresh-label "else"))
	      (after-label (fresh-label "after")))
	  (compile-lang0-instruction condition)
	  (lang0-emit 'tsel then-label else-label)

	  (lang0-mark-label then-label)
	  (compile-lang0-instruction then-body)
	  (when else-body
	    (compile-l0-goto after-label))

	  (lang0-mark-label else-label)
	  (when else-body
	    (compile-lang0-instruction else-body)
	    (lang0-mark-label after-label))

	  t))))

(defun compile-l0-cond (conditions)
  (if conditions
    (compile-l0-if (caar conditions) (cons 'begin (cdar conditions)) `(cond ,@(cdr conditions)))
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

(defun compile-l0-when (condition body)
  (compile-l0-if condition `(begin ,@body) nil))

(defun compile-l0-unless (condition body)
  (compile-l0-if `(not ,condition) `(begin ,@body) nil))

(defun compile-l0-incf (place)
  (lang0-prim-set! place `(+ ,place 1)))

(defun compile-l0-decf (place)
  (lang0-prim-set! place `(- ,place 1)))

(defun l0-prim-or (args)
  (cond ((null args) (l0-emit-nil) t)
	((null (cdr args)) (compile-lang0-instruction (car args)))
	((null (cddr args)) (compile-l0-if (first args) 1 (second args)))
	(t (l0-prim-or `(,(car args) (or ,@(cdr args)))))))

(defun l0-prim-and (args)
  (cond ((null args) (l0-emit-t) t)
	((null (cdr args)) (compile-lang0-instruction (car args)) t)
	((null (cddr args)) (compile-l0-if (first args) (second args) 0) t)
	(t (l0-prim-and `(,(car args) (and ,@(cdr args)))))))

(defun l0-prim-arith (op args)
  (cond ((null args) (l0-emit-nil) t)
	(t (let ((count 0))
	     (dolist (arg args)
	       (incf count)
	       (compile-lang0-instruction arg))
	     (dotimes (idx (- count 1))
	       (lang0-emit op))
	     t))))
