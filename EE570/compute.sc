;;; (compute expression bindings definitions) ===> value

;;; A value is a number.
;;; A variable is a symbol.
;;; An expression is
;;;  - a value,
;;;  - a variable,
;;;  - (+ e1 ... en), where e1, ..., en are expressions,
;;;  - (- e1 e2 ... en), where e1, e2, ..., en are expressions,
;;;  - (* e1 ... en), where e1, ..., en are expressions,
;;;  - (/ e1 e2 ... en), where e1, e2, ..., en are expressions,
;;;  - (sqrt e), where e is an expression,
;;;  - (expt e1 e2), where e1 and e2 are expressions,
;;;  - (if e1 e2 e3), where e1, e2, and e3 are expressions, or
;;;  - (f e1 ... en), where f is a function name and e1, ..., en are
;;;                   expressions.
;;; A definition is (define (f x1 ... xn) e) where f is a function name,
;;; x1, ..., xn are variables, and e is an expression.
;;; A binding is (x v) where x is a variable and v is a value.

(define (lookup variable bindings)
 (cond ((null? bindings) (panic "Undefined variable"))
       ((eq? variable (first (first bindings))) (second (first bindings)))
       (else (lookup variable (rest bindings)))))

(define (lookup-definition function-name definitions)
 (cond
  ((null? definitions) (panic "Undefined function"))
  ((eq? function-name (first (second (first definitions)))) (first definitions))
  (else (lookup-definition function-name (rest definitions)))))

(define (make-bindings variables values)
 (if (null? variables)
     (if (null? values)
	 '()
	 (panic "Too many arguments"))
     (if (null? values)
	 (panic "Too few arguments")
	 (cons (list (first variables) (first values))
	       (make-bindings (rest variables) (rest values))))))

(define (compute e bindings definitions)
 (define (compute-with-stuff e) (compute e bindings definitions))
 (cond ((number? e) e)
       ((symbol? e) (lookup e bindings))
       ((list? e)
	(if (null? e)
	    (panic "Invalid expression")
	    (case (first e)
	     ((+) (map-reduce + 0 compute-with-stuff (rest e)))
	     ((-) (case (length e)
		   ((1) (panic "- must take at least one argument"))
		   ((2) (- (compute (second e) bindings definitions)))
		   (else (- (compute (second e) bindings  definitions)
			    (map-reduce
			     + 0 compute-with-stuff (rest (rest e)))))))
	     ((*) (map-reduce * 1 compute-with-stuff (rest e)))
	     ((/) (case (length e)
		   ((1) (panic "/ must take at least one argument"))
		   ((2) (/ (compute (second e) bindings  definitions)))
		   (else (/ (compute (second e) bindings  definitions)
			    (map-reduce
			     * 1 compute-with-stuff (rest (rest e)))))))
	     ((sqrt) (if (= (length e) 2)
			 (sqrt (compute (second e) bindings  definitions))
			 (panic "sqrt must take exactly one argument")))
	     ((expt) (if (= (length e) 3)
			 (expt (compute (second e) bindings  definitions)
			       (compute (third e) bindings  definitions))
			 (panic "expt must take exactly two arguments")))
	     ((si) (if (= (length e) 4)
		       (if (zero? (compute (second e) bindings  definitions))
			   (compute (third e) bindings  definitions)
			   (compute (fourth e) bindings  definitions))
		       (panic "if must take exactly four arguments")))
	     (else (let* ((definition (lookup-definition (first e) definitions))
			  (values (map compute-with-stuff (rest e)))
			  (bindings
			   (make-bindings (rest (second definition)) values)))
		    (compute (third definition) bindings  definitions))))))
       (else (panic "Invalid expression"))))
