(define (factorial n)
 (if (zero? n)
     1
     (* n (factorial (- n 1)))))

(define (my-if antecedent consequent alternate)
 (if antecedent consequent alternate))

(define (my-factorial n)
 (my-if (zero? n)
	1
	(* n (my-factorial (- n 1)))))
