;;;;;;;;;;;backtracking implementation

(define (place-n-queens-by-backtracking N)
 (let ((q (place-n-queens N)))
  (for-each-indexed
   (lambda (j i)
    (place-queen i j))
   q)))

(define (place-n-queens N)
 (define (loop columns)
  (if (= (length columns) n)
      columns
      (let ((column (an-integer-between 0 (- n 1))))
       (check-queen column columns)
       (loop (cons column columns)))))
 (loop '()))

(define (check-queen new-column old-columns)
 (for-each-indexed
  (lambda (old-column i)
   (when (attacks? new-column old-column (+ i 1))
    (fail)))
  old-columns))


;;;;;;;;;;;;;;useful function implementation

(define (list-of n f)
 (if (= n 0) '() (cons (f) (list-of (- n 1) f))))

(define (attacks? qi qj delta-rows)
 (or (= qi qj) (= (abs (- qi qj)) delta-rows)))

;;;;;;;;;;;;;;place n queens by constraints function

(define (place-n-queens-by-constraints n)
 (define (demon-loop domain-variable i)
  (attach-after-demon!
   (lambda ()
    (when (bound? domain-variable) (place-queen i (binding domain-variable))))
   domain-variable))
 (let* ((domain (enumerate n))
	(vars (list-of n (lambda ()
			  (create-domain-variable domain))))
	(x (for-each-indexed demon-loop vars))
	(x1 (for-each-pair (lambda (v1 v2 j)
			    (assert-constraint!
			     (lambda (a b) (not (attacks? a b j)))
			     (list v1 v2)))
			   vars)))
  (csp-solution vars first)))

(define (for-each-pair f l)
 (unless (null? l)
  (let ((x (for-each-indexed (lambda (x2 i)
			      (f (first l) x2 (+ i 1)))
			     (rest l))))
   (for-each-pair f (rest l)))))


;;;;;;;;;;;;;;;;Generalized Forward Checking



(define (assert-unary-constraint-gfc! constraint x)
 (attach-after-demon!
  (lambda ()
   (when (some (lambda (xe) (not (constraint xe)))
	       (domain-variable-domain x))
    (restrict-domain!
     x
     (remove-if (lambda (x1) (not (constraint x1))) (domain-variable-domain x)))))
  x))

(define (assert-binary-constraint-gfc! constraint x y)
 (for-each
  (lambda (v)
   (attach-after-demon!
    (lambda ()
     (when (bound? x)
      (when (some (lambda (ye) (not (constraint (binding x) ye)))
		  (domain-variable-domain y))
       (restrict-domain!
	y
	(remove-if (lambda (y1) (not (constraint (binding x) y1))) (domain-variable-domain y)))))
     (when (bound? y)
      (when (some (lambda (xe) (not (constraint xe (binding y))))
		  (domain-variable-domain x))
       (restrict-domain!
	x
	(remove-if (lambda (x1) (not (constraint x1 (binding y)))) (domain-variable-domain x))))))
    v))
  (list x y)))

;;;;;;;;;;;;;;Arc Consistency
(define (or a b)
 (or a b))

(define (assert-unary-constraint-ac! constraint x)
 (restrict-domain!
  x
  (remove-if-not
   (lambda (x0) (constraint x0))
   (domain-variable-domain x))))

(define (assert-binary-constraint-ac! constraint x y)
 (attach-after-demon!
  (lambda ()
   (restrict-domain!
    y
    (remove-if-not
     (lambda (y1)
      (some (lambda (x1) (constraint x1 y1))
	    (domain-variable-domain x)))
     (domain-variable-domain y)))) x)
  (attach-after-demon!
  (lambda ()
   (restrict-domain!
    x
    (remove-if-not
     (lambda (x2)
      (some (lambda (y2) (constraint x2 y2))
	    (domain-variable-domain y)))
     (domain-variable-domain x)))) y))
 
    
	 
   
