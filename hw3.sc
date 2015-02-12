;;; splits a list into different combinations return a list of them
(define (splits l)
 (if (null? l) (list (list '() '()))
     (cons (list '() l)
	   (map  (lambda (split) (list (cons (first l) (first split))
				       (second split)))
		 (splits (rest l))))))

(define (pattern-variable? pattern) (memq pattern '(e e1 e2 e3)))

(define (pattern-list-variable? pattern) (memq pattern '(e... e1... e2... e3...
							      )))

(define (lookup-pattern-variable pattern-variable bindings)
 (cond
  ((null? bindings) #f)
  ((eq? pattern-variable (first (first bindings)))
   (second (first bindings)))
  (else (lookup-pattern-variable
	 pattern-variable (rest bindings)))))

(define (inconsistent-binding? b1 b2)
 (and (eq? (first b1) (first b2)) (not (equal? (second b1) (second b2)))))

(define (inconsistent-bindings? r1 r2)
 (some (lambda (b1) (some (lambda (b2) (inconsistent-binding? b1 b2)) r2)) r1))

(define (match env pattern expression)
 (cond
  ((pattern-variable? pattern)
   (if (inconsistent-bindings? env
			       (list (list pattern expression)))
       (list #f)
       (list (list pattern expression))))
  ((pattern-list-variable? pattern)
   (list (list pattern expression)))
  ((and (list? pattern)
	(= (length pattern) 1)
	(pattern-list-variable? (first pattern)))
   (list (list (first pattern) expression)))
  ((and (list? pattern) (not (null? pattern)))
   (if (and (list? expression) (not (null? expression)))
       (if (pattern-list-variable? (first pattern))
	   (let ((bindings (match-with-splits env (splits expression)
					      (first pattern) (rest pattern))))
	    (if (memq #f bindings) (list #f) bindings))
	   (let* ((bindings1 (match env (first pattern) (first expression)))
		  
		  (bindings2
		   (if (memq #f bindings1)
		      (list #f)
		   (match (append env bindings1)
				    (rest pattern) (rest expression)))))
	    (if (or (memq #f bindings1) (memq #f bindings2)) (list #f)
		(append bindings1 bindings2))))
       (list #f)))
  ((equal? pattern expression) '())
  (else (list #f))))

(define (match-with-splits env splits pat1 pat2)
 (if (null? splits) (list #f)
     (let* ((split (first splits))
	    (lhs (first split))
	    (rhs (second split))
	    (b1 (match env pat1 lhs))
	    (b2 (match (append env b1) pat2 rhs))
	    (bs (lambda ()
		 (append b1 b2))))
      (cond
       ((or (memq #f b1) (memq #f b2))
	(match-with-splits env (rest splits) pat1 pat2))
       (else (bs))))))

(define (instantiate pattern bindings)
 (cond ((pattern-variable? pattern)
	(lookup-pattern-variable pattern bindings))
       ((pattern-list-variable? pattern)
	(lookup-pattern-variable pattern bindings))
      ;;((and (list? pattern)
	;;    (= (length pattern) 1)
	  ;;  (pattern-list-variable? (first pattern)))
	;;(lookup-pattern-variable (first pattern) bindings))
       ((and (list? pattern) (not (null? pattern)))
	(append (makelist (instantiate (first pattern) bindings))
	      (instantiate (rest pattern) bindings)))
       (else pattern)))
(define (makelist l)
 (if (list? l) l (list l)))

(define (applicable? rule expression)
 (not (memq #f (match '() (first rule) expression))))

(define (first-applicable-rule rules expression)
 (cond ((null? rules) #f)
       ((applicable? (first rules) expression) (first rules))
       (else (first-applicable-rule (rest rules) expression))))

(define (apply-rule rule expression)
 (instantiate (third rule) (match '() (first rule) expression)))

(define (apply-rules rules expression)
 (let ((rule (first-applicable-rule rules expression)))
  (if rule
      (rewrite rules (apply-rule rule expression))
      expression)))


(define (rewrite rules expression)
 (if (list? expression)
     (apply-rules
      rules
      (map (lambda (expression) (rewrite rules expression))
	   expression))
     expression))

(define *SATRules*
 '(
					; not #t --> #f
   ((not #t)
    -~->
    #f)
					; not #f --> #t
   ((not #f)
    -~->
    #t)
					; double negation
   ((not (not e))
    -~->
    e)
					; and
   (and
    -~->
    #t)
					; and p
   ((and e)
    -~->
    e)
					; and #t
   ((and e1... #t e2...)
    -~->
    (and e1... e2...))
					; and #f
   ((and e1... #f e2...)
    -~->
    #f)
					; nested and
   ((and e1... (and e2...) e3...)
    -~->
    (and e1... e2... e3...))
					; duplicate props in and
   ((and e1... e e2... e e3...)
    -~->
    (and e1... e e2... e3...))
					; contradictions in and - 1
   ((and e1... e e2... (not e) e3...)
    -~->
    #f)
					; contradictions in and - 2
   ((and e1... (not e) e2... e e3...)
    -~->
    #f)
				        ; or
   (or
    -~->
    #f)
					; or P
   ((or e)
    -~->
    e)
					; or #f
   ((or e1... #f e2...)
    -~->
    (or e1... e2...))
					; or #t
   ((or e1... #t e2...)
    -~->
    #t)
					; nested or
   ((or e1... (or e2...) e3...)
    -~->
    (or e1... e2... e3...))
					; duplicate props in or
   ((or e1... e e2... e e3...)
    -~->
    (or e1... e e2... e3...))
					; LEM - 1
   ((or e1... e e2... (not e) e3...)
    -~->
    #t)
					; LEM - 2
   ((and e1... (not e) e2... e e3...)
    -~->
    #t)
   ))



(define (boolean-simplify phi)
 (rewrite *SATRules* phi))
;;;
(define (truth-tables-match? foo1 foo2)
 (define (eqfoo2 a)
  (eq? a foo2))
 (if (boolean? foo2)
     (reduce and (map eqfoo2 (map second (truth-table foo1))) #t)
     (table-equal? (truth-table foo1) (truth-table foo2)))
 )


(define (or a b)
 (or a b))
(define (and a b)
 (and a b))
(define (list-equal-noorder? l1 l2)
     (eq? (length (set-intersection (first l1) (first l2))) (length (first l1))));; two list is equal without same order

(define (table-equal? l1 l2)
 (define (equal-first? flist)
  (define (list-equal-noorder-flist? l4)
   (list-equal-noorder? flist l4))
  (reduce or (map list-equal-noorder-flist? l2) #f))
 (if (list-equal? l1 l2)
     #t
     (if (not (eq? (length l1) (length l2)))
	 #f
	 (reduce and
		 (map equal-first? l1) #t))));;find the first element is in list2?


(define (list-equal? l1 l2)
 (if (or (symbol? l1) (boolean? l1))
     (if (or (symbol? l2) (boolean? l2))

	 (eq? l1 l2)
	 #f)
     (if (not (eq? (length l1) (length l2)))
	 #f
	 (if (eq? (length l1) 1)
	     (list-equal? (first l1) (first l2))
	     (let ((temp (list-equal? (rest l1) (rest l2))))
	      (and (list-equal? (first l1) (first l2))
		   temp))))))
;;; Boolean Truth table
(define (propositions-in formula)
 (cond ((symbol? formula) (list formula))
       ((boolean? formula) '())
       ((and (list? formula) (not (null? formula)))
	(case (first formula)
	 ((not) (if (= (length formula) 2)
		    (propositions-in (second formula))
		    (panic "Unrecognized formula")))
	 ((and) (reduce unionq (map propositions-in (rest formula)) '()))
	 ((or) (reduce unionq (map propositions-in (rest formula)) '()))
	 (else (panic "Unrecognized formula"))))
       (else (panic "Unrecognized formula"))))

(define (all-truth-assignments propositions)
 (if (null? propositions)
     '(())
     (let ((truth-assignments (all-truth-assignments (rest propositions))))
      (append (map (lambda (truth-assignment)
		    (cons (list (first propositions) #t) truth-assignment))
		   truth-assignments)
	      (map (lambda (truth-assignment)
		    (cons (list (first propositions) #f) truth-assignment))
		   truth-assignments)))))

(define (lookup-proposition proposition truth-assignment)
 (cond ((null? truth-assignment) (panic "Proposition not in truth assignment"))
       ((eq? proposition (first (first truth-assignment)))
	(second (first truth-assignment)))
       (else (lookup-proposition proposition (rest truth-assignment)))))

(define (boolean-evaluate formula truth-assignment)
 (cond ((symbol? formula) (lookup-proposition formula truth-assignment))
       ((boolean? formula) formula)
       ((and (list? formula) (not (null? formula)))
	(case (first formula)
	 ((not) (if (= (length formula) 2)
		    (not (boolean-evaluate (second formula) truth-assignment))
		    (panic "Unrecognized formula")))
	 ((and) (every (lambda (formula)
			(boolean-evaluate formula truth-assignment))
		       (rest formula)))
	 ((or) (some (lambda (formula)
		      (boolean-evaluate formula truth-assignment))
		     (rest formula)))
	 (else (panic "Unrecognized formula"))))
       (else (panic "Unrecognized formula"))))

(define (truth-table formula)
 (map (lambda (truth-assignment)
       (list truth-assignment (boolean-evaluate formula truth-assignment)))
      (all-truth-assignments (propositions-in formula))))

(define (member? x list)
 (cond ((null? list) #f)
       ((equal? x (first list)) #t)
       (else (member? x (rest list)))))

(define (setify x)
 (define (loop x c)
  (cond ((null? x) c)
        ((member? (first x) c) (loop (rest x) c))
        (else (loop (rest x) (cons (first x) c)))))
 (loop x '()))

(define (set-union x y)
 (define (loop x y c)
  (cond ((null? x) (append c y))
        ((member? (first x) y) (loop (rest x) y c))
        (else (loop (rest x) y (cons (first x) c)))))
 (loop (setify x) (setify y) '()))

(define (set-intersection x y)
 (define (loop x y c)
  (cond ((null? x) c)
        ((member? (first x) y) (loop (rest x) y (cons (first x) c)))
        (else (loop (rest x) y c))))
 (loop (setify x) (setify y) '()))

(define (set-minus x y)
 (define (loop x y c)
  (cond ((null? x) c)
        ((member? (first x) y) (loop (rest x) y c))
        (else (loop (rest x) y (cons (first x) c)))))
 (loop (setify x) (setify y) '()))
