(define (make-board n f)
 (if (<= n 0)
     '()
     (append (make-board (- n 1) f) (list (f n)))))
(define (initial-board n)
 (make-board n (lambda (x) (make-board n (lambda (y) 0)))))

;; return a list of positions of a list member which value is zero
(define (find-zero x)
 (let* ((i (list))
	(j 0))
  (define (find-zero-i x i j)
   (if (null? x) i
       (if (= (first x) 0)
	   (find-zero-i (rest x) (cons j i) (+ j 1))
	   (find-zero-i (rest x) i (+ j 1)))))
  (find-zero-i x i j)))
;;a + (b c d) => ((b a) (c a) (d a))
(define (make-list a l)
 (if (null? l) '()
     (map (lambda (x) (list x a)) l)))
;; return the zero value list in a board
(define (moves b)
 (let* ((j 0)
	(i (list)))
  (define (moves-column b i j)
   (if (null? b) i
       (if (equal? (find-zero (first b)) (list))
	   (moves-column (rest b) i (+ j 1))
	   (moves-column (rest b)
			 (append (make-list j
					    (find-zero (first b))) i)
			 (+ j 1)))))
  (moves-column b i j)))

;; find which player to go, 1 go first as default
(define (which-to-go b)
 (let ((steps (total-step b)))
  (if (= 0 (modulo steps 2))
      1
      -1)))
(define (total-step b)
 (reduce +
	 (map (lambda (x) (reduce + x 0)) b) 0))

;;change the index in the list with value

(define (list-set lst idx val)
 (if (null? lst)
     lst
     (cons
      (if (zero? idx)
	  val
	  (car lst))
      (list-set (cdr lst) (- idx 1) val))))
;;2-d list set
(define (list-set-2d lst x y val)
 (let* ((ylist (list-ref lst y))
	(sety (list-set ylist x val))
	(setlst (list-set lst y sety)))
  setlst))

;;make move and return the next board
(define (make-move m b)
;;;;;;; (write m) (newline)
 (if (or (null? m) (not (list? m))) b
     (let* ((x (first m))
	 
	    (y (second m))
	    (p (which-to-go b)))
      (if (not (= 0 (list-ref (list-ref b y) x)))
	  b
	  (list-set-2d b x y p)))))
;;diagonal win
;;;diagnal from n*n to 0*0
(define (get-diag b n)
 (list-ref (list-ref b n) n))
(define (get-column b m)
 (map (lambda (x) (list-ref x m)) b))
(define (get-diag-2d b x y)
 (list-ref (list-ref b y) x))
(define (diag-list b n)
 (let ((m 0))
  (define (diag-list-sum b n m)
   (if (= 0 n) m
       (diag-list-sum b (- n 1) (+ (get-diag b (- n 1)) m))))
  (diag-list-sum b n m)))
;;;reverse diag
(define (diag-list-2d b n)
 (let ((m 0))
  (define (diag-list-sum b x y m)
   (if (= 0 y) m
       (diag-list-sum b (+ 1 x) (- y 1) 
		      (+ (get-diag-2d b x (- y 1)) m))))
  (diag-list-sum b 0 n m)))
;;;per row sum
(define (row-sum b)
 (let ((temp (map (lambda (x) (reduce + x 0)) b)))
  (if (member? (length b) temp)
      1
      (if (member? (- 0 (length b)) temp)
	  -1
	  0))))

;;;per column sum
(define (column-sum b)
 (define (column-sum-iter b n l)
  (if (= n (length b)) l
      (column-sum-iter b (+ n 1) (cons (get-column b n) l))))
 (let ((temp (column-sum-iter b 0 '())))
  (row-sum temp)))

;;;member?
(define (member? x list)
 (cond ((null? list) #f)
       ((equal? x (first list)) #t)
       (else (member? x (rest list)))))

(define (win b)
 (let* ((len (length b))
	(diag1 (diag-list b len))
	(diag2 (diag-list-2d b len))
	(rowsum (row-sum b))
	(colsum (column-sum b)))
  (cond ((or (= len diag1) (= (- 0 len) diag1))
	 (if (= len diag1) 1 -1))
	((or (= len diag2) (= (- 0 len) diag2))
	 (if (= len diag2) 1 -1))
	((or (= -1 rowsum) (= 1 rowsum))
	 rowsum)
	((or (= -1 colsum) (= 1 colsum))
	 colsum)
	(else 0))))



;;optimalmoves--minimax
;;best => (best-so-far, right move)
(define (maximize f l limit)
; (write "max") (newline)
 (define (loop best l)
;  (write best) (newline)
;  (write l) (newline)
  (let ((best-so-far (first best)))
   (cond ((>= best-so-far limit)
;	  (write "hh!") (newline)
	  (list 1 (second best)))
	 ((null? l)
;	  (write "Hello") (newline)
	  best)
	 (else
;	  (write l) (newline)
	  (let ((temp (f (first l) best-so-far)))
;	   (write "temp") (newline)
;	   (write temp) (newline)
	   (cond ((= temp best-so-far)
		  (loop (list temp (cons (first l) (second best)))
		     (rest l)))
		 ((> temp best-so-far)
		  ;(write temp) (newline)
		  (loop (list temp (list (first l))) (rest l)))
		 (else (loop best
		     (rest l)))))))))
 (loop (list (- 0 infinity) '()) l))

 (define (optimal-moves~ k b)
  (define (wstar b l k last i)
   ;; k from inf to 0, i from 0 to inf
   ;;last -> last movement
   ;; frst -> first movement

   (if (and (= 0 (win b)) (not (null? (moves b)))
	    (>= k 0))
       
       (let* ((lst (maximize (lambda (m lp)
			      (* (which-to-go b)
				 (first (wstar (make-move m b)
					       (* (which-to-go b) lp)
					       (- k 1) m (+ 1 i)))))
			     (moves b)
			     (* (which-to-go b) l)))
	      
	      (value (first lst))
	      (mv (second lst)))
;	   (write "board") (newline)
;	   (write b) (newline)
;	(write "t") (newline)
;	(write lst) (newline)
;	(write k) (newline)
;	(write i) (newline)
	
	(if (= k 0) (list 0 mv)
	    (list (* (which-to-go b) value) mv)))	
       (cond ((not (= 0 (win b)))
;	      (write "HIHIHIH1") (newline)
	      (list (win b) (list last)))
	     ((and (null? (moves b))
		     (= 0 (win b)))
;	      (write "HIHIHIH2") (newline)
	      (list 0 (list last)))
;	     (write "HIHIHIH3") (newline)
	       ((= k -1) (list 0 last)))))
  (second (wstar b -3 k '() 0)))

