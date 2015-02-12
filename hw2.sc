(define (set-union list1 list2)
 (reverse (set-union1 list1 list2)))
(define (set-union1 list1 list2)
  (let ((list3 (list)))
   (let ((list4 (union2 list1 list2)))
   (union1 list4 list3))))
(define (union1 list1 list2)
   (if (null? list1)
       list2
       (if (not (member? (first list1) list2))
	   (union1 (rest list1) (cons (first list1) list2))
	   (union1 (rest list1) list2))))
   
(define (union2 list1 list2)
 (if (null? list1)
    list2
    (if (not (member? (first list1) list2))
	(union2 (rest list1) (cons (first list1) list2))
	(union2 (rest list1) list2 ))))
       
(define (member? x list)
  (cond ((null? list) #f)
	((equal? x (first list)) #t)
	(else (member? x (rest list)))))
(define (set-intersection list1 list2)
 (reverse(set-intersection1 list1 list2)))
(define (set-intersection1 list1 list2)
 (let ((list3 (list)))
 (intersection list1 list2 list3)))

 (define (intersection list1 list2 list3)
  (if (null? list1)
    list3
    (if (and (member? (first list1) list2) (not (member? (first list1) list3)))
	(intersection (rest list1) list2 (cons (first list1) list3))
	(intersection (rest list1) list2 list3))))
(define (set-minus list1 list2)
 (reverse(set-minus1 list1 list2)))
(define (set-minus1 list1 list2)
 (if (not (equal? (set-intersection list1 list2) (set-intersection list2 list2)))
     (display "Error, Cannot be abstracted")
     (let ((list3 (list)))
     (set-minus-space list1 list2 list3))))
 
(define (set-minus-space list1 list2 list3)

 (if (null? list1)
     list3
     (if (and (not(member? (first list1) list2)) (not (member? (first list1) list3)))
	 (set-minus-space (rest list1) list2 (cons (first list1) list3))
	 (set-minus-space (rest list1) list2 list3))))

(define (reverse l)
 (if (null? l)
     (list)
     (append (reverse (rest l)) (list (first l)))))



