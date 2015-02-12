(define (increment n) (+ n 1))

(define (decrement n) (- n 1))

(define (plus m n)
 (if (zero? n)
     m
     (plus (increment m) (decrement n))))

(define (minus m n)
 (if (zero? n)
     m
     (minus (decrement m) (decrement n))))

(define (times m n)
 (if (zero? n)
     0
     (plus m (times m (decrement n)))))

(define (divide m n)
 (if (zero? m)
     0
     (increment (divide (minus m n) n))))
