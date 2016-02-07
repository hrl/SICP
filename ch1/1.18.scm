(define (even? n)
  (= (remainder n 2) 0))

(define (half x)
  (/ x 2))

(define (double x)
  (+ x x))

(define (fast-* a b)
  (fast-*-iter 0 a b))

(define (fast-*-iter acc a b)
  (cond ((= b 1) (+ acc a))
        ((even? b) (fast-*-iter acc (double a) (half b)))
        (else (fast-*-iter (+ acc a) a (+ b -1)))))
