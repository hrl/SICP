(define (even? n)
  (= (remainder n 2) 0))

(define (half x)
  (/ x 2))

(define (double x)
  (+ x x))

(define (* a b)
  (cond ((= b 1) a)
        ((even? b) (* (double a) (half b)))
        (else (+ (* a (+ b -1)) a))))
