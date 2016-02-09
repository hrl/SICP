(define (cube-root x)
  (cube-root-iter 1.0 0.1 x))

(define (cube-root-iter guess old-guess x)
  (if (cube-good-enough? guess old-guess x)
      guess
      (cube-root-iter (cube-improve guess x) guess x)))

(define (cube-good-enough? guess old-guess x)
  (< (abs (- 1.0 (/ guess old-guess))) 0.001))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))
