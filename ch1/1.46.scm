(define (iterative-improve good-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (try next))))
  (lambda (first-guess) (try first-guess)))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  ((iterative-improve (lambda (guess next) (< (abs (- guess next)) tolerance))
                     (lambda (guess) (f guess)))
   first-guess))

(define (sqrt n)
  (define tolerance 0.001)
  ((iterative-improve (lambda (guess next) (< (abs (- 1.0 (/ guess next))) tolerance))
                      (lambda (guess) (/ (+ guess (/ n guess)) 2)))
   1.0))
