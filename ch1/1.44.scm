(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((< n 1) (lambda (x) x))
        (else (compose f (repeated f (- n 1))))))

(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (smooth-n f n)
  ((repeated smooth n) f))
