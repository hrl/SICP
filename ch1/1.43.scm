(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((< n 1) (lambda (x) x))
        (else (compose f (repeated f (- n 1))))))
