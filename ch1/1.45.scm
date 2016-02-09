(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enouth? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enouth? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((< n 1) (lambda (x) x))
        (else (compose f (repeated f (- n 1))))))

(define (fast-expt b n)
  (cond ((= 0 n) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (damp-time n)
  (/ (log n) (log 2)))

(define (root-n n x)
  (fixed-point ((repeated average-damp (damp-time n)) (lambda (y) (/ x (fast-expt y (- n 1)))))
               1.0))
