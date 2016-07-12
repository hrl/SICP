(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                   the-empty-stream
                   (let ((integrand (force delayed-integrand)))
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define iL (integral (delay diL) iL0 dt))
    (define vC (integral (delay dvC) vC0 dt))
    (define dvC (scale-stream iL (* -1 (/ -1 C))))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (* -1 (/ R L)))))
    (cons vC iL)))

(define rlc0 ((RLC 1 1 0.2 0.1) 10 0))
