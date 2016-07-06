(define (smooth s)
  (define (iter s last-value)
    (if (stream-null? s)
        s
        (cons-stream (/ (+ (stream-car s) last-value) 2)
                     (iter (stream-cdr s)
                           (stream-car s)))))
  (iter s 0))
