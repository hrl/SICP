(define (inv-series s)
  (define x
    (cons-stream 1
                 (scale-stream (mul-series (stream-cdr s) x)
                               -1)))
  x)
