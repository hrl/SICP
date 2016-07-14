(define (foo n)
  (define (f m)
    (set! n (* n m))
    n)
  f)

(define f (foo 1))
