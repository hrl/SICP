(define (make-accumulator value)
  (define (accumulator acc-value)
    (set! value (+ value acc-value))
    value)
  accumulator)
