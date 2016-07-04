(define sense-data
  (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define (sign-change-detector current-value last-value)
  (cond ((and (< last-value 0) (>= current-value 0))
         1)
        ((and (>= last-value 0) (< current-value 0))
         -1)
        (else
         0)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))
