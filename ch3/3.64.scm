(define (stream-limit s tolerance)
  (let ((s-car (stream-car s))
        (s-cdr (stream-cdr s)))
    (if (< (abs (- s-car (stream-car s-cdr))) tolerance)
        s-car
        (stream-limit s-cdr tolerance))))
