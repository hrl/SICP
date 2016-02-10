(define (last-pair items)
  (cond ((null? items) ())
        ((null? (cdr items)) items)
        (else (last-pair (cdr items)))))
