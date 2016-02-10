(define (last-pair items)
  (cond ((null? items) nil)
        ((null? (cdr items)) items)
        (else (last-pair (cdr items)))))
