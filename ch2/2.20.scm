(define (reverse items)
  (define (iter result next)
    (cond ((null? next) result)
          (else (iter (cons (car next) result) (cdr next)))))
  (if (null? items)
      ()
      (iter (cons (car items) ()) (cdr items))))

(define (same-parity x . items)
  (define filter
    (lambda (i) (= (remainder x 2) (remainder i 2))))
  (define (iter result next)
    (if (null? next)
        result
        (iter (if (filter (car next))
                  (cons (car next) result)
                  result)
              (cdr next))))
  (reverse (iter (list x) items)))
