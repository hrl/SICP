(define (fringe items)
  (define (rec current next)
    (let ((next (if (pair? next) (rec (car next) (cdr next)) ())))
      (cond
       ((pair? current) (append (rec (car current) (cdr current)) next))
       ((null? current) next)
       (else (cons current next)))))
  (rec (car items) (cdr items)))
