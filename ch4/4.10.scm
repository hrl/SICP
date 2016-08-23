(define (assignment? exp)
  (if (pair? exp)
      (if (pair? (cdr exp))
          (eq? (cadr exp) ':=)
          false)
      false))

(define (assignment-variable exp) (car exp))

(define (assignment-value exp) (caddr exp))
