(define (let-parameters let-exp)
  (cadr let-exp))

(define (let-parameter-vars let-exp)
  (map car (let-parameters let-exp)))

(define (let-parameter-values let-exp)
  (map cadr (let-parameters let-exp)))

(define (let-body let-exp)
  (cddr let-exp))

(define (let->combination let-exp)
  (cons (make-lambda (let-parameter-vars let-exp) (let-body let-exp))
        (let-parameter-values let-exp)))

(define (eval-let let-exp env)
  (eval (let->combination let-exp) env))
