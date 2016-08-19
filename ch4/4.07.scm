(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (make-let parameters body)
  (cons 'let (cons parameters body)))

(define (let*->nested-lets exp)
  (car
   (accumulate
    (lambda (paramater body) (list (make-let (list paramater) body)))
    (list (make-let () (let-body exp)))
    (let-parameters exp))))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))
