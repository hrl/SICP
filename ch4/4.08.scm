(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-begin seq)
  (cons 'begin seq))

(define (named-let? let-exp)
  (and (tagged-list? let-exp 'let)
       (symbol? (cadr let-exp))))

(define (make-let parameters body)
  (cons 'let (cons parameters body)))

(define (let-var let-exp)
  (cadr let-exp))

(define (let-bindings let-exp)
  (caddr let-exp))

(define (let-binding-vars let-exp)
  (map car (let-bindings let-exp)))

(define (let-binding-values let-exp)
  (map cadr (let-bindings let-exp)))

(define (let-parameters let-exp)
  (cadr let-exp))

(define (let-parameter-vars let-exp)
  (map car (let-parameters let-exp)))

(define (let-parameter-values let-exp)
  (map cadr (let-parameters let-exp)))

(define (let-body let-exp)
  (if (named-let? let-exp)
      (cdddr let-exp)
      (cddr let-exp)))

(define (named-let->combination named-let-exp)
  (make-begin
   (list
    `(define ,(let-var named-let-exp)
       ,(make-lambda
         (let-binding-vars named-let-exp)
         (let-body named-let-exp)))
    `(,(let-var named-let-exp) ,@(let-binding-values named-let-exp)))))

(define (let->combination let-exp)
  (if (named-let? let-exp)
      (named-let->combination let-exp)
      (cons (make-lambda (let-parameter-vars let-exp) (let-body let-exp))
            (let-parameter-values let-exp))))

(define (eval-let let-exp env)
  (eval (let->combination let-exp) env))
