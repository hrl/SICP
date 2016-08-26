# 4.1

```scheme
(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (let ((right (list-of-values-left-to-right (rest-operands exps env))))
          (cons left right)))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-right-to-left (rest-operands exps) env)))
        (let ((left (eval (first-operand exps env))))
          (cons left right)))))
```

# 4.2

`(define x 3)`会先被`application?`匹配到

```scheme
(define (application? exp)
  (tagged-list? exp 'call))
```

# 4.3

```scheme
(define (_eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((procedure? (get 'syntax (exp-tag exp)))
         (get 'syntax (exp-tag)) exp env)
        ((application? exp)
         (_apply (_eval (operator exp) env)
                (list-of-values (oprands exp) env)))
        (else
         (error "Unknown expression type -- _EVAL" exp))))

(define (install-_eval-package)
  (put 'syntax 'quote eval-quoted)
  (put 'syntax 'set! eval-assignment)
  (put 'syntax 'define eval-definition)
  (put 'syntax 'if eval-if)
  (put 'syntax 'lambda eval-lambda)
  (put 'syntax 'begin eval-begin)
  (put 'syntax 'cond eval-cond)
  'done)
(install-_eval-package)
```

# 4.4

```scheme
(define (and->if and-clauses)
  (if (null? and-clauses)
      'true
      (make-if (car and-clauses)
               (and->if (cdr and-clauses))
               'false)))

(define (or->if or-clauses)
  (if (null? or-clauses)
      'false
      (make-if (car or-clauses)
               'true
               (or->if (cdr or-clauses)))))

(define (eval-and exp env)
  (eval (and->if (cdr exp)) env))

(define (eval-or exp env)
  (eval (or->if (cdr exp)) env))
```

# 4.5

```scheme
(define (cond-actions clause)
  (if (pair? (cdr clause))
      (if (eq? '=> (cadr clause))
          (cddr clause)
          (error "Invalid cond clause -- COND-ACTIONS" clause))
      (cdr clause)))
```

# 4.6

```scheme
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
```

# 4.7

```scheme
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
```

# 4.8

```scheme
(define (named-let? let-exp)
  (and (tagged-list? let-exp 'let)
       (symbol? (cadr let-exp))))

(define (let-var let-exp)
  (cadr let-exp))

(define (let-bindings let-exp)
  (caddr let-exp))

(define (let-binding-vars let-exp)
  (map car (let-bindings let-exp)))

(define (let-binding-values let-exp)
  (map cadr (let-bindings let-exp)))

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
```

# 4.9

```scheme
(define (while-condition exp)
  (cadr exp))

(define (while-body exp)
  (cddr exp))

(define (while->combination exp)
  `((lambda ()
     (define (while-loop)
       (if ,(while-condition exp)
           (begin
             ,@(while-body exp)
             (while-loop))))
     (while-loop))))
```

# 4.10

```scheme
(define (assignment? exp)
  (if (pair? exp)
      (if (pair? (cdr exp))
          (eq? (cadr exp) ':=)
          false)
      false))

(define (assignment-variable exp) (car exp))

(define (assignment-value exp) (caddr exp))
```

# 4.11

```scheme
(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (if (null? (cdr frame))
      (set-cdr! frame (list (cons var val)))
      (add-binding-to-frame! var val (cdr frame))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (cdar frame))
            (else
             (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (set-cdr! (car frame) val))
            (else
             (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (add-binding-to-frame! var val (first-frame env)))
            ((eq? var (caar frame))
             (set-cdr! (car frame) val))
            (else
             (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (extend-environment (list var) (list val) env)
        (scan (first-frame env))))
  (env-loop env))
```

# 4.12

```scheme
(define (frame-lookup-variable var frame)
  (cond ((null? frame)
         #f)
        ((eq? var (caar frame))
         frame)
        (else
         (frame-lookup-variable (cdr frame)))))

(define (frame-add-variable-value! var val frame)
  (let ((frame-var-val (frame-lookup-variable var frame)))
    (if (false? frame-var-val)
        (add-binding-to-frame! var val frame)
        (set-cdr! (car frame) val))))

(define (env-lookup-variable env var found not-found)
  (if (eq? env the-empty-environment)
      (not-found)
      (let ((frame-var-val (frame-lookup-variable var (first-frame env))))
        (if (false? frame-var-val)
            (env-loop (enclosing-environment env))
            (found frame-var-val)))))

(define (lookup-variable-value var env)
  (env-lookup-variable
   env
   var
   (lambda (frame) (car frame))
   (lambda () (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (env-lookup-variable
   env
   var
   (lambda (frame) (set-cdr! (car frame) val))
   (lambda () (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((frame-var-val (frame-lookup-variable var (first-frame env))))
    (if (false? frame-var-val)
        (add-binding-to-frame! var val frame)
        (set-cdr! (car frame) val))))
```

# 4.13

```scheme
(define (make-unbound! var env)
  (define (scan vars vals)
    (cond ((null? vars)
           #f)
          ((eq? (cadr vars) var)
           (set-cdr! vars (cddr vars))
           (set-cdr! vals (cddr vals)))
          (else
           (scan (cdr vars) (cdr vals)))))
  (let* ((frame (first-frame env))
         (vars (car frame))
         (vals (cdr frame)))
    (cond ((null? vars)
           #f)
          ((eq? (car vars) var)
           (set-car! frame (cdr vars))
           (set-cdr! frame (cdr vals)))
          (else
           (scan vars vals)))))
```
