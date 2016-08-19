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
