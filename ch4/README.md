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
         (apply (_eval (operator exp) env)
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
