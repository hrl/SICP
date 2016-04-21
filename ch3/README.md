# 3.1
```scheme
(define (make-accumulator value)
  (define (accumulator acc-value)
    (set! value (+ value acc-value))
    value)
  accumulator)
```

# 3.2
```scheme
(define (mark-monitored func)
  (define call-time 0)
  (define (monitored . args)
    (if (eq? (car args) 'how-many-calls?)
        call-time
        (begin (set! call-time (+ call-time 1))
               (apply func args))))
  monitored)
```

# 3.3
```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch password-input m)
    (if (eq? password password-input)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) "Incorrect password")))
  dispatch)
```

# 3.4
```scheme
(define (call-the-cops) (error "BiBiBi"))
(define (make-account balance password)
  (define retry-time 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch password-input m)
    (if (eq? password password-input)
        (begin (set! retry-time 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT"
                                  m))))
        (cond ((>= retry-time 7) (call-the-cops))
              (else (begin (set! retry-time (+ retry-time 1))
                           (lambda (x) "Incorrect password"))))))
  dispatch)
```

# 3.5
```scheme
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((size (* (- x2 x1) (- y2 y1))))
    (* size (monte-carlo trials experiment))))
```

# 3.6
```scheme
(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'set-init)
             (lambda (new-x)
               (set! x new-x)
               x))
            (else
             (error "Unknown action -- RAND"
                    m))))
    dispatch))
```

# 3.7
```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch password-input m)
    (if (eq? password password-input)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'check-password) #t)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x)
          (display "Incorrect password")
          #f)))
  dispatch)

(define (make-joint account original-password additional-password)
  (cond ((eq? #t (account original-password 'check-password))
         (lambda (password-input m)
           (if (eq? password-input additional-password)
               (account original-password m)
               (lambda (x)
                 (display "Incorrect password")
                 #f))))
        (else
         (display "Incorrect password")
         #f)))
```

# 3.8
```scheme
(define (foo n)
  (define (f m)
    (set! n (* n m))
    n)
  f)

(define f (foo 1))
```

# 3.11

acc的局部状态保存在新的局部环境中，包含`balance`，`withdraw`，`deposit`，`dispatch`。

新账户的局部环境是全新的，共享`make-account`。

# 3.12

(b)，(b c d)

# 3.13

无法返回值。

# 3.14

v为只含原始v第一个元素的列表，w为原始v的逆置。

# 3.15

```text
z1 -> [ . . ]
        | |
        v v
      [ . .-]--> [ . / ]
        |          |
        v          v
       wow         b

z2 -> [ . .-]--> [ . .-]--> [ . / ]
        |          |          |
        |          v          v
        |          a          b
        |                     ^
        |         wow         |
        |          ^          |
        |          |          |
        -------> [ . .-]--> [ . / ]
```

# 3.16

```scheme
(define return-3 '(a b c))

(define return-4 '(a (a)))
(set-car! return-4 (cadr return-4))

(define return-7 '(a b c))
(set-car! return-7 (cdr return-7))
(set-car! (cdr return-7) (cddr return-7))

(define return-inf '(a b c))
(set-cdr! (cddr return-inf) return-inf)
```

# 3.17

```scheme
(define (count-pairs x)
  (define visited-set '())
  (define (rec x)
    (cond ((not (pair? x))
           0)
          ((element-of-set? x visited-set)
           0)
          (else
           (set! visited-set
                 (adjoin-set x visited-set))
           (+ (rec (car x))
              (rec (cdr x))
              1))))
  (rec x))
```

# 3.18
```scheme
(define (has-loop? x)
  (define visited-set '())
  (define (rec x)
    (cond ((not (pair? x))
           #f)
          ((element-of-set? x visited-set)
           #t)
          (else
           (set! visited-set
                 (adjoin-set x visited-set))
           (or (rec (car x))
               (rec (cdr x))))))
  (rec x))
```

# 3.19

```scheme
(define (has-loop? x)
  (define (chase slow fast)
    (cond ((null? slow)
           #f)
          ((null? fast)
           #f)
          ((null? (cdr fast))
           #f)
          ((eq? (car slow) (car fast))
           #t)
          (else
           (chase (cdr slow) (cddr fast)))))
  (if (null? x)
      #f
      (chase x (cdr x))))
```

# 3.21

```scheme
(define (print-queue q)
  (display (front-ptr q)))
```
