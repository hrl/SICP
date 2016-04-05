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
