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