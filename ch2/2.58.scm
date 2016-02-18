(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        ((equal? a1 a2) (list 2 '* a1))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (deriv raw-exp var)
  (define (parse-exp exp)
    (define (op< op1 op2)
      (cond ((and (eq? op1 '+) (eq? op2 '*)) #t)
            (else #f)))
    (define (parse-continue exp)
      (list (parse-exp (car exp)) (cadr exp) (parse-exp (cddr exp))))
    (define (add-quote-to-first-3-items exp)
      (cons (list (car exp) (cadr exp) (caddr exp)) (cdddr exp)))
    (cond ((not (pair? exp))
           exp)
          ((= (length exp) 1)
           (parse-exp (car exp)))
          ((= (length exp) 3)
           (parse-continue exp))
          ((op< (cadr exp) (cadddr exp))
           (parse-continue exp))
          (else
           (parse-exp (add-quote-to-first-3-items exp)))))
  (define exp (parse-exp raw-exp))
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))
