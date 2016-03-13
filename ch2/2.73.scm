(define (install-sum-package)
  (define (addend operand)
    (car operand))
  (define (augend operand)
    (cadr operand))
  (define (deriv-sum operand var)
    (make-sum (deriv (addend operand) var)
              (deriv (augend operand) var)))
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '(+) deriv-sum)
  (put 'tag '+ tag)
  'done)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else ((get 'tag '+) (list a1 a2)))))

(define (install-product-package)
  (define (multiplier operand)
    (car operand))
  (define (multiplicand operand)
    (cadr operand))
  (define (deriv-product operand var)
    (make-sum
     (make-product (multiplier operand)
                   (deriv (multiplicand operand) ver))
     (make-product (deriv (multiplier operand) var)
                   (multiplicand operand))))
  (define (tag x) (attach-tag '* x))
  (put 'deriv '(*) deriv-product)
  (put 'tag '* tag)
  'done)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else ((get 'tag '*) (list m1 m2)))))