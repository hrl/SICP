(define (gcd a b) (apply-generic 'gcd a b))

(define (install-polynomial-package)
  ; ...
  (define (gcd-poly a b)
    (if (same-variable? (variable a) (variable b))
        (make-poly (variable a)
                   (gcd-terms (term-list a)
                              (term-list b)))
        (error "require same variable -- GCD-POLY" (list a b))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  (define (remainder-terms a b)
    (div-rest (div-terms a b)))
  ; ...
  (put 'gcd '(polynomial polynomial)
       (lambda (a b) (tag (gcd-poly a b))))
  ; ...
  )
