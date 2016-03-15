(define (sub x y) (apply-generic 'sub x y))
(define (neg x) (apply-generic 'neg x))

(define (install-polynomial-package)
  ; ...
  (define (neg-poly x)
    (define (neg-terms terms)
      (let ((term (first-term terms)))
        (adjoin-term (make-term (order term)
                                (neg (coeff term)))
                     (neg-terms (rest-terms terms)))))
    (make-poly (variable x)
               (neg-terms (term-list x)))
  (define (sub-poly x y)
    (add-poly x (neg-poly y)))
  ; ...
  (put 'sub '(polynomial polynomial)
       (lambda (x y) (tag (sub-poly x y))))
  (put 'neg '(polynomial)
       (lambda (x y) (tag (neg-poly x))))
  ; ...
  )
