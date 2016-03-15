(define (=zero? x) (apply-generic '=zero? x))

(define (install-polynomial-package)
  ; ...
  (define (=zero? x)
    (define (rec terms)
      (if (empty-termlist? terms)
          #t
          (and (=zero? (coeff (first-term terms)))
               (rec (rest-terms)))))
    (rec (term-list x)))
  ; ...
  (put '=zero? 'polynomial =zero?)
  ; ...
  )
