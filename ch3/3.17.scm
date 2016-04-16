(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

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
