(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

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
