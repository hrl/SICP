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
