(define (make-sempahore n)
  (let ((mutex (make-mutex))
        (count n))
    (define (the-sempahore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (cond ((> 0 count)
                    (set! count (- count 1))
                    (mutex 'release))
                   (else
                    (mutex 'release)
                    (the-sempahore 'acquire)))) ;; retry
            ((eq? m 'release)
             (mutex 'acquire)
             (set! count (+ count 1))
             (mutex 'release))
            (else
             (error "Unknown request -- MAKE-SEMPAHORE"))))
    the-sempahore))


(define (make-sempahore n)
  (let ((cell (cons #f '()))
        (count n))
    (define (the-sempahore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-sempahore 'acquire)
                 (cond ((> 0 count)
                        (set! count (- count 1))
                        (clear! cell))
                       (else
                        (clear! cell)
                        (the-sempahore 'acquire)))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-sempahore 'release)
                 (begin
                   (set! count (+ count 1))
                   (clear! cell))))
            (else
             (error "Unknown request -- MAKE-SEMPAHORE"))))
    the-sempahore))
