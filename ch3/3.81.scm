(define (rand requests)
  (define random-init 100)
  (define (random-update number)
    (+ (* number 2) 33))
  (define (random-updater number request)
    (cond ((eq? (car request) 'generate)
           (random-update number))
          ((eq? (car request) 'reset)
           (cdr request))
          (else
           (error "Unknown request -- RAND" request))))
  (define random-numbers
    (cons-stream random-init
                 (stream-map random-updater random-numbers requests)))
  (stream-cdr random-numbers))
