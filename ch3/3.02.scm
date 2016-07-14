(define (mark-monitored func)
  (define call-time 0)
  (define (monitored . args)
    (if (eq? (car args) 'how-many-calls?)
        call-time
        (begin (set! call-time (+ call-time 1))
               (apply func args))))
  monitored)
