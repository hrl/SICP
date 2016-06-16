(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (integrate-series s)
  (div-streams s integers))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 1 (integrate-series cosine-series)))
