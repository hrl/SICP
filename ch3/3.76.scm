(define (smooth s)
  (define (iter s last-value)
    (if (stream-null? s)
        s
        (cons-stream (/ (+ (stream-car s) last-value) 2)
                     (iter (stream-cdr s)
                           (stream-car s)))))
  (iter s 0))

(define (make-zero-crossings input-stream)
  (let ((smoothed-stream (smooth input-stream)))
    (stream-map sign-change-detector
                smoothed-stream
                (cons-stream 0 smoothed-stream))))
