(define (c+ a b)
  (let ((c (make-connector)))
    (adder a b c)
    c))

(define (c* a b)
  (let ((c (make-connector)))
    (multiplier a b c)
    c))

(define (cv a)
  (let ((b (make-connector)))
    (constant a b)
    b))
