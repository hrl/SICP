(define (averager a b c)
  (let ((con-sum (make-connector))
        (con-const-2 (make-connector)))
    (constant 2 con-const-2)
    (adder a b con-sum)
    (multiplier con-const-2 c con-sum)
    'ok))
