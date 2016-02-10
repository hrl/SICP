(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment point-begin point-end)
  (cons point-begin point-end))

(define (begin-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (begin-segment segment))
                    (x-point (end-segment segment)))
                 2)
              (/ (+ (y-point (begin-segment segment))
                    (y-point (end-segment segment)))
                 2)))

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")"))
