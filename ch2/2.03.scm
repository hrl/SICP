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

; define rect using diagonal
(define (make-rect diagonal)
  diagonal)

(define (width-rect rect)
  (- (x-point (end-segment rect))
     (x-point (begin-segment rect))))

(define (height-rect rect)
  (- (y-point (end-segment rect))
     (y-point (begin-segment rect))))

; define rect using point and shift
(define (make-rect bottom-left-point width height)
  (cons bottom-left-point (cons width height)))

(define (width-rect rect)
  (car (cdr rect)))

(define (height-rect rect)
  (cdr (cdr rect)))

(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))

(define (perimeter-rect rect)
  (* 2 (+ (width-rect rect) (height-rect rect))))
