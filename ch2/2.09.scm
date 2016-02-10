(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval))
     2))

; (width-interval (add-interval x y))
; =>
; (width-interval (make-interval (+ (lower-bound x) (lower-bound y))
;                                (+ (upper-bound x) (upper-bound y))))
; (< (lower-bound i) (upper-bound i))
; (< (+ (lower-bound i) (lower-bound j))
;    (+ (upper-bound i) (upper-bound j)))
; =>
; (/ (- (+ (upper-bound x) (upper-bound y))
;       (+ (lower-bound x) (lower-bound y)))
;    2)
; =>
; (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;    (/ (- (upper-bound y) (lower-bound x)) 2))
; =>
; (+ (width-interval x)
;    (width-interval y))
