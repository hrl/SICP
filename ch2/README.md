# 2.1

```scheme
(define (make-rat n d)
  (let ((g ((if (< d 0) - +)(gcd n d))))
    (cons (/ n g) (/ d g))))
```

# 2.2

```scheme
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
```

# 2.3

```scheme
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
```

# 2.4

```scheme
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

; (car (cons x y))
; (car (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) p))
; ((lambda (p q) p) x y)
; x
```

# 2.5

```scheme
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (count-divisor n divisor)
  (define (iter result next)
    (cond ((= (remainder next divisor) 0) (iter (+ result 1) (/ next divisor)))
          (else result)))
  (iter 0 n))

(define (car z)
  (count-divisor z 2))

(define (cdr z)
  (count-divisor z 3))
```
