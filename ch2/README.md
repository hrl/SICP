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

# 2.6

```scheme
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
; (add-1 zero)
; expand
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (inc n) (+ n 1))

(define (int church-number)
  ((church-number inc) 0))

(int (add one two)) ; 3
; expand
(int (lambda (f) (lambda (x) (f (f (f x))))))
(((lambda (f) (lambda (x) (f (f (f x))))) inc) 0)
((lambda (x) (inc (inc (inc x)))) 0)
(inc (inc (inc 0))) ; 3
```

# 2.7

```scheme
(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))
```

# 2.8

```scheme
(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))
```

# 2.9

```scheme
(define (width-interval interval)
  (/ (- (upper-bound interval) (lower-bound interval))
     2))

(width-interval (add-interval x y))
; =>
(width-interval (make-interval (+ (lower-bound x) (lower-bound y))
                               (+ (upper-bound x) (upper-bound y))))
; (< (lower-bound i) (upper-bound i))
; (< (+ (lower-bound i) (lower-bound j))
;    (+ (upper-bound i) (upper-bound j)))
; =>
(/ (- (+ (upper-bound x) (upper-bound y))
      (+ (lower-bound x) (lower-bound y)))
   2)
; =>
(+ (/ (- (upper-bound x) (lower-bound x)) 2)
   (/ (- (upper-bound y) (lower-bound x)) 2))
; =>
(+ (width-interval x)
   (width-interval y))
```

# 2.10

```scheme
(define (div-interval x y)
  (define (include-zero? interval)
    (or (= (lower-bound interval 0))
        (= (upper-bound interval 0))
        (and (< (lower-bound interval) 0) (> (upper-bound interval) 0))))
  (if (include-zero? y)
      (display "error")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
```

# 2.11

```scheme
(define (mul-interval x y)
  (define (sign-interval interval)
    (cond ((and (< (upper-bound interval) 0) (< (lower-bound interval) 0)) -1)
          ((and (> (upper-bound interval) 0) (> (lower-bound interval) 0)) 1)
          (else 0)))
  (define (abs-interval interval)
    (make-interval (abs (lower-bound interval))
                   (abs (upper-bound interval))))
  (let ((sign-x (sign-interval x))
        (sign-y (sign-interval y))
        (sign-xy (* (sign-interval x) (sign-interval y)))
        (abs-x (abs-interval x))
        (abs-y (abs-interval y)))
    (cond ((and (not (= sign-x 0)) (not (= sign-y 0)))
           (make-interval (* sign-xy (lower-bound abs-x) (lower-bound abs-y))
                          (* sign-xy (upper-bound abs-x) (upper-bound abs-y))))
          ((and (= sign-x 0) (not (= sign-y 0)))
           (make-interval (* sign-y (lower-bound x) (upper-bound abs-y))
                          (* sign-y (upper-bound x) (upper-bound abs-y))))
          ((and (not (= sign-x 0)) (= sign-y 0))
           (make-interval (* sign-x (lower-bound y) (upper-bound abs-x))
                          (* sign-x (upper-bound y) (upper-bound abs-x))))
          ((and (= sign-x 0) (= sign-y 0))
           (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                          (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))))))
```

# 2.12

```scheme
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent i)
  (* (/ (width i) (center i)) 100))
```

# 2.13

```text
a = (Ca - 0.5Wa, Ca + 0.5Wa)
b = (Cb - 0.5Wb, Cb + 0.5Wb)
a * b = ((CaCb + 0.25WaWb) - (0.5CaWb + 0.5 CbWa), ...)
Pab = 50(CaWb + CbWa) / (CaCb + 0.25WaWb)
CaCb >> WaWb
Pab = 50(Wb/Cb + Wa/Ca)
Pab = 0.5(Pa + Pb)
```

# 2.17

```scheme
(define (last-pair items)
  (cond ((null? items) ())
        ((null? (cdr items)) items)
        (else (last-pair (cdr items)))))
```

# 2.18

```scheme
(define (reverse items)
  (define (iter result next)
    (cond ((null? next) result)
          (else (iter (cons (car next) result) (cdr next)))))
  (if (null? items)
      ()
      (iter (cons (car items) ()) (cdr items))))
```

# 2.19

```scheme
(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))
```

# 2.20

```scheme
(define (same-parity x . items)
  (define filter
    (lambda (i) (= (remainder x 2) (remainder i 2))))
  (define (iter result next)
    (if (null? next)
        result
        (iter (if (filter (car next))
                  (cons (car next) result)
                  result)
              (cdr next))))
  (reverse (iter (list x) items)))
```

# 2.21

```scheme
(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x)) items))
```

# 2.22

迭代过程最后访问的元素会被置于结果表的头部，所以顺序与原来相反。

交换`cons`参数顺序不能解决这个问题，顺序表示要求最末访问元素在cons结构的最内层，而迭代过程中最末访问元素永远在最外层。
