# 1.1

```scheme
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; -8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; a
(define b (+ a 1)) ; b
(+ a b (* a b)) ; 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16
```

# 1.2

```scheme
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
```

# 1.3

```scheme
(define (sum-of-larger x y z)
  (cond ((> x y) (cond ((> z y) (+ x z))
                       (else (+ x y))))
        ((> x z) (+ x y))
        (else (+ y z))))
```

# 1.4

```scheme
(define (a-plus-abs-b a b) 
  ((if (> b 0) + -) a b))
; equals to
(define (a-plus-abs-b a b)
  (if (> b 0) (+ a b) (- a b)))
```

# 1.5

应用序会陷入递归调用中无法返回结果，正则序会返回0。

因为(p)为无限递归定义，一旦展开就会陷入递归调用，应用序一开始就展开了(p)，而正则序不会运行到展开(p)。

# 1.6

若求值为应用序，new-if调用时会先将then-clause和else-clause都求值，这会导致递归调用无限递归下去。

# 1.7

`good-enough?`的误差值固定为0.001，这会导致对于较小的数误差较大(如0.00001，其平方根为0.001，所求得值可能为真实值的两倍)，而对于较大的数又运算次数过多(其相对精度变大)。

新的`good-enough?`如下:

```scheme
(define (good-enough? guess old-guess x)
  (< (abs (- 1.0 (/ guess old-guess))) 0.001))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (sqrt x)
  (sqrt-iter 1.0 0.1 x))
```

# 1.8

```scheme
(define (cube-root x)
  (cube-root-iter 1.0 0.1 x))

(define (cube-root-iter guess old-guess x)
  (if (cube-good-enough? guess old-guess x)
      guess
      (cube-root-iter (cube-improve guess x) guess x)))

(define (cube-good-enough? guess old-guess x)
  (< (abs (- 1.0 (/ guess old-guess))) 0.001))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))
```

# 1.9

```scheme
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
; 递归过程
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
; 迭代过程
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
```

# 1.10

```scheme
(A 1 10) ; 1024
(A 2 4) ; 65536
(A 3 3) ; 65536

(f n) ; 2n
(g n) ; 2^n
(h n) ; 2^2^2...(共n个2)
```

# 1.11

```scheme
; 递归过程
(define (f n)
  (if (< n 3)
      n
      (+ (* 1 (f (- n 1))) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; 迭代过程
(define (f n)
  (if (< n 3)
      n
      (f-iter 2 1 0 n)))

(define (f-iter fn-1 fn-2 fn-3 n)
  (if (= n 2)
      fn-1
      (f-iter (+ (* 1 fn-1) (* 2 fn-2) (* 3 fn-3)) fn-1 fn-2 (- n 1))))
```

# 1.12

```scheme
(define (pascal-triangle n k)
  (cond ((or (< n 1) (< k 1) (< n k)) 0)
        ((or (= n 1) (= k 1) (= n k)) 1)
        (else (+ (pascal-triangle (- n 1) (- k 1)) (pascal-triangle (- n 1) k)))))
```

# 1.15

a) 5

b) logn

# 1.16

```scheme
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))
```

# 1.17

```scheme
(define (even? n)
  (= (remainder n 2) 0))

(define (half x)
  (/ x 2))

(define (double x)
  (+ x x))

(define (* a b)
  (cond ((= b 1) a)
        ((even? b) (* (double a) (half b)))
        (else (+ (* a (+ b -1)) a))))
```
