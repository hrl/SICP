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
