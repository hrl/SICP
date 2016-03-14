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

# 2.23

```scheme
(define (for-each f items)
  (cond ((null? items) #t)
        (else (f (car items)) (for-each f (cdr items)))))
```

# 2.24

```scheme
(list 1 (list 2 (list 3 4))) ; (1 (2 (3 4)))
```

# 2.25

```scheme
(lambda (items) (car (cdr (car (cdr (cdr items))))))
(lambda (items) (car (car items)))
(lambda (items) (cdr (cdr (cdr (cdr (cdr (cdr (cdr items))))))))
```

# 2.26

```scheme
(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))
```

# 2.27

```scheme
(define (deep-reverse items)
  (define (iter result next)
    (cond ((null? next) result)
          (else (iter (cons (deep-reverse (car next)) result) (cdr next)))))
  (if (pair? items)
      (iter (cons (car items) ()) (cdr items))
      items))
```

# 2.28

```scheme
(define (fringe items)
  (define (rec current next)
    (let ((next (if (pair? next) (rec (car next) (cdr next)) ())))
      (cond
       ((pair? current) (append (rec (car current) (cdr current)) next))
       ((null? current) next)
       (else (cons current next)))))
  (rec (car items) (cdr items)))
```

# 2.29

```scheme
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (is-mobile? structure)
  (pair? structure))

; a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; b)
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (is-mobile? structure)
        (total-weight structure)
        structure)))

; c)
(define (balanced? mobile)
  (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

; d)
; (define (right-branch mobile) (cdr mobile))
; (define (branch-structure branch) (cdr branch))
```

# 2.30

```scheme
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
```

# 2.31

```scheme
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))
```

# 2.32

```scheme
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (i) (cons (car s) i)) rest)))))
```

对非空集合S，i∈S，则对任一S的子集M，必有i∈M或i∉M。

# 2.33

```scheme
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
```

# 2.34

```scheme
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))
```

# 2.35

```scheme
(define (count-leaves t)
  (accumulate (lambda (leave rest-count) (+ rest-count 1)) 0 (map (lambda (x) x) (enumerate-tree t))))
```

# 2.36

```scheme
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
```

# 2.37

```scheme
(define (matrix-*-vector m v)
  (map (lambda (i) (dot-product i v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (i) (matrix-*-vector cols i)) m)))
```

# 2.38

```scheme
(fold-right / 1 (list 1 2 3)) ; 2/3
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list () (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list () (list 1 2 3)) ; (((() 1) 2) 3)
```

`(= (op a b) (op b a))`

`(= (op a (op b c)) (op (op a b) c))`

# 2.39

```scheme
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))
``` 

# 2.40

```scheme
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-prime-sum
       (filter prime-sum? (unique-pairs n))))
```

# 2.41

```scheme
(define (unique-triples n)
  (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                                 (enumerate-interval 1 (- j 1))))
                                (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triples-sum n s)
  (filter (lambda (triples) (= (+ (car triples) (cadr triples) (caddr triples)) s))
          (unique-triples n)))
```

# 2.42

```scheme
(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (define current-pos (car positions))
  (define (iter shift rest)
    (cond ((null? rest)
           #t)
          ((or (= (car rest) current-pos)
               (= (- (car rest) shift) current-pos)
               (= (+ (car rest) shift) current-pos))
           #f)
          (else
           (iter (+ 1 shift) (cdr rest)))))
  (iter 1 (cdr positions)))
```

# 2.43

`queen-cols`被移入内层后，从原来每层只计算1次变为了计算`board-size`次。当`board-size`为8时，耗时约为T^8

# 2.53

```scheme
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
((cdr '((x1 x2) (y1 y2)))) ; error
((cadr '((x1 x2) (y1 y2)))) ; error
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)
```

# 2.54

```scheme
(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))
```

# 2.55

```scheme
(car ''abracadabra)
(car (quote (quote abracadabra))) ; quote
```

# 2.56

```scheme
(define (make-exponentiation base exp)
  (cond ((or (=number? base 1) (=number? exp 0)) 1)
        ((=number? base 0) 0)
        ((and (number? base) (number? exp)) (expt base exp))
        (else (list 'expt base exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) 'expt)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))
```

# 2.57

```scheme
(define (augend s)
  (accumulate make-sum 0 (cddr s)))

(define (multiplicand p)
  (accumulate make-product 1 (cddr p)))
```

# 2.58

```scheme
; a)
; 改变一下位置即可 
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        ((equal? a1 a2) (list 2 '* a1))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

; b)
; 基于a，deriv前先对exp解析一次以补全括号
(define (parse-exp exp)
  (define (op< op1 op2)
    (cond ((and (eq? op1 '+) (eq? op2 '*)) #t)
          (else #f)))
  (define (parse-continue exp)
    (list (parse-exp (car exp)) (cadr exp) (parse-exp (cddr exp))))
  (define (add-quote-to-first-3-items exp)
    (cons (list (car exp) (cadr exp) (caddr exp)) (cdddr exp)))
  (cond ((not (pair? exp))
         exp)
        ((= (length exp) 1)
         (parse-exp (car exp)))
        ((= (length exp) 3)
         (parse-continue exp))
        ((op< (cadr exp) (cadddr exp))
         (parse-continue exp))
        (else
         (parse-exp (add-quote-to-first-3-items exp)))))
```

# 2.59

```scheme
(define (union-set set1 set2)
  (accumulate adjoin-set set1 set2))
```

# 2.60

```scheme
(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))
```

`adjoin-set`变为O(1), `union-set`变为O(n), 插入/求并次数多于删除/求交次数或是元素重复较少时较为适用

# 2.61

```scheme
(define (adjoin-set set x)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set (cdr set) x)))))
```

# 2.62

```scheme
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2)))))))))
```

# 2.63

a) 同样的结果

b) `tree->list-2`增长更慢

# 2.64

列表已经是有序状态，所以中心元素即根节点，左侧元素构成左子树，右侧元素构成右子树，`partial-tree`依据此递归构建平衡树

```text
   5
 1   9
x 3 7 11
```

O(n)

# 2.65

```scheme
; using union-set-list, intersection-set-list
(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list-2 set1)
                              (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list (tree->list-2 set1)
                                     (tree->list-2 set2))))
```

# 2.66

```scheme
(define (lookup given-key tree-of-records)
  (if (null? tree-of-records)
      #f
      (let ((current-entry (entry tree-of-records)))
        (cond ((= given-key (key current-entry))
               current-entry)
              ((< given-key (key current-entry))
               (lookup given-key (left-branch tree-of-records)))
              (else
               (lookup given-key (right-branch tree-of-records)))))))
```

# 2.67

```scheme
(decode sample-message sample-tree) ; (a d a b b c a)
```

# 2.68

```scheme
(define (symbol-in-branch? message-symbol branch)
  (define (elem-in-set? elem set)
    (cond ((null? set) #f)
          ((eq? elem (car set)) #t)
          (else (elem-in-set? elem (cdr set)))))
  (if (leaf? branch)
      (eq? message-symbol (symbol-leaf branch))
      (elem-in-set? message-symbol (symbols branch))))

(define (encode-symbol message-symbol tree)
  (let ((l-branch (left-branch tree))
        (r-branch (right-branch tree)))
    (cond ((symbol-in-branch? message-symbol l-branch)
           (if (leaf? l-branch)
               (list 0)
               (cons 0 (encode-symbol message-symbol l-branch))))
          ((symbol-in-branch? message-symbol r-branch)
           (if (leaf? r-branch)
               (list 1)
               (cons 1 (encode-symbol message-symbol r-branch))))
          (else
           (error "bad symbol -- ENCODE-SYMBOL" message-symbol)))))
```

# 2.69

```scheme
(define (successive-merge leaf-pairs)
  (cond ((= (length leaf-pairs) 2)
         (make-code-tree (car leaf-pairs)
                         (cadr leaf-pairs)))
        (else (successive-merge
               (adjoin-set (make-code-tree (car leaf-pairs)
                                           (cadr leaf-pairs))
                           (cddr leaf-pairs))))))
```

# 2.70

```scheme
'(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
```

84位，定长编码最少需108位

# 2.71

```text
       {31}
       / \
     {15} 16
     / \
   {7}  8
   / \
 {3}  4
 / \
1   2
```

最频繁1位，最不频繁2^(n-1)位

# 2.72

最频繁O(1)，最不频繁O(n^2)

# 2.73

a) 判断是否为基本数据(非操作符)，然后对基本数据直接做处理，对操作符取出相应的处理函数后做处理；  
&nbsp;&nbsp;&nbsp;`number?`与`same-variable`的对象为基本数据，而非操作符。

```scheme
; b) 
(define (install-sum-package)
  (define (addend operand)
    (car operand))
  (define (augend operand)
    (cadr operand))
  (define (deriv-sum operand var)
    (make-sum (deriv (addend operand) var)
              (deriv (augend operand) var)))
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '+ deriv-sum)
  (put 'tag '+ tag)
  'done)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else ((get 'tag '+) (list a1 a2)))))

(define (install-product-package)
  (define (multiplier operand)
    (car operand))
  (define (multiplicand operand)
    (cadr operand))
  (define (deriv-product operand var)
    (make-sum
     (make-product (multiplier operand)
                   (deriv (multiplicand operand) ver))
     (make-product (deriv (multiplier operand) var)
                   (multiplicand operand))))
  (define (tag x) (attach-tag '* x))
  (put 'deriv '* deriv-product)
  (put 'tag '* tag)
  'done)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else ((get 'tag '*) (list m1 m2)))))
```

# 2.74

```scheme
; a)
(define (get-record org employee-name)
  ((get org 'get-record) employee-name))

; b)
(define (get-salary org employee-record)
  ((get org 'get-salary) employee-record))

; c)
(define (find-employee-record orgs employee-name)
  (if (null? orgs)
      '()
      (let ((record (get-record (car orgs) employee-name))
            (rest (find-employee-record (cdr orgs) employee-name)))
        (if (null? record)
            rest
            (cons record rest)))))
```

d) 无需特殊修改，跟其他人事文件一样安装即可

# 2.75

```scheme
(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* mag (cos ang)))
          ((eq? op 'imag-part)
           (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
```

# 2.76

数据导向更适合经常需要加入新类型的系统；消息传递更适合经常需要加入新操作的系统。

# 2.77

未加入新代码前，`magnitude`过程不存在于`complex`包中，仅存在于其下的两个包中，故会引起错误。

加入后，`(magnitude z)`先进入`complex`包，再进去其下的某个具体实现的包，故`apply-generic`共调用了两次。

# 2.78

```scheme
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number) ; ?
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))
```

# 2.79

```scheme
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  ; ...
  (define (equ? x y)
    (= x y))
  ; ...
  (put 'equ? '(scheme-number scheme-number) equ?)
  ; ...
  )

(define (install-rational-package)
  ; ...
  (define (equ? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  ; ...
  (put 'equ? '(rational rational) equ?)
  ; ...
  )

(define (install-complex-package)
  ; ...
  (define (equ? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  ; ...
  (put 'equ? '(complex complex) equ?)
  ; ...
  )
```

# 2.80

```scheme
(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  ; ...
  (define (=zero? x)
    (= x 0))
  ; ...
  (put '=zero? '(scheme-number) =zero?)
  ; ...
  )

(define (install-rational-package)
  ; ...
  (define (=zero? x)
    (= (numer x) 0))
  ; ...
  (put '=zero? '(rational) =zero?)
  ; ...
  )

(define (install-complex-package)
  ; ...
  (define (=zero? x)
    (= (magnitude x) 0))
  ; ...
  (put '=zero? '(complex) =zero?)
  ; ...
  )
```

# 2.81

a) 陷入无限递归中。

b) 没有纠正，无法正常工作。

```scheme
; c)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
                   (not (eq? (car type-tags) (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond ((t1->t2)
                         (apply-generic op (t1->t2 a1) a2))
                        ((t2->t1)
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))
```

# 2.82

```scheme
(define (args-coercion args)
  (define (get-coercions types type-to)
    (map (lambda (type)
           (if (eq? type type-to)
               (lambda (x) x)
               (get-coercion type type-to)))
         types))
  (define (iter rest)
    (if (null? rest)
        #f
        (let ((coercions (get-coercions args (car rest))))
          (if (apply and coercions)
              (map (lambda (f x) (f x)) coercions args)
              (iter (cdr rest))))))
  (iter args))
```

# 2.83

```scheme
(define (raise datum)
  (let ((type-level (get 'level (type-tag datum))))
    (if (= (get 'level 'max) type-level)
        datum
        (let ((upper-coercion (get-coercion
                               datum
                               (get 'level-reverse (+ type-level 1)))))
          (if upper-coercion
              (upper-coercion datum)
              (error "Unable to raise -- RAISE" datum))))))

(define (install-level-package)
  (put 'level 'scheme-number 0)
  (put 'level-reverse 0 'scheme-number)
  (put 'level 'rational 1)
  (put 'level-reverse 1 'rational)
  (put 'level 'complex 2)
  (put 'level-reverse 2 'complex)
  (put 'level 'max 2)
  'done)
```

# 2.84

```scheme
(define (raise-to-same-level a1 a2)
  (let ((level1 (get 'level (type-tag a1)))
        (level2 (get 'level (type-tag a2))))
    (cond ((or (null? level1) (null? level2))
           (error "No method for these types"
                  (list a1 a2)))
          ((= level1 level2)
           (list a1 a2))
          ((< level1 level2)
           (raise-to-same-level (raise a1) a2))
          ((> level1 level2)
           (raise-to-same-level a1 (raise a2))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
                   (not (eq? (car type-tags) (cadr type-tags))))
              (let ((a1-a2 (raise-to-same-level a1 a2)))
                (let ((a1 (car a1-a2))
                      (a2 (cade a1-a2)))
                  (apply-generic op (t1->t2 a1) a2))
              (error "No method for these types"
                     (list op type-tags)))))))
```
