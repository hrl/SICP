# 3.1
```scheme
(define (make-accumulator value)
  (define (accumulator acc-value)
    (set! value (+ value acc-value))
    value)
  accumulator)
```

# 3.2
```scheme
(define (mark-monitored func)
  (define call-time 0)
  (define (monitored . args)
    (if (eq? (car args) 'how-many-calls?)
        call-time
        (begin (set! call-time (+ call-time 1))
               (apply func args))))
  monitored)
```

# 3.3
```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch password-input m)
    (if (eq? password password-input)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) "Incorrect password")))
  dispatch)
```

# 3.4
```scheme
(define (call-the-cops) (error "BiBiBi"))
(define (make-account balance password)
  (define retry-time 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch password-input m)
    (if (eq? password password-input)
        (begin (set! retry-time 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request -- MAKE-ACCOUNT"
                                  m))))
        (cond ((>= retry-time 7) (call-the-cops))
              (else (begin (set! retry-time (+ retry-time 1))
                           (lambda (x) "Incorrect password"))))))
  dispatch)
```

# 3.5
```scheme
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((size (* (- x2 x1) (- y2 y1))))
    (* size (monte-carlo trials experiment))))
```

# 3.6
```scheme
(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'set-init)
             (lambda (new-x)
               (set! x new-x)
               x))
            (else
             (error "Unknown action -- RAND"
                    m))))
    dispatch))
```

# 3.7
```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch password-input m)
    (if (eq? password password-input)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'check-password) #t)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x)
          (display "Incorrect password")
          #f)))
  dispatch)

(define (make-joint account original-password additional-password)
  (cond ((eq? #t (account original-password 'check-password))
         (lambda (password-input m)
           (if (eq? password-input additional-password)
               (account original-password m)
               (lambda (x)
                 (display "Incorrect password")
                 #f))))
        (else
         (display "Incorrect password")
         #f)))
```

# 3.8
```scheme
(define (foo n)
  (define (f m)
    (set! n (* n m))
    n)
  f)

(define f (foo 1))
```

# 3.11

acc的局部状态保存在新的局部环境中，包含`balance`，`withdraw`，`deposit`，`dispatch`。

新账户的局部环境是全新的，共享`make-account`。

# 3.12

(b)，(b c d)

# 3.13

无法返回值。

# 3.14

v为只含原始v第一个元素的列表，w为原始v的逆置。

# 3.15

```text
z1 -> [ . . ]
        | |
        v v
      [ . .-]--> [ . / ]
        |          |
        v          v
       wow         b

z2 -> [ . .-]--> [ . .-]--> [ . / ]
        |          |          |
        |          v          v
        |          a          b
        |                     ^
        |         wow         |
        |          ^          |
        |          |          |
        -------> [ . .-]--> [ . / ]
```

# 3.16

```scheme
(define return-3 '(a b c))

(define return-4 '(a (a)))
(set-car! return-4 (cadr return-4))

(define return-7 '(a b c))
(set-car! return-7 (cdr return-7))
(set-car! (cdr return-7) (cddr return-7))

(define return-inf '(a b c))
(set-cdr! (cddr return-inf) return-inf)
```

# 3.17

```scheme
(define (count-pairs x)
  (define visited-set '())
  (define (rec x)
    (cond ((not (pair? x))
           0)
          ((element-of-set? x visited-set)
           0)
          (else
           (set! visited-set
                 (adjoin-set x visited-set))
           (+ (rec (car x))
              (rec (cdr x))
              1))))
  (rec x))
```

# 3.18
```scheme
(define (has-loop? x)
  (define visited-set '())
  (define (rec x)
    (cond ((not (pair? x))
           #f)
          ((element-of-set? x visited-set)
           #t)
          (else
           (set! visited-set
                 (adjoin-set x visited-set))
           (or (rec (car x))
               (rec (cdr x))))))
  (rec x))
```

# 3.19

```scheme
(define (has-loop? x)
  (define (chase slow fast)
    (cond ((null? slow)
           #f)
          ((null? fast)
           #f)
          ((null? (cdr fast))
           #f)
          ((eq? (car slow) (car fast))
           #t)
          (else
           (chase (cdr slow) (cddr fast)))))
  (if (null? x)
      #f
      (chase x (cdr x))))
```

# 3.21

```scheme
(define (print-queue q)
  (display (front-ptr q)))
```

# 3.22

```scheme
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" dispatch)
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" dispatch))
            (else
             (set-front-ptr! (cdr front-ptr))
             dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "Undefined operation -- MAKR-QUEUE" m))))
    dispatch))
```

# 3.33

```scheme
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-deque?) (or (null? front-ptr) (null? rear-ptr)))
    (define (make-item data) (cons (cons data '()) '())) ; ((data prev-ptr) next-ptr)
    (define (set-item-prev-ptr! item prev-ptr) (set-cdr! (car item) prev-ptr))
    (define (set-item-next-ptr! item next-ptr) (set-cdr! item next-ptr))
    (define (get-item-prev-ptr item) (cdar item))
    (define (get-item-next-ptr item) (cdr item))
    (define (get-item-data item) (caar item))
    (define (front-deque)
      (if (empty-deque?)
          (error "GET-FRONT-DATA! called with an empty deque" dispatch)
          (get-item-data front-ptr)))
    (define (rear-deque)
      (if (empty-deque?)
          (error "GET-REAR-DATA! called with an empty deque" dispatch)
          (get-item-data rear-ptr)))
    (define (front-insert-deque! data)
      (let ((new-item (make-item data)))
        (set-item-next-ptr! new-item front-ptr)
        (cond ((empty-deque?)
               (set-front-ptr! new-item)
               (set-rear-ptr! new-item)
               dispatch)
              (else
               (set-item-prev-ptr! front-ptr new-item)
               (set-front-ptr! new-item)
               dispatch))))
    (define (rear-insert-deque! data)
      (let ((new-item (make-item data)))
        (set-item-prev-ptr! new-item rear-ptr)
        (cond ((empty-deque?)
               (set-front-ptr! new-item)
               (set-rear-ptr! new-item)
               dispatch)
              (else
               (set-item-next-ptr! rear-ptr new-item)
               (set-rear-ptr! new-item)
               dispatch))))
    (define (front-delete-deque!)
      (cond ((empty-deque?)
             (error "DELETE-FRONT! called with an empty deque" dispatch))
            (else
             (set-front-ptr! (cdr front-ptr))
             (if (empty-deque?)
                 (set-rear-ptr! '())
                 (set-item-prev-ptr! front-ptr '()))
             dispatch)))
    (define (rear-delete-deque!)
      (cond ((empty-deque?)
             (error "DELETE-REAR! called with an empty deque" dispatch))
            (else
             (set-rear-ptr! (get-item-prev-ptr rear-ptr))
             (if (empty-deque?)
                 (set-front-ptr! '())
                 (set-item-next-ptr! rear-ptr '()))
             dispatch)))
    (define (print-deque)
      (define (rec-print-item item)
        (if (not (null? item))
            (begin
              (display (get-item-data item))
              (display " ")
              (rec-print-item (get-item-next-ptr item)))))
      (display "( ")
      (rec-print-item front-ptr)
      (display ")"))
    (define (dispatch m)
      (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-deque?) empty-deque?)
            ((eq? m 'front-deque) front-deque)
            ((eq? m 'rear-deque) rear-deque)
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) front-delete-deque!)
            ((eq? m 'rear-delete-deque!) rear-delete-deque!)
            ((eq? m 'print) print-deque)
            (else (error "Undefined operation -- MAKR-DEQUE" m))))
    dispatch))
```

# 3.24

```scheme
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown opereation -- TABLE" m))))
    dispatch))
```

# 3.25

```scheme
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup keys)
      (if (null? keys)
          dispatch ; subtable
          (let ((kvpair (assoc (car keys) (cdr local-table)))
                (next-keys (cdr keys)))
            (if kvpair
                (if (null? next-keys)
                    (cdr kvpair)                            ; return current value
                    (if (procedure? (cdr kvpair))
                        (((cdr kvpair) 'lookup) next-keys)) ; search in subtable, need fix (value cannot be procedure)
                        #f)                                 ; search failed
                #f))))                                      ; search failed
    (define (insert! keys value)
      (if (null? keys)
          #f
          (let ((kvpair (assoc (car keys) (cdr local-table)))
                (key (car keys))
                (next-keys (cdr keys)))
            (if kvpair
                (if (null? next-keys)
                    (set-cdr! kvpair value)                         ; 1.  overwrite current value
                    (begin
                      (if (not (procedure? (cdr kvpair)))           ; need fix (value cannot be procedure)
                          (set-cdr! kvpair (make-table same-key?))) ; 2a. overwrite with a new table if current value is not table
                      (((cdr kvpair) 'insert!) next-keys value)))   ; 2b. insert value into subtable
                (if (null? next-keys)
                    (set-cdr! local-table                           ; 3.  insert value into current table
                              (cons (cons key value)
                                    (cdr local-table)))
                    (let ((new-table (make-table same-key?)))       ; 4.  inset a new subtable into current table
                      ((new-table 'insert!) next-keys value)        ;     then insert value into subtable
                      (set-cdr! local-table
                                (cons (cons key new-table)
                                      (cdr local-table)))))))))
    (define (print-table ident)
      (define (print-ident ident)
        (if (> ident 0)
            (begin
              (display " ")
              (print-ident (- ident 1)))))
      (define (print-kvpairs-with-ident kvpairs ident)
        (if (not (null? kvpairs))
            (begin
              (newline)
              (print-ident ident)
              (display (caar kvpairs))
              (display " . ")
              (if (procedure? (cdar kvpairs))
                  (((cdar kvpairs) 'print) (+ ident 2))
                  (display (cdar kvpairs)))
              (print-kvpairs-with-ident (cdr kvpairs) ident))))
      (define (print-table-with-ident ident)
        (display "(")
        (print-kvpairs-with-ident (cdr local-table) (+ ident 2))
        (newline)
        (print-ident ident)
        (display ")"))
      (print-table-with-ident ident))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print-table)
            (else (error "Unknown opereation -- TABLE" m))))
    dispatch))
```

# 3.26

```scheme
(define (make-table cmp)
  (define (make-tree entry-key entry-value left-branch right-branch)
    (define (lookup key)
      (let ((cmp-value (cmp key entry-key)))
        (cond ((= cmp-value 0)
               entry-value)
              ((< cmp-value 0)
               (if left-branch
                   ((left-branch 'lookup) key)
                   #f))
              ((> cmp-value 0)
               (if right-branch
                   ((right-branch 'lookup) key)
                   #f)))))
    (define (insert! key value)
      (let ((cmp-value (cmp key entry-key)))
        (cond ((= cmp-value 0)
               (set! entry-value value))
              ((< cmp-value 0)
               (if left-branch
                   ((left-branch 'insert!) key value)
                   (set! left-branch (make-tree key value #f #f))))
              ((> cmp-value 0)
               (if right-branch
                   ((right-branch 'insert!) key value)
                   (set! right-branch (make-tree key value #f #f)))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown opereation -- TREE" m))))
    dispatch)
  (let ((local-table #f))
    (define (lookup key)
      (if local-table
          ((local-table 'lookup) key)
          #f))
    (define (insert! key value)
      (if local-table
          ((local-table 'insert!) key value)
          (set! local-table (make-tree key value #f #f)))
      #t)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown opereation -- TABLE" m))))
    dispatch))
```

# 3.27

小于当前计算值的函数值可以直接查表得到，不需重复计算；不能。

# 3.28

```scheme
(define (logical-or a1 a2)
  (cond ((or (= a1 1) (= a2 1)) 1)
        ((and (= a1 0) (= a2 0)) 0)
        (else (error "Invalid signal" a1 a2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
```

# 3.29

```scheme
(define (or-gate a1 a2 output)
  (let ((wire-inv-a1 (make-wire))
        (wire-inv-a2 (make-wire))
        (wire-and-out (make-wire)))
    (inverter a1 wire-inv-a1)
    (inverter a2 wire-inv-a2)
    (and-gate wire-inv-a1 wire-inv-a2 wire-and-out)
    (inverter wire-and-out output)
    'ok))
```

总延时为`(+ inverter-delay and-gate-delay inverter-delay)`

# 3.30

```scheme
(define (ripple-carry-adder An Bn Cin)
  (let ((Sn '(*Sn*))
        (Cout '()))
    (define (iter A B C S)
      (if (or (null? A) (null? B))
          '()
          (let ((Ci (make-wire))
                (Si (make-wire)))
            (full-adder (car A) (car B) C Si Ci)
            (set-cdr! S (cons Si '()))
            (set! Cout Ci)
            (iter (cdr A) (cdr B) Ci (cdr S)))))
    (iter An Bn Cin Sn)
    (cons (cdr Sn) Cout)))
```

```scheme
(* n full-adder-delay)
(* n (+ or-gate-delay (* 2 half-adder-delay)))
(* n (+ or-gate-delay (* 2 (+ and-gate-delay (max or-gate-delay (+ and-gate-delay inverter-delay))))))
```

# 3.31

新加入的设备不会触发更新，导致后续电路全部无法工作。

# 3.32

input: (0,1) -> (1,1) -> (1,0) :  
action: ((set-output 1) (set-output 0))  
如果采用LIFO的常规表会导致与门输出结果为1，但正确结果为0，采用FIFO的队列才能保证执行顺序。

# 3.33

```scheme
(define (averager a b c)
  (let ((con-sum (make-connector))
        (con-const-2 (make-connector)))
    (constant 2 con-const-2)
    (adder a b con-sum)
    (multiplier con-const-2 c con-sum)
    'ok))
```

# 3.34

由a可以推出b，但反过来不行。对单纯的乘法器来说，知道积无法推出乘数与被乘数（乘法器不知道他们是相等的）。

# 3.35

```scheme
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (square (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknow request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)
```

# 3.37

```scheme
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
```

# 3.38

```text
a)
Peter->Paul->Mary: 45
Peter->Mary->Paul: 35
Paul->Peter->Mary: 45
Paul->Mary->Peter: 50
Mary->Paul->Peter: 40
Mary->Peter->Paul: 40

b)
Peter: 110
Paul: 80
Mary: 50
Peter->Paul: 90
Peter->Mary: 55
Paul->Peter: 90
Paul->Mary: 40
Mary->Peter: 60
Mary->Paul: 30
```

# 3.39

```text
Pa: (define tmp (* x x)
Pb: (set! x tmp)
Pc: (set! x (+ x 1))

Pa->Pb->Pc: 101
Pa->Pc->Pb: 100
Pc->Pa->Pb: 121
```

# 3.40

```text
a)
Pa1: (define xa1 x)
Pa2: (define xa2 x)
Pa3: (define xa3 (* xa1 xa2))
Pa4: (set! x xa3)

Pb1: (define xb1 x)
Pb2: (define xb2 x)
Pb3: (define xb3 x)
Pa4: (define xb4 (* xb1 xb2 xb3))
Pa5: (set! x xb4)

Pa1..4->Pb1..5: 1000000
Pb1..4->Pa1..5: 1000000
Pa1->Pb1..5->Pa2..4: 10000
Pa1..2->Pb1..5->Pa3..4: 100
Pb1->Pa1..4->Pb2..5: 100000
Pb1..2->Pa1..4->Pb3..5: 10000
Pb1..3->Pa1..4->Pb4..5: 1000

(list 100 1000 10000 100000 1000000)

b)
(list 1000000)
```

# 3.41

不能解决不可重复读的问题。

# 3.42

安全。

# 3.43

```text
a: 10
b: 20
c: 30

Pac1: (define diffac -20) ;; (- 10 30)
Pab1: (define diffab -10) ;; (- 10 20)
Pac2: ((a 'withdraw) diffac) ;; a: 30
Pab2: ((a 'withdraw) diffab) ;; a: 40
Pac3: ((c 'deposit) diffac) ;; c: 10
Pab3: ((b 'deposit) diffab) ;; b: 10
```

# 3.44

不是，交换需要保持两个账户的同步状态，转移不需要。

# 3.45

这样做在`serialized-exchange`中会产生重复嵌套的序列化对象，导致内层对象进入无法修改数据。

# 3.46

```text
P1a: (define tmp1 cell)
P2a: (define tmp2 cell)
P1b: (set! cell true)
P2b: (set! cell true)

导致P1 & P2 均获得互斥元
```

# 3.47

```scheme
(define (make-sempahore n)
  (let ((mutex (make-mutex))
        (count n))
    (define (the-sempahore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (cond ((> 0 count)
                    (set! count (- count 1))
                    (mutex 'release))
                   (else
                    (mutex 'release)
                    (the-sempahore 'acquire)))) ;; retry
            ((eq? m 'release)
             (mutex 'acquire)
             (set! count (+ count 1))
             (mutex 'release))
            (else
             (error "Unknown request -- MAKE-SEMPAHORE"))))
    the-sempahore))


(define (make-sempahore n)
  (let ((cell (cons #f '()))
        (count n))
    (define (the-sempahore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-sempahore 'acquire)
                 (cond ((> 0 count)
                        (set! count (- count 1))
                        (clear! cell))
                       (else
                        (clear! cell)
                        (the-sempahore 'acquire)))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-sempahore 'release)
                 (begin
                   (set! count (+ count 1))
                   (clear! cell))))
            (else
             (error "Unknown request -- MAKE-SEMPAHORE"))))
    the-sempahore))
```

# 3.48

死锁必须存在循环等待，对于多个进程同时需求的有序资源1、2、3、4...，各个进程均按顺序获取时不会出现循环等待的情况。

```scheme
(define (make-account balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'id) id)
            ((eq? m 'serializer) serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))))
  dispatch)


(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
    (if (< id1 id2)
        ((serializer2 (serializer1 (exchange)))
         account1
         account2)
        ((serializer1 (serializer2 (exchange)))
         account1
         account2))))
```

# 3.49

当无法在一开始就判明所需的全部资源或资源顺序时就可能再次死锁，如先获取锁读取某资源，再根据这一资源的信息获取下一资源时。

# 3.50

```scheme
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
```

# 3.51

```scheme
;; promise
(define x (stream-map show (stream-enumerate-interval 0 10)))
;
;0
;Value: x

(stream-ref x 5)
;
;1
;2
;3
;4
;5
;Value: 5

(stream-ref x 7)
;
;6
;7
;Value: 7

;; non-memozied stream
(define x (stream-map show (stream-enumerate-interval 0 10)))
;
;0
;Value: x

(stream-ref x 5)
;
;0
;1
;2
;3
;4
;5
;Value: 5

(stream-ref x 7)
;
;0
;1
;2
;3
;4
;5
;6
;7
;Value: 7
```

# 3.52

```scheme
;; promise
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;   1, (+ sum 1), seq: 1
(define y (stream-filter even? seq))
;   6, (+ sum 2 3), seq: 1->3->6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;  10, (+ sum 4), seq: 6->10
(stream-ref y 7)
; 136, (+ sum 5 6 7 ... 16)
(display-stream z)
; 210, (+ sum 17 18 19 20)

;; non-memozied stream
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;   1, (+ sum 1), seq: 1
(define y (stream-filter even? seq))
;   6, (+ sum 2 3), seq: 1->3->6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;  15, (+ sum 2 3 4), seq: 6->8->11->15
(stream-ref y 7)
; 134, (+ sum 2 3 4 ... 15), seq: 15->17->20->24->...->134
(display-stream z)
; 343, (+ sum 2 3 4 ... 20), seq: 134->139->143->...->343
```

# 3.53

```scheme
(1 2 4 8 ...)
```

# 3.54

```scheme
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))
```

# 3.55

```scheme
(define (partial-sums s)
  (define sums
    (cons-stream (stream-car s) (add-streams sums (stream-cdr s))))
  sums)
```

# 3.56

```scheme
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
```

# 3.57

只需n次(`promise`不会重复计算)，如果不用`memo-proc`优化会导致树形的`add-streams`展开。

# 3.58

```scheme
(expand 1 7 10) ;; 1 4 2 8 5 7 1 4 ....
(expand 3 8 10) ;; 3 7 5 0 0 ...
```

# 3.59

```scheme
;; a
(define (integrate-series s)
  (div-streams s integers))

;; b
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
```
 
# 3.60
 
 ```scheme
(define (mul-series a b)
  (cons-stream (* (stream-car a) (stream-car b))
               (add-streams (scale-stream (stream-cdr b) (stream-car a))
                            (mul-series (stream-cdr a) b))))

;; (define c (mul-series a b))
;; ->
;; (+     ;; c0 c1 c2 c3 c4 c5
;;  (* a0 (+ b0 b1 b2 b3 b4 b5 ...))
;;  (* a1 (+    b0 b1 b2 b3 b4 ...))
;;  (* a2 (+       b0 b1 b2 b3 ...))
;;  ...
;; )
 ```

# 3.61

```scheme
(define (inv-series s)
  (define x
    (cons-stream 1
                 (scale-stream (mul-series (stream-cdr s) x)
                               -1)))
  x)
```

# 3.62

```scheme
(define (div-series num den)
  (let ((den-const (stream-car den)))
    (if (= den-const 0)
        (error "Invalid den -- DIV-SERIES" b)
        (mul-series num
                    (scale-stream (inv-series (scale-stream den
                                                            (/ 1 den-const)))
                                  den-const)))))
```

# 3.63

`(sqrt-stream x)`会返回一个新的stream，导致无法充分利用到`memo-proc`的优化。无`memo-proc`时没有差异。

# 3.64

```scheme
(define (stream-limit s tolerance)
  (let ((s-car (stream-car s))
        (s-cdr (stream-cdr s)))
    (if (< (abs (- s-car (stream-car s-cdr))) tolerance)
        s-car
        (stream-limit s-cdr tolerance))))
```

# 3.65

```scheme
ln2-stream
;; 1
;; .5
;; .8333333333333333
;; .5833333333333333
;; .7833333333333332
;; .6166666666666666

ln2-euler-stream
;; .7
;; .6904761904761905
;; .6944444444444444
;; .6924242424242424
;; .6935897435897436
;; .6928571428571428

ln2-accelerated-euler-stream
;; 1
;; .7
;; .6932773109243697
;; .6931488693329254
;; .6931471960735491
;; .6931471806635636
```

# 3.66

`(1, 100)`前有(100-1-1)*2+1=197个序对，`(100, 100)`前有2^100-1-1个序对

# 3.67

```scheme
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) t))))
```

# 3.68

会陷入对`pairs`的无限递归调用中。

# 3.69

```scheme
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (phythagorean)
  (stream-filter (lambda (x) (= (+ (square (car x))
                                   (square (cadr x)))
                                (square (caddr x))))
                 (triples integers integers integers)))
```

# 3.70

```scheme
(define (merge-weighted weight i j)
  (cond ((stream-null? i) j)
        ((stream-null? j) i)
        (else
         (let ((i-car (stream-car i))
               (j-car (stream-car j)))
           (cond ((< (weight i-car) (weight j-car))
                  (cons-stream i-car (merge-weighted weight (stream-cdr i) j)))
                 (else
                  (cons-stream j-car (merge-weighted weight i (stream-cdr j)))))))))

(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

(define (integer-pairs-1)
  (weighted-pairs (lambda (p) (+ (car p) (cadr p)))
                  integers
                  integers))

(define (integer-pairs-2)
  (define integers-2-3-5
    (stream-filter (lambda (x) (or (= (remainder x 2) 0)
                                   (= (remainder x 3) 0)
                                   (= (remainder x 5) 0)))
                   integers))
  (weighted-pairs (lambda (p) (let ((i (car p))
                                    (j (cadr p)))
                                (+ (* 2 i) (* 3 j) (* 5 i j))))
                  integers-2-3-5
                  integers-2-3-5))
```

# 3.71

```scheme
(define (ramanujan)
  (define (cube x)
    (* x x x))
  (define (weight item)
    (let ((i (car item))
          (j (cadr item)))
      (+ (cube i) (cube j))))
  (define (filter)
    (define weight-last 0) ;; real weight could't be less than 2
    (define weight-skip 0) ;; same weight may occurs more than 2 times
    (lambda (item)
      (let ((weight-current (weight item)))
        (cond ((and (= weight-current weight-last) (not (= weight-current weight-skip)))
               (set! weight-last weight-current)
               (set! weight-skip weight-current)
               #t)
              (else
               (set! weight-last weight-current)
               #f)
              ))))
  (stream-map weight
              (stream-filter (filter)
                             (weighted-pairs weight
                                             integers
                                             integers))))

(ramanujan)
;; 1729
;; 4104
;; 13832
;; 20683
;; 32832
;; 39312
```

# 3.72

```scheme
(define (ramanujan-weight-n weight n)
  (define (filter weight-last items s)
    (if (stream-null? s)
        (if (>= (length items) n)
            (cons-stream (list weight-last items) s)
            s)
        (let* ((s-car (stream-car s))
               (s-cdr (stream-cdr s))
               (weight-car (weight s-car)))
          (if (= weight-car weight-last)
              (filter weight-last (cons s-car items) s-cdr)
              (if (>= (length items) n)
                  (cons-stream (list weight-last (cons s-car items)) (filter weight-car (cons s-car '()) s-cdr))
                  (filter weight-car (cons s-car '()) s-cdr))))))
  (filter 0
          '()
          (weighted-pairs weight
                          integers
                          integers)))

(define (ramanujan-square-3)
  (define (weight item)
    (let ((i (car item))
          (j (cadr item)))
      (+ (square i) (square j))))
  (ramanujan-weight-n weight 3))

(ramanujan-square-3)
;; (325 ((2 18) (1 18) (6 17) (10 15)))
;; (425 ((12 17) (5 20) (8 19) (13 16)))
;; (650 ((13 22) (5 25) (11 23) (17 19)))
;; (725 ((17 21) (7 26) (10 25) (14 23)))
;; (845 ((8 28) (2 29) (13 26) (19 22)))
```

# 3.73

```scheme
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (integral i v0 (/ dt C))
                 (scale-stream i R))))
```

# 3.74

```scheme
(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))
```

# 3.75

```scheme
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
```

# 3.76

```scheme
(define (smooth s)
  (define (iter s last-value)
    (if (stream-null? s)
        s
        (cons-stream (/ (+ (stream-car s) last-value) 2)
                     (iter (stream-cdr s)
                           (stream-car s)))))
  (iter s 0))

(define (make-zero-crossings input-stream)
  (let ((smoothed-stream (smooth input-stream)))
    (stream-map sign-change-detector
                smoothed-stream
                (cons-stream 0 smoothed-stream))))
```

# 3.77

```scheme
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                   the-empty-stream
                   (let ((integrand (force delayed-integrand)))
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))
```

# 3.78

```scheme
(define (slove-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy dy0 dt)))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)
```

# 3.79

```scheme
(define (slove-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy dy0 dt)))
  (define ddy (stream-map f dy y))
  y)
```

# 3.80

```scheme
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define iL (integral (delay diL) iL0 dt))
    (define vC (integral (delay dvC) vC0 dt))
    (define dvC (scale-stream iL (* -1 (/ -1 C))))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (* -1 (/ R L)))))
    (cons vC iL)))

(define rlc0 ((RLC 1 1 0.2 0.1) 10 0))
```

# 3.81

```scheme
(define (rand requests)
  (define random-init 100)
  (define (random-update number)
    (+ (* number 2) 33))
  (define (random-updater number request)
    (cond ((eq? (car request) 'generate)
           (random-update number))
          ((eq? (car request) 'reset)
           (cdr request))
          (else
           (error "Unknown request -- RAND" request))))
  (define random-numbers
    (cons-stream random-init
                 (stream-map random-updater random-numbers requests)))
  (stream-cdr random-numbers))
```

# 3.82

```scheme
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo experiment-stream)
  (define (next experiment-stream trials-passed trials-all)
    (cons-stream (/ trials-passed trials-all)
                 (cond ((stream-null? experiment-stream)
                        the-empty-stream)
                       ((stream-car experiment-stream)
                        (next (stream-cdr experiment-stream) (+ trials-passed 1) (+ trials-all 1)))
                       (else
                        (next (stream-cdr experiment-stream) (+ trials-passed 0) (+ trials-all 1))))))
  (next (stream-cdr experiment-stream) (if (stream-car experiment-stream) 1 0) 1))

(define (estimate-integral P x1 x2 y1 y2)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (define (experiment-stream)
    (cons-stream (experiment)
                 (experiment-stream)))
  (let ((size (* (- x2 x1) (- y2 y1))))
    (scale-stream (monte-carlo (experiment-stream)) size)))

(define (P x y)
  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))
```
