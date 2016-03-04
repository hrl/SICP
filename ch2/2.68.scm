(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-symbols '(A D A B B C A))

