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
