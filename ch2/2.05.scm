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
