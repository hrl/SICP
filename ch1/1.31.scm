(define (identity x) x)

(define (inc x) (+ 1 x))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (term x)
    (cond ((even? x) (/ (+ 2 x) (+ 1 x)))
          (else (/ (+ 1 x) (+ 2 x)))))
  (* 4.0 (product term 1 inc n)))
