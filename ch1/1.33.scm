(define (identity x) x)

(define (inc x) (+ 1 x))

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter a)
                           (combiner result (term a))
                           result))))
  (iter a null-value))

(define (filtered-sum filter term a next b)
  (filtered-accumulate filter + 0 term a next b))

(define (filtered-product filter term a next b)
  (filtered-accumulate filter * 1 term a next b))

(define (sum-of-prime a b)
  (filtered-sum prime? identity a inc b))

(define (product-of-gcd n)
  (define (filter x)
    (= (gcd x n) 1))
  (filtered-product filter identity 1 inc n))
