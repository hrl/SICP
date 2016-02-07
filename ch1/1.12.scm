(define (pascal-triangle n k)
  (cond ((or (< n 1) (< k 1) (< n k)) 0)
        ((or (= n 1) (= k 1) (= n k)) 1)
        (else (+ (pascal-triangle (- n 1) (- k 1)) (pascal-triangle (- n 1) k)))))
