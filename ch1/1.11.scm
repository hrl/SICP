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
