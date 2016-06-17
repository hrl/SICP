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
