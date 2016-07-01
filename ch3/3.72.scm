(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

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
