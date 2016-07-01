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
