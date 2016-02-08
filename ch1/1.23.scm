(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next test-divisor)
  (cond ((= test-divisor 2) (+ 1 test-divisor))
        (else (+ 2 test-divisor))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start-number end-number)
  (cond ((<= start-number end-number)
         (cond ((even? start-number) (search-for-primes (+ 1 start-number) end-number))
               (else (timed-prime-test start-number) (search-for-primes (+ 2 start-number) end-number))))))
