(define (raise datum)
  (let ((type-level (get 'level (type-tag datum))))
    (if (= (get 'level 'max) type-level)
        datum
        (let ((upper-coercion (get-coercion
                               datum
                               (get 'level-reverse (+ type-level 1)))))
          (if upper-coercion
              (upper-coercion datum)
              (error "Unable to raise -- RAISE" datum))))))

(define (install-level-package)
  (put 'level 'scheme-number 0)
  (put 'level-reverse 0 'scheme-number)
  (put 'level 'rational 1)
  (put 'level-reverse 1 'rational)
  (put 'level 'complex 2)
  (put 'level-reverse 2 'complex)
  (put 'level 'max 2)
  'done)
