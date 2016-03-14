(define (raise type)
  (let ((type-level (get 'level (type-tag type))))
    (if (= (get 'level 'max) type-level)
        type
        (let ((upper-coercion (get-coercion
                               type
                               (get 'level-reverse (+ type-level 1)))))
          (if upper-coercion
              (upper-coercion type)
              (error "Unable to raise -- RAISE" type))))))

(define (install-level-package)
  (put 'level 'scheme-number 0)
  (put 'level-reverse 0 'scheme-number)
  (put 'level 'rational 1)
  (put 'level-reverse 1 'rational)
  (put 'level 'complex 2)
  (put 'level-reverse 2 'complex)
  (put 'level 'max 2)
  'done)
