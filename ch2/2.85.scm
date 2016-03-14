(define (equ? x y) (apply-generic 'equ? x y))

(define (get-lower-level-name type)
  (let ((type-level (get 'level type)))
    (if (= (get 'level 'min) type-level)
        #f
        (get 'level-reverse (- type-level 1)))))

(define (project datum)
  (let ((lower-level-name (get-lower-level-name (type-tag datum))))
    (if (eq? lower-level-name #f)
        datum
        (apply-generic 'project datum lower-level-name))))

(define (drop datum)
  (let (projected-datum (project datum))
    (if (equ? datum (raise projected-datum))
        (drop projected-datum)
        datum)))

(define (install-scheme-number-package)
  ; ...
  (define (equ? x y)
    (= x y))
  (define (project-keep x . _)
    x)
  ; ...
  (put 'project '(scheme-number scheme-number) project-keep)
  (put 'equ? '(scheme-number scheme-number) equ?)
  ; ...
  )

(define (install-rational-package)
  ; ...
  (define (project-scheme-number x . _)
    (/ (numer x) (denom x)))
  (define (equ? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  ; ...
  (put 'project '(rational scheme-number) project-scheme-number)
  (put 'equ? '(rational rational) equ?)
  ; ...
  )

(define (install-complex-package)
  ; ...
  (define (project-rational x . _)
    (real-part x))
  (define (equ? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  ; ...
  (put 'project '(complex rational) project-rational)
  (put 'equ? '(complex complex) equ?)
  ; ...
  )

(define (install-level-package)
  (put 'level 'min 0)
  (put 'level 'scheme-number 0)
  (put 'level-reverse 0 'scheme-number)
  (put 'level 'rational 1)
  (put 'level-reverse 1 'rational)
  (put 'level 'complex 2)
  (put 'level-reverse 2 'complex)
  (put 'level 'max 2)
  'done)
