(define (shift-map f l1 l2 shift pad)
  (if (= shift 0)
      (map f l1 l2)
      (shift-map f l1 (cons pad l2) (- shift 1))))

(define (adjoirn-term term term-list)
  (let ((depth-term (length term))
        (depth-terms (length term-list)))
    (let ((depth-shift (- depth-terms depth-term)))
      (cond
       ((> depth-shift 0)
        (shift-map add term-list term depth-shift 0))
       (else
        (shift-map add term term-list (- 0 depth-shift) 0))))))

(define (make-term order coeff)
  (if (< order 0)
      '()
      (cons coeff (make-term (- order 1) coeff))))

(define (order term) (length (cdr term)))
(define (coeff term) (car term))
