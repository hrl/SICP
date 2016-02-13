(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (is-mobile? structure)
  (pair? structure))

; a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; b)
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (is-mobile? structure)
        (total-weight structure)
        structure)))

; c)
(define (balanced? mobile)
  (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

; d)
; (define (right-branch mobile) (cdr mobile))
; (define (branch-structure branch) (cdr branch))
