(define (get-record org employee-name)
  ((get org 'get-record) employee-name))

(define (get-salary org employee-record)
  ((get org 'get-salary) employee-record))

(define (find-employee-record orgs employee-name)
  (if (null? orgs)
      '()
      (let ((record (get-record (car orgs) employee-name))
            (rest (find-employee-record (cdr orgs) employee-name)))
        (if (null? record)
            rest
            (cons record rest)))))
