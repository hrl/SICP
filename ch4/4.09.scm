(define (while-condition exp)
  (cadr exp))

(define (while-body exp)
  (cddr exp))

(define (while->combination exp)
  `((lambda ()
     (define (while-loop)
       (if ,(while-condition exp)
           (begin
             ,@(while-body exp)
             (while-loop))))
     (while-loop))))
