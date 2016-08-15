(define (and->if and-clauses)
  (if (null? and-clauses)
      'true
      (make-if (car and-clauses)
               (and->if (cdr and-clauses))
               'false)))

(define (or->if or-clauses)
  (if (null? or-clauses)
      'false
      (make-if (car or-clauses)
               'true
               (or->if (cdr or-clauses)))))

(define (eval-and exp env)
  (eval (and->if (cdr exp)) env))

(define (eval-or exp env)
  (eval (or->if (cdr exp)) env))
