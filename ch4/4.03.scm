(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define *syntax-table* (make-table))

(define get (*syntax-table* 'lookup-proc))
(define put (*syntax-table* 'insert-proc!))

(define (get-tag exp)
  (if (pair? exp)
      (car exp)
      '()))

(define (_eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((procedure? (get 'syntax (exp-tag exp)))
         (get 'syntax (exp-tag)) exp env)
        ((application? exp)
         (apply (_eval (operator exp) env)
                (list-of-values (oprands exp) env)))
        (else
         (error "Unknown expression type -- _EVAL" exp))))

(define (install-_eval-package)
  (put 'syntax 'quote eval-quoted)
  (put 'syntax 'set! eval-assignment)
  (put 'syntax 'define eval-definition)
  (put 'syntax 'if eval-if)
  (put 'syntax 'lambda eval-lambda)
  (put 'syntax 'begin eval-begin)
  (put 'syntax 'cond eval-cond)
  'done)
(install-_eval-package)
