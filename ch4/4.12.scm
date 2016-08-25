(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (if (null? (cdr frame))
      (set-cdr! frame (list (cons var val)))
      (add-binding-to-frame! var val (cdr frame))))

(define (frame-lookup-variable var frame)
  (cond ((null? frame)
         #f)
        ((eq? var (caar frame))
         frame)
        (else
         (frame-lookup-variable (cdr frame)))))

(define (frame-add-variable-value! var val frame)
  (let ((frame-var-val (frame-lookup-variable var frame)))
    (if (false? frame-var-val)
        (add-binding-to-frame! var val frame)
        (set-cdr! (car frame) val))))

(define (env-lookup-variable env var found not-found)
  (if (eq? env the-empty-environment)
      (not-found)
      (let ((frame-var-val (frame-lookup-variable var (first-frame env))))
        (if (false? frame-var-val)
            (env-loop (enclosing-environment env))
            (found frame-var-val)))))

(define (lookup-variable-value var env)
  (env-lookup-variable
   env
   var
   (lambda (frame) (car frame))
   (lambda () (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (env-lookup-variable
   env
   var
   (lambda (frame) (set-cdr! (car frame) val))
   (lambda () (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((frame-var-val (frame-lookup-variable var (first-frame env))))
    (if (false? frame-var-val)
        (add-binding-to-frame! var val frame)
        (set-cdr! (car frame) val))))
