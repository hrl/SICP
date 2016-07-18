(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (let ((right (list-of-values-left-to-right (rest-operands exps env))))
          (cons left right)))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-right-to-left (rest-operands exps) env)))
        (let ((left (eval (first-operand exps env))))
          (cons left right)))))