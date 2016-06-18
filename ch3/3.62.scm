(define (div-series num den)
  (let ((den-const (stream-car den)))
    (if (= den-const 0)
        (error "Invalid den -- DIV-SERIES" b)
        (mul-series num
                    (scale-stream (inv-series (scale-stream den
                                                            (/ 1 den-const)))
                                  den-const)))))
