(define (or-gate a1 a2 output)
  (let ((wire-inv-a1 (make-wire))
        (wire-inv-a2 (make-wire))
        (wire-and-out (make-wire)))
    (inverter a1 wire-inv-a1)
    (inverter a2 wire-inv-a2)
    (and-gate wire-inv-a1 wire-inv-a2 wire-and-out)
    (inverter wire-and-out output)
    'ok))