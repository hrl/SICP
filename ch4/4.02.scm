(define (application? exp)
  (tagged-list? exp 'call))
