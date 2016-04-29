(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup keys)
      (if (null? keys)
          dispatch ; subtable
          (let ((kvpair (assoc (car keys) (cdr local-table)))
                (next-keys (cdr keys)))
            (if kvpair
                (if (null? next-keys)
                    (cdr kvpair)                            ; return current value
                    (if (procedure? (cdr kvpair))
                        (((cdr kvpair) 'lookup) next-keys)) ; search in subtable, need fix (value cannot be procedure)
                        #f)                                 ; search failed
                #f))))                                      ; search failed
    (define (insert! keys value)
      (if (null? keys)
          #f
          (let ((kvpair (assoc (car keys) (cdr local-table)))
                (key (car keys))
                (next-keys (cdr keys)))
            (if kvpair
                (if (null? next-keys)
                    (set-cdr! kvpair value)                         ; 1.  overwrite current value
                    (begin
                      (if (not (procedure? (cdr kvpair)))           ; need fix (value cannot be procedure)
                          (set-cdr! kvpair (make-table same-key?))) ; 2a. overwrite with a new table if current value is not table
                      (((cdr kvpair) 'insert!) next-keys value)))   ; 2b. insert value into subtable
                (if (null? next-keys)
                    (set-cdr! local-table                           ; 3.  insert value into current table
                              (cons (cons key value)
                                    (cdr local-table)))
                    (let ((new-table (make-table same-key?)))       ; 4.  inset a new subtable into current table
                      ((new-table 'insert!) next-keys value)        ;     then insert value into subtable
                      (set-cdr! local-table
                                (cons (cons key new-table)
                                      (cdr local-table)))))))))
    (define (print-table ident)
      (define (print-ident ident)
        (if (> ident 0)
            (begin
              (display " ")
              (print-ident (- ident 1)))))
      (define (print-kvpairs-with-ident kvpairs ident)
        (if (not (null? kvpairs))
            (begin
              (newline)
              (print-ident ident)
              (display (caar kvpairs))
              (display " . ")
              (if (procedure? (cdar kvpairs))
                  (((cdar kvpairs) 'print) (+ ident 2))
                  (display (cdar kvpairs)))
              (print-kvpairs-with-ident (cdr kvpairs) ident))))
      (define (print-table-with-ident ident)
        (display "(")
        (print-kvpairs-with-ident (cdr local-table) (+ ident 2))
        (newline)
        (print-ident ident)
        (display ")"))
      (print-table-with-ident ident))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print-table)
            (else (error "Unknown opereation -- TABLE" m))))
    dispatch))
