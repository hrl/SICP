(define (lookup given-key tree-of-records)
  (if (null? tree-of-records)
      #f
      (let ((current-entry (entry tree-of-records)))
        (cond ((= given-key (key current-entry))
               current-entry)
              ((< given-key (key current-entry))
               (lookup given-key (left-branch tree-of-records)))
              (else
               (lookup given-key (right-branch tree-of-records)))))))
