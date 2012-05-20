; sec2.4.3で使うため先に写経
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))

; TODO 
