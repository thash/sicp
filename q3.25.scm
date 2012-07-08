;; table次元の一般化
;; http://wqzhang.wordpress.com/2009/07/20/sicp-exercise-3-25/
(define (make-table)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (lookup1 keys table)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
            (if (null? (cdr keys))
              (cdr subtable)
              (lookup1 (cdr keys) subtable))
            #f)))
      (lookup1 key-list local-table))

    (define (insert! key-list value)
      (define (make-entry keys)
        (if (null? (cdr keys))
          (cons (car keys) value)
          (list (car keys) (make-entry (cdr keys)))))
      (define (insert1 keys table)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
            (if (null? (cdr keys))
              (set-cdr! subtable value)
              (insert1 (cdr keys) subtable))
            (set-cdr! table
                      (cons (make-entry keys)
                            (cdr table))))))
      (insert1 key-list local-table)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
  dispatch))


