;;;; http://wizardbook.wordpress.com/2010/12/16/exercise-3-24/
;;(define (make-table same-key?)
;;  (define local-table (list '*table*))
;;
;;  (define (assoc key records)
;;    (cond ((null? records) #f)
;;          ((same-key? key (caar records)) (car records))
;;          (else (assoc key (cdr records)))))
;;  (define (lookup key-1 key-2)
;;    (let ((subtable (assoc key-1 (cdr local-table))))
;;      (if subtable
;;        (let ((record (assoc key-2 (cdr subtable))))
;;          (if record
;;            (cdr record)
;;            #f))
;;        #f)))
;;
;;  (define (insert! key-1 key-2 value)
;;    (let ((subtable (assoc key-1 (cdr local-table))))
;;      (if subtable
;;        (let ((record (assoc key-2 (cdr subtable))))
;;          (if record
;;            (set-mcdr! record value)
;;            (set-mcdr! subtable
;;                       (cons (cons key-2 value)
;;                             (cdr subtable)))))
;;        (set-mcdr! local-table
;;                   (cons (list key-1
;;                               (cons key-2 value))
;;                         (cdr local-table)))))
;;    'ok)
;;
;;  (define (dispatch m)
;;    (cond ((eq? m 'lookup) lookup)
;;          ((eq? m 'insert!) insert!)
;;          (else (error "Unknown operation -- TABLE" m))))
;;
;;  dispatch)


;; http://wqzhang.wordpress.com/2009/07/20/sicp-exercise-3-24/
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))

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


