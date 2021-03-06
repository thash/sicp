;; http://wqzhang.wordpress.com/2010/04/28/sicp-exercise-4-44/
;; コピペ. エイトクイーン

(define (safe? positions)
  (define (two-queens-safe? q1 q2)
    (let ((row1 (car q1))
          (col1 (cadr q1))
          (row2 (car q2))
          (col2 (cadr q2)))
      (and (not (= row1 row2))
           (not (= (- col2 col1)
                   (abs (- row2 row1)))))))
  (let ((new-queen (list (last positions) (length positions))))
    (define (check col positions)
      (cond ((null? (cdr positions)) true)
            ((two-queens-safe? (list (car positions) col)
                               new-queen)
             (check (+ col 1) (cdr positions)))
            (else false)))
    (check 1 positions)))

(define (eight-queens)
  (let ((q1 (amb 1 2 3 4 5 6 7 8)))
    (let ((q2 (amb 1 2 3 4 5 6 7 8)))
      (require (safe? (list q1 q2)))
      (let ((q3 (amb 1 2 3 4 5 6 7 8)))
        (require (safe? (list q1 q2 q3)))
        (let ((q4 (amb 1 2 3 4 5 6 7 8)))
          (require (safe? (list q1 q2 q3 q4)))
          (let ((q5 (amb 1 2 3 4 5 6 7 8)))
            (require (safe? (list q1 q2 q3 q4 q5)))
            (let ((q6 (amb 1 2 3 4 5 6 7 8)))
              (require (safe? (list q1 q2 q3 q4 q5 q6)))
              (let ((q7 (amb 1 2 3 4 5 6 7 8)))
                (require
                 (safe? (list q1 q2 q3 q4 q5 q6 q7)))
                (let ((q8 (amb 1 2 3 4 5 6 7 8)))
                  (let ((queens (list q1 q2 q3 q4 q5 q6 q7 q8)))
                    (require (safe? queens))
                    queens))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 対角線にない
;; requireに失敗したらbacktrackする
;; an-integer-between が使える
;; n-queenとして一般化することもできるよ


