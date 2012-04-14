gosh> (list 'a 'b 'c)
(a b c)
gosh> (list (list 'george))
((george))
gosh> (cdr '((x1 x2) (y1 y2)))
((y1 y2))
gosh> (cadr '((x1 x2) (y1 y2)))
(y1 y2)
gosh> (pair? (car '(a short list)))
#f
gosh> (car '(a short list))
a
gosh> (memq 'red '((red shores) (blue socks)))
#f
gosh> (memq 'red '(red shoes blue socks))
(red shoes blue socks)
