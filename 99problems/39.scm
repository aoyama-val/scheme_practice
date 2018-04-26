(define (successor? a b)
  (= a (- b 1)))

(define (pack-num-list lis)
  (cond
    ((null? lis) '())
    ((null? (cdr lis)) (list (car lis)))
    ((successor? (car lis) (cadr lis))
     (let [(next (pack-num-list (cdr lis)))]
       (if (pair? (car next))
         (cons [cons (car lis) (cdar next)] (cdr next))
         (cons [cons (car lis) (car next)] (cdr next)))))
    (else (cons (car lis) (pack-num-list (cdr lis))))))

(print (pack-num-list '()))
(print (pack-num-list '(1)))
(print (pack-num-list '(1 2)))
(print (pack-num-list '(1 3)))
(print (pack-num-list '(1 3 5)))
(print (pack-num-list '(3 5)))
(print (pack-num-list '(1 2 3 5)))
(print (pack-num-list '(1 2 3 5 7 8 10)))
(print (pack-num-list '(1 2 3 4 5 7 8 9)))
; ((1 . 3) 5 (7 . 8) 10)
