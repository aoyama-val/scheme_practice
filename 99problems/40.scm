(define (expand-pair pair)
  (iota (+ 1 (- (cdr pair) (car pair))) (car pair)))

(print (expand-pair '(1 . 3)))

(define (expand-num-list lis)
  (cond
    ((null? lis) '())
    ((pair? (car lis)) (append (expand-pair (car lis)) (expand-num-list (cdr lis))))
    (else (cons (car lis) (expand-num-list (cdr lis))))))

(print (expand-num-list '((1 . 3) 5 (7 . 8) 10)))
;(1 2 3 5 7 8 10)


;(print (expand-num-list '()))
;(print (expand-num-list '(1))
;(print (expand-num-list '((1 . 2)))
;(print (expand-num-list '(1 3))
;(print (expand-num-list '(1 3 5))
;(print (expand-num-list '(3 5))
;(print (expand-num-list '((1 . 3) 5))
;(print (expand-num-list '((1 . 3) 5 (7 . 8) 10))
;(print (expand-num-list '((1 . 5) (7 . 9)))
