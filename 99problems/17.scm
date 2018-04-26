(define (set-of-list lis)
  (cond
    ((null? lis) '())
    ((member (car lis) (cdr lis)) (set-of-list (cdr lis)))
    (else (cons (car lis) (set-of-list (cdr lis))))))

(print (set-of-list '(a b c d e f a b c)))
; (d e f a b c)
