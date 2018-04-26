(define (pack lis)
  (cond
    ((null? lis) '())
    ((null? (cdr lis)) (list (list (car lis))))
    ((equal? (car lis) (cadr lis)) (let [(next (pack (cdr lis)))] (cons (cons (car lis) (car next)) (cdr next))))
    (else (cons (list (car lis)) (pack (cdr lis))))))

(print (pack '()))
(print (pack '(a)))
(print (pack '(a a)))
(print (pack '(a b c)))

(print (pack '(a a a b b c c c c d e e e e e)))
; ((a a a) (b b) (c c c c) (d) (e e e e e))
