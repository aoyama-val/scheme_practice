(define (delete x lis)
  (cond
    ((null? lis) '())
    ((equal? x (car lis)) (delete x (cdr lis)))
    (else (cons (car lis) (delete x (cdr lis))))))

