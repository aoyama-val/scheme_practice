(define (filter f lis a)
  (cond ((null? lis) '())
        ((f (car lis) a) (cons (car lis) (filter f (cdr lis) a)))
        (else (filter f (cdr lis) a))))

(display (filter < '(1 2 3 4 5 6) 5)) (newline)
(display (filter < '(3 1 4 1 5 9 2 6 5) 3)) (newline)

(define (qsort lis)
  (cond ((null? lis) '())
        ((= (length lis) 1) lis)
        (else (append (qsort (filter < lis (car lis))) (filter = lis (car lis)) (qsort (filter > lis (car lis)))))))

(display (qsort '(3 1 4 1 5 9 2 6 5))) (newline)
