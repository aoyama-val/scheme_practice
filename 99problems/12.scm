(define (sum-list lis)
  (cond ((null? lis) 0)
        (else (+ (car lis) (sum-list (cdr lis))))))

(print (sum-list '(1 2 3 4 5)))
; 15
