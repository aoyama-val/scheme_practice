(define (single? lis)
  (if (null? lis)
    #f
    (null? (cdr lis))))

(print (single? '(a)))
(print (single? '(a b)))
(print (single? '()))
