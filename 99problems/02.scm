(define (single? lis)
  (if (null? lis)
    #f
    (null? (cdr lis))))

(define (double? lis)
  (if (null? lis)
    #f
    (single? (cdr lis))))

(print (double? '(a b)))
(print (double? '(a b c)))
(print (double? '(a)))
