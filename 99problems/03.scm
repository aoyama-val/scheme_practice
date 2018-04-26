(define (longer? xs ys)
  (cond
    [(null? xs) #f]
    [(null? ys) #t]
    [else (longer? (cdr xs) (cdr ys))]))

(print (longer? '(a b c) '(a b)))
(print (longer? '(a b) '(a b)))
(print (longer? '(a) '(a b)))
