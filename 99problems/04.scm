(define (last lis)
  (list (car (reverse lis))))

(print (last '(a b c)))
(print (last '(a)))

(define (butlast lis)
  (reverse (cdr (reverse lis))))

(print (butlast '(a b c)))
(print (butlast '(a)))
