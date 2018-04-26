(define (foldr f init lis)
  (cond
    ((null? lis) init)
    (else (f (car lis) (foldr f init (cdr lis))))))

(define (for-each-list fn comb term xs)
  (foldr comb term (map fn xs)))

(define (mymap f lis)
  (for-each-list f cons '() lis))

(define (myfilter f lis)
  (apply append (for-each-list (lambda (x) (if (f x) (list x) '())) cons '() lis)))

(define (myfold f init lis)
  (for-each-list (lambda (x) x) f init lis))

(print (mymap (lambda (x) (+ x 1)) '(1 2 3)))
(print (myfilter even? '(1 2 3 4 5 6)))
(print (myfold + 0 '(1 2 3)))

