(define (foldr f init lis)
  (cond
    ((null? lis) init)
    (else (f (car lis) (foldr f init (cdr lis))))))

;(print (foldr append '() '((a b) (c d e) () (f g))))
;(print (foldr + 0 '(1 2 3 4 5)))

(define (for-each-list fn comb term xs)
  (foldr comb term (map fn xs)))

(print (for-each-list (lambda (x) x) + 0 '(1 2 3 4 5)))
; 15
(print (for-each-list (lambda (x) (* x x)) + 0 '(1 2 3 4 5)))
; 55
(print (for-each-list (lambda (x) x) append '() '((a b) (c d e) () (f g))))
; (a b c d e f g)
