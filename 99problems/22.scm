
(define (merge-list comp a b)
  (sort (append a b) comp))                          ; w

(define (merge-sort comp a lis)
  (cond
    ((null? lis) '())
    ((comp a (car lis)) (merge-sort comp a (cdr lis)))
    (else (merge-list comp (list (car lis)) (merge-sort comp a (cdr lis))))))



(print (merge-sort < 9 '(5 6 4 7 8 3 2 9 1 10)))
; (1 2 3 4 5 6 7 8 9)
(print (merge-sort < 10 '(5 6 4 7 8 3 2 9 1 10)))
; (1 2 3 4 5 6 7 8 9 10)
(print (merge-sort < 11 '(5 6 4 7 8 3 2 9 1 10 0)))
; (0 1 2 3 4 5 6 7 8 9 10)
