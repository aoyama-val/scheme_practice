(define (merge-list comp a b)
  (sort (append a b) comp))                          ; w

(print (merge-list < '(1 3 5 7) '(2 4 6 8)))
; (1 2 3 4 5 6 7 8)
