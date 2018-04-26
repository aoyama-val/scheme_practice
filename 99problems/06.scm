(define (drop lis n)
  (cond ((null? lis) '())
        ((= n 0) lis)
        (else (drop (cdr lis) (- n 1)))))

(print (drop '(a b c d e) 3))
;(d e)
(print (drop '(a b c d e) 0))
;(a b c d e)
(print (drop '(a b c d e) 6))
;()
