(define (drop lis n)
  (cond ((null? lis) '())
        ((= n 0) lis)
        (else (drop (cdr lis) (- n 1)))))

(define (butlastn xs n)
  (reverse (drop (reverse xs) n)))

(print (butlastn '(a b c d e) 3))
;(a b)
(print (butlastn '(a b c d e) 0))
;(a b c d e)
(print (butlastn '(a b c d e) 5))
;()

