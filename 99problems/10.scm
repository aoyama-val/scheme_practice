(define (position x lis)
  (cond ((null? lis) #f)
        ((equal? x (car lis)) 0)
        (else (and (position x (cdr lis)) (+ 1 (position x (cdr lis)))))))

(print (position 'a '(a b c d e)))
;0
(print (position 'c '(a b c d e)))
;2
(print (position 'e '(a b c d e)))
;4
(print (position 'f '(a b c d e)))
;#f
