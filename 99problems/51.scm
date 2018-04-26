(define (xor p q)
  (or
    (and (not p) q)
    (and p (not q))))

(print (xor #f #f))
; #f
(print (xor #f #t))
; #t
(print (xor #t #f))
; #t
(print (xor #t #t))
; #f
