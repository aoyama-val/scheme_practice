(define (xor p q)
  (or
    (and (not p) q)
    (and p (not q))))

(define (half-adder p q)
  (values
    (xor p q)
    (and p q)))

(print (call-with-values (lambda () (half-adder #f #f)) list))
; #f
; #f
(print (call-with-values (lambda () (half-adder #f #t)) list))
; #t
; #f
(print (call-with-values (lambda () (half-adder #t #f)) list))
; #t
; #f
;(print-multi-values (half-adder #t #t))
(print (call-with-values (lambda () (half-adder #t #t)) list))
; #f
; #t
