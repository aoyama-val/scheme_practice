(define (xor p q)
  (or
    (and (not p) q)
    (and p (not q))))

(define (half-adder p q)
  (values
    (xor p q)
    (and p q)))

(define (full-adder p q r)
  (receive (s c) (half-adder p q)
           (values
             (xor s r)
             (or
               (and p q)
               (and r (xor p q))))))

(print (call-with-values (lambda () (full-adder #f #f #f)) list))
(print (call-with-values (lambda () (full-adder #f #t #f)) list))
(print (call-with-values (lambda () (full-adder #t #f #f)) list))
(print (call-with-values (lambda () (full-adder #t #t #f)) list))
(print (call-with-values (lambda () (full-adder #f #f #t)) list))
(print (call-with-values (lambda () (full-adder #f #t #t)) list))
(print (call-with-values (lambda () (full-adder #t #f #t)) list))
(print (call-with-values (lambda () (full-adder #t #t #t)) list))
