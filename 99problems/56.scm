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

(define (uint-add i1 i2)
  (cond
    ((null? i1) (values i2 #f))
    ((null? i2) (values i1 #f))
    (else
      (receive (next-sum next-carry) (uint-add (cdr i1) (cdr i2))
               (receive (s c) (full-adder (car i1) (car i2) next-carry)
                        (values
                          (cons s next-sum)
                          c))))))

(print (call-with-values (lambda () (uint-add '(#f #f #f #t) '(#f #f #t #t))) list))
; (#f #t #f #f)
; #f
(print (call-with-values (lambda () (uint-add '(#f #f #f #t) '(#t #t #t #t))) list))
; (#f #f #f #f)
; #t
