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

(define (uint-inc i1)
  (uint-add i1 '(#f #f #f #t)))

(receive (s c) (uint-inc '(#f #f #f #f))
         (print s)
         (print c))
; (#f #f #f #t)
; #f
(receive (s c) (uint-inc '(#t #t #t #t))
         (print s)
         (print c))
; (#f #f #f #f)
; #t
