(use srfi-1)

(define (bool-and a b)
  (if (and a b)
    #t
    #f))

(define (bool-or a b)
  (if (or a b)
    #t
    #f))

(define (bool-xor p q)
  (or
    (and (not p) q)
    (and p (not q))))

(define (uint-and i1 i2)
  (map (lambda (x) (apply bool-and x)) (zip i1 i2)))

(define (uint-or i1 i2)
  (map (lambda (x) (apply bool-or x)) (zip i1 i2)))

(define (uint-xor i1 i2)
  (map (lambda (x) (apply bool-xor x)) (zip i1 i2)))

(define (uint-not i1)
  (map not i1))

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

(define (uint-neg x)
  (uint-inc (uint-not x)))

(define (uint-sub x y)
  (receive (s c) (uint-add x (uint-neg y))
           (values
             s
             (not c))))

; 14 - 5 = 9
(receive (s c) (uint-sub '(#t #t #t #f) '(#f #t #f #t))
         (print s)
         (print c))
; (#t #f #f #t)
; #f

; 14 - 15 = -1
(receive (s c) (uint-sub '(#t #t #t #f) '(#t #t #t #t))
         (print s)
         (print c))
; (#t #t #t #t)
; #t

; 15 - 15 = 0
(receive (s c) (uint-sub '(#t #t #t #t) '(#t #t #t #t))
         (print s)
         (print c))
; (#f #f #f #f)
; #f
