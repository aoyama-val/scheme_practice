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


(print (uint-and '(#t #t #f #f) '(#t #f #t #f)))
; (#t #f #f #f)
(print (uint-or '(#t #t #f #f) '(#t #f #t #f)))
; (#t #t #t #f)
(print (uint-xor '(#t #t #f #f) '(#t #f #t #f)))
; (#f #t #t #f)
(print (uint-not '(#t #f #t #f)))
; (#f #t #f #t)
