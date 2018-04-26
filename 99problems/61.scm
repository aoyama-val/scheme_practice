(define (uint-equal? i1 i2)
  (cond
    ((and (null? i1) (null? i2)) #t)
    ((null? i1) #f)
    ((null? i2) #f)
    ((eqv? (car i1) (car i2)) (uint-equal? (cdr i1) (cdr i2)))
    (else
      #f)))

(define (uint-zero? i1)
  (cond
    ((null? i1) #t)
    ((car i1) #f)
    (else
      (uint-zero? (cdr i1)))))

(define (uint-greater? i1 i2)
  (cond
    ((and (null? i1) (null? i2)) #f)
    ((null? i1) #f)
    ((null? i2) #t)
    ((and (car i1) (car i2)) (uint-greater? (cdr i1) (cdr i2)))
    ((and (car i1) (not (car i2))) #t)
    ((and (not (car i1)) (car i2)) #f)
    (else
      (uint-greater? (cdr i1) (cdr i2)))))

(define (uint-less? i1 i2)
  (cond
    ((uint-equal? i1 i2) #f)
    ((uint-greater? i1 i2) #f)
    (else
      #t)))

(print (uint-equal? '(#t #t #t #t) '(#t #t #t #t)))
; #t
(print (uint-equal? '(#t #t #t #t) '(#t #t #f #t)))
; #f
(print (uint-zero? '(#t #t #t #t)))
; #f
(print (uint-zero? '(#f #f #f #f)))
; #t
(print (uint-greater? '(#f #f #f #t) '(#f #f #f #f)))
; #t
(print (uint-greater? '(#f #f #f #f) '(#f #f #f #f)))
; #f
(print (uint-greater? '(#f #f #f #f) '(#f #f #f #t)))
; #f
(print (uint-less? '(#f #f #f #f) '(#f #f #f #f)))
; #f
(print (uint-less? '(#f #f #f #t) '(#f #f #f #f)))
; #f
(print (uint-less? '(#f #f #f #f) '(#f #f #f #t)))
; #t
