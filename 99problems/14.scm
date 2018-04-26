(define (adjacent? x y lis)
  (cond
    ((null? lis) #f)
    ((and (not (null? (cdr lis))) (equal? (car lis) x) (equal? (cadr lis) y)) #t)
    (else (adjacent? x y (cdr lis)))))

(print (adjacent? 'a 'b '(a b c d e f)))
; #t
(print (adjacent? 'e 'f '(a b c d e f)))
; #t
(print (adjacent? 'f 'e '(a b c d e f)))
; #f
