(define (maplist f lis)
    (cond
      ((null? lis) '())
      (else (cons (f lis) (maplist f (cdr lis))))))

(define (ff x)
  (fold + 0 x))

(print (maplist (lambda (x) x) '(a b c d e)))
; ((a b c d e) (b c d e) (c d e) (d e) (e))#<undef>
(print (maplist ff '(1 2 3 4 5)))
; (15 14 12 9 5)
