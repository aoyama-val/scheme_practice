(define (prefix lis pref)
  (cond
    ((null? lis) #f)
    ((null? pref) #t)
    ((equal? (car lis) (car pref)) (prefix (cdr lis) (cdr pref)))
    (else #f)))

(print (prefix '(a b c d e f) '(a b c)))
; #t
(print (prefix '(a b c d e f) '(a b c e)))
; #f
(print (prefix '(a b c d e f) '()))
; #t
