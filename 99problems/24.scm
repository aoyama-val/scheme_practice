(define (prefix lis pref)
  (cond
    ((null? lis) #f)
    ((null? pref) #t)
    ((equal? (car lis) (car pref)) (prefix (cdr lis) (cdr pref)))
    (else #f)))

(define (suffix lis suf)
  (prefix (reverse lis) (reverse suf)))

(print (suffix '(a b c d e f) '(d e f)))
; #t
(print (suffix '(a b c d e f) '()))
; #t
(print (suffix '(a b c d e f) '(f g)))
; #f
