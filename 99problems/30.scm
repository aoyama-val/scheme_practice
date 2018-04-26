(define (repeat-perm n lis)
  (cond ((= n 0) '())
        ((= n 1) (map (lambda (x) (list x)) lis))
        (else (apply append (map (lambda (x) (map (lambda (y) (cons x y)) (repeat-perm (- n 1) lis))) lis)))))

(print (repeat-perm 1 '(a)))
;((a))

(print (repeat-perm 1 '(a b c)))
;((a) (b) (c))

(print (repeat-perm 2 '(a)))
(print (repeat-perm 3 '(a)))

(print (repeat-perm 3 '(a b c)))
; ((a a a) (a a b) (a a c) (a b a) (a b b) (a b c) (a c a) (a c b) (a c c) (b a a)
;  (b a b) (b a c) (b b a) (b b b) (b b c) (b c a) (b c b) (b c c) (c a a) (c a b)
;  (c a c) (c b a) (c b b) (c b c) (c c a) (c c b) (c c c))
