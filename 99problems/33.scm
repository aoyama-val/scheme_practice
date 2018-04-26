(define (repeat-comb n lis)
  (cond
    ((= n 1) (map (lambda (x) (list x)) lis))
    (else (apply append (map
            (lambda (x) (map (lambda (y) (cons x y)) (repeat-comb (- n 1) (member x lis))))
            lis)))))

(print (repeat-comb 1 '(a b c d)))
(print (repeat-comb 2 '(a)))
(print (repeat-comb 2 '(a b)))


(print (repeat-comb 3 '(a b c d)))
; ((a a a) (a a b) (a a c) (a a d) (a b b) (a b c) (a b d) (a c c) (a c d) (a d d)
;  (b b b) (b b c) (b b d) (b c c) (b c d) (b d d) (c c c) (c c d) (c d d) (d d d))
