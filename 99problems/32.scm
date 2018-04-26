(define (combination n lis)
  (cond
    ((= n 1) (map (lambda (x) (list x)) lis))
    (else (apply append (map (lambda (x) (map (lambda (y) (cons x y)) (combination (- n 1) (cdr (member x lis))))) lis)))))

(print (combination 1 '(a b)))
(print (combination 2 '(a b c)))

(print (combination 3 '(a b c d e)))

; ((a b c) (a b d) (a b e) (a c d) (a c e) (a d e) (b c d) (b c e) (b d e) (c d e))#<undef>
