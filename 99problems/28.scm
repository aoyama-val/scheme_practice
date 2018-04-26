(define (subst a b tree)
  (cond
    ((null? tree) '())
    ((pair? tree)
     (cons (subst a b (car tree))
           (subst a b (cdr tree))))
    (else (if (equal? tree a)
            b
            tree))))



(print (subst 'a 'x '(a (b (a (c . a) d) a) e)))
; (x (b (x (c . x) d) x) e)
