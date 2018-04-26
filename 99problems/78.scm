(define (power-set lis)
  (cond
    ((null? lis) '(()))
    (else
      (apply append (map (lambda (s) (list s (cons (car lis) s))) (power-set (cdr lis)))))))

(print (power-set '()))
(print (power-set '(a)))
(print (power-set '(a b)))
(print (power-set '(a b c)))
