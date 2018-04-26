(define (difference a b)
  (cond
    ((null? a) '())
    ((member (car a) b) (difference (cdr a) b))
    (else (cons (car a) (difference (cdr a) b)))))


(print (difference '(a b c d) '(c d e f)))
; (a b)
