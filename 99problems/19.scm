(define (intersection a b)
  (cond
    ((null? a) '())
    ((member (car a) b) (cons (car a) (intersection (cdr a) b)))
    (else (intersection (cdr a) b))))

(print (intersection '(a b c d) '(c d e f)))
; (c d)
