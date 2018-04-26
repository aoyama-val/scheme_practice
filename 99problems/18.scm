(define (union a b)
  (cond
    ((null? a) b)
    ((member (car a) b) (union (cdr a) b))
    (else (cons (car a) (union (cdr a) b)))))


(print (union '(a b c d) '(c d e f)))
; (a b c d e f)
