(define (encode lis)
  (cond
    ((null? lis) '())
    ((null? (cdr lis)) (list (cons (car lis) 1)))
    ((equal? (car lis) (cadr lis)) 
     (let
        ((next (encode (cdr lis))))
        (cons (cons (car lis) (+ 1 (cdar next))) (cdr next))))
    (else
      (let
        ((next (encode (cdr lis))))
        (cons (cons (car lis) 1) next)))))

(print (encode '(a)))
(print (encode '(a a)))
(print (encode '(a a a)))
(print (encode '(a b)))

(print (encode '(a a a b b c d d d d d e)))
; ((a . 3) (b . 2) (c . 1) (d . 5) (e . 1))
