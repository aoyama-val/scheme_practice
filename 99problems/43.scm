(define (any? pred lis)
  (cond
    ((null? lis) #f)
    ((pred (car lis)) #t)
    (else (any? pred (cdr lis)))))

(define (every? pred lis)
  (cond
    ((null? lis) #t)
    ((pred (car lis)) (every? pred (cdr lis)))
    (else #f)))

(print (any? even? '(1 3 5 7 9)))
; #f
(print (any? even? '(1 3 4 5 7 9)))
; #t
(print (every? even? '(2 4 6 8 10)))
; #t
(print (every? even? '(2 4 6 5 8 10)))
; #f
