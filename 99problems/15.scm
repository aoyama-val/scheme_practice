
(define (before? x y lis)
  (cond ((null? lis) #f)
        ((equal? x (car lis)) (member? y (cdr lis)))
        (else (before? x y (cdr lis)))))

(define (member? x lis)
  (cond ((null? lis) #f)
        ((equal? x (car lis)) #t)
        (else (member? x (cdr lis)))))

; x が y よりもあとにあるかどうか
;(define (after x y lis)



(print (before? 'a 'b '(a b c d e f)))
; (b c d e f)
(print (before? 'c 'b '(a b c d e f)))
; #f
