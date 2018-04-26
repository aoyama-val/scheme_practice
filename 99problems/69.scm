(define (rpn lis)
  (rpn-aux lis '()))

(define (rpn-aux lis stack)
  (cond
    ((null? lis) (car stack))
    ((equal? (car lis) '+) (rpn-aux (cdr lis) (cons (+ (cadr stack) (car stack)) (cddr stack))))
    ((equal? (car lis) '-) (rpn-aux (cdr lis) (cons (- (cadr stack) (car stack)) (cddr stack))))
    ((equal? (car lis) '*) (rpn-aux (cdr lis) (cons (* (cadr stack) (car stack)) (cddr stack))))
    ((equal? (car lis) '/) (rpn-aux (cdr lis) (cons (/ (cadr stack) (car stack)) (cddr stack))))
    ((number? (car lis)) (rpn-aux (cdr lis) (cons (car lis) stack)))
    (else
      (car lis))))

(print (rpn '(1 2 +)))

(print (rpn '(1 2 + 3 4 + *)))
; 21
(print (rpn '(1 2 + 3 4 - *)))
; -3
(print (rpn '(1 2 + 3 4 + 5 6 + * *)))
; 231
(print (rpn '(1 2 + 3 4 + 5 6 + * /)))
; 3/77
(print (rpn '(1 2 + 3 4 + * 5 6 + /)))
; 21/11
