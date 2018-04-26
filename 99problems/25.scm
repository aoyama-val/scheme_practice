(define (take xs n)
  (let loop ((i 0) (lis xs) (ret '()))
    (cond
      ((null? lis) (reverse ret))
      ((= i n) (reverse ret))
      (else (loop (+ i 1) (cdr lis) (cons (car lis) ret))))))

(define (sublist sub lis)
  (cond
    ((null? sub) #t)
    ((null? lis) #f)
    ((equal? (take lis (length sub)) sub) #t)
    (else (sublist sub (cdr lis)))))

(print (sublist '(c d e) '(a b c d e f)))
; #t
(print (sublist '(d e) '(a b c d e f)))
; #t
(print (sublist '(d e g) '(a b c d e f)))
; #f
(print (sublist '() '(a b c d e f)))
; #t
