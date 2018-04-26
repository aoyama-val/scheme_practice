(define (take xs n)
  (let loop ((i 0) (lis xs) (ret '()))
    (cond
      ((null? lis) (reverse ret))
      ((= i n) (reverse ret))
      (else (loop (+ i 1) (cdr lis) (cons (car lis) ret))))))

(define (drop lis n)
  (cond ((null? lis) '())
        ((= n 0) lis)
        (else (drop (cdr lis) (- n 1)))))

(define (group lis n)
  (cond ((null? lis) '())
        (else (cons (take lis n) (group (drop lis n) n)))))

(print (group '(a b c d e f) 2))
;((a b) (c d) (e f))
(print (group '(a b c d e f) 3))
;((a b c) (d e f))
(print (group '(a b c d e f) 4))
;((a b c d) (e f))
