(define (split-nth lis n)
  (cond
    ((null? lis) (values '() '()))
    (else (let loop ((i 0) (p1 '()) (rest lis))
      (cond
        ((= i n) (values p1 rest))
        (else (loop (+ i 1) (append p1 (list (car rest))) (cdr rest))))))))

(print (call-with-values (lambda () (split-nth '() 3)) list))

(print (call-with-values (lambda () (split-nth '(a b c d e f) 3)) list))
; (a b c)
; (d e f)
(print (call-with-values (lambda () (split-nth '(a b c d e f) 4)) list))
; (a b c d)
; (e f)
