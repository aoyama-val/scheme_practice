; これはfoldr
; |x, s| x + s
; (my-reduce + 0 '(1 2 3))
; = 0 + (1 + (2 + 3))
(define (my-reduce f init lis)
  (cond ((null? lis) init)
        (else (f (car lis) (my-reduce f init (cdr lis))))))

(display (my-reduce + 0 '(1 2 3 4 5))) (newline)
(display (my-reduce * 1 '(1 2 3 4 5))) (newline)
(display (my-reduce cons '() '(1 2 3 4 5))) (newline)
(display (my-reduce (lambda (x s) (string-append (number->string x) s)) "." '(1 2 3 4 5))) (newline)

(define (foldl f init lis)
  (cond ((null? lis) init)
        (else (foldl f (f init (car lis)) (cdr lis)))))

(display (foldl + 0 '(1 2 3 4 5))) (newline)
(display (foldl * 1 '(1 2 3 4 5))) (newline)
(display (foldl cons '() '(1 2 3 4 5))) (newline)
(display (foldl (lambda (s x) (string-append s (number->string x))) "." '(1 2 3 4 5))) (newline)
