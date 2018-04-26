(define (my-append lis1 lis2)
  (cond ((null? lis1) lis2)
        (else (cons (car lis1) (my-append (cdr lis1) lis2)))))

(define (my-reverse lis)
  (cond ((null? lis) '())
        (else (my-append (my-reverse (cdr lis)) (list (car lis))))))

(display (my-append '(1 2) '(3 4))) (newline)

(display (my-reverse '())) (newline)
(display (my-reverse '(1))) (newline)
(display (my-reverse '(1 2))) (newline)
(display (my-reverse '(1 2 3 4))) (newline)
