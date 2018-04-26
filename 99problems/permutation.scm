
(print (call-with-values (lambda () (delete-nth 0 '(a b c d e))) list))
(print (call-with-values (lambda () (delete-nth 1 '(a b c d e))) list))
(print (call-with-values (lambda () (delete-nth 2 '(a b c d e))) list))
(print (call-with-values (lambda () (delete-nth 3 '(a b c d e))) list))
(print (call-with-values (lambda () (delete-nth 4 '(a b c d e))) list))

(define (permutation lis)
  (define (delete-nth n lis)
    (let loop ((i 0) (lis lis) (forward '()))
      (cond
        ((equal? i n) (values (append (reverse forward) (cdr lis)) (car lis)))
        (else (loop (+ i 1) (cdr lis) (cons (car lis) forward))))))
  (if (null? lis)
    '(())
    (let loop ((i 0) (result '()))
      (cond
        ((= i (length lis)) result)
        (else
          (loop (+ i 1)
                (append result (receive (deleted nth) (delete-nth i lis) (map (lambda (x) (cons nth x)) (permutation deleted))))))))))

(print (permutation '()))
(print (permutation '(1)))
(print (permutation '(1 2)))
(print (permutation '(1 2 3)))
(print (permutation '(1 2 3 4)))
