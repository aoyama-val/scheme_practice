(define (count x lis)
  (cond ((null? lis) 0)
        ((equal? x (car lis)) (+ 1 (count x (cdr lis))))
        (else (count x (cdr lis)))))

(print (count 'a '(a b a b c a b c d)))
; 3
(print (count 'c '(a b a b c a b c d)))
; 2
(print (count 'd '(a b a b c a b c d)))
; 1
(print (count 'e '(a b a b c a b c d)))
; 0
