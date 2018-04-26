(define (expression lis)
  (parse-add lis))

(define (parse-number lis)
  (cond
    ((number? (car lis)) (values (car lis) (cdr lis)))
    ((list? (car lis)) (parse-add (car lis)))
    (else
      (error "parse-number error" (car lis)))))

(define (parse-mul lis)
  (let loop ((result (parse-number lis)) (lis (cdr lis)))
    (cond
      ((null? lis) (values result '()))
      ((equal? (car lis) '*)
       (loop (* result (parse-number (cdr lis))) (cddr lis)))
      ((equal? (car lis) '/)
       (loop (/ result (parse-number (cdr lis))) (cddr lis)))
      (else
        (values result lis)))))

(define (parse-add lis)
  (receive (t1 rest1) (parse-mul lis)
           (let loop ((result t1) (rest rest1))
             (cond
               ((null? rest) result)
               ((equal? (car rest) '+)
                (receive (term rest2) (parse-mul (cdr rest))
                         (loop (+ result term) rest2)))
               ((equal? (car rest) '-)
                (receive (term rest2) (parse-mul (cdr rest))
                         (loop (- result term) rest2)))
               (else
                 (error "parse-mul error" lis))))))

;(print (parse-number '((1 + 2))))
;(exit)
(print (parse-number '(123)))
(print (parse-mul '(2)))
(print (parse-mul '(2 * 3)))
(print (parse-mul '(2 * 3 * 4)))
(print (parse-add '(2 + 3)))
(print (parse-add '(2 + 3 * 4 * 5 + 6)))

(print (parse-add '(1 + 2 * 3 + 4)))
(print (expression '((1 + 2) * (3 + 4))))

(print (expression '((1 + 2) / (3 + 4))))
;(define a '((1 + 2) * (3 + 4)))
;(print (length a))
