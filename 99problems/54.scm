(define (int->uint n m)
  (cond
    ((= m 0) '())
    (else
      (append (int->uint (quotient n 2) (- m 1))
               (list (if (= (mod n 2) 0) #f #t))))))

(print (int->uint 0 8))
(print (int->uint 127 8))
(print (int->uint 128 8))
(print (int->uint 255 8))

(define (uint->int x)
  (let loop ((result 0) (rest x))
    (cond
      ((null? rest) result)
      (else
        (loop (+ (* 2 result) (if (car rest) 1 0)) (cdr rest))))))

(print (uint->int '(#f #f #f #f)))
(print (uint->int '(#f #t #t #t)))
(print (uint->int '(#t #f #t #f)))
(print (uint->int '(#t #t #t #t)))
