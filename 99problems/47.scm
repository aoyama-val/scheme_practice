(define (nth lis n)
  (let loop ((lis lis) (i 0))
    (cond
      ((null? lis) '())
      ((= i n) (car lis))
      (else (loop (cdr lis) (+ i 1))))))

(define (delete-nth lis n)
  (let loop ((i 0) (lis lis) (ret '()))
    (cond ((null? lis) ret)
          (else
            (loop (+ i 1) (cdr lis) (if (equal? i n)
                                      ret
                                      (append ret (list (car lis)))))))))

(define (permutation n lis)
  (let loop ((i 0) (ret '()))
    (cond
      ((null? lis) ret)
      ((= 1 (length lis)) (list lis))
      ((= i (length lis)) ret)
      (else (loop (+ i 1) (append ret (map (lambda (x) (cons (nth lis i) x)) (permutation (- n 1) (delete-nth lis i)))))))))



(define (komachi)
  (let loop
    [(permlist (permutation 9 '(1 2 3 4 5 6 7 8 9)))
     (result '())]
    (cond
      ((null? permlist) (reverse result))
      ((answer? (car permlist)) (loop (cdr permlist) (cons (car permlist) result)))
      (else (loop (cdr permlist) result)))))

(define (calc-aux a b c)
  (/ a (+ (* 10 b) c)))

(define (calc p)
  (+
    (calc-aux (nth p 0) (nth p 1) (nth p 2))
    (calc-aux (nth p 3) (nth p 4) (nth p 5))
    (calc-aux (nth p 6) (nth p 7) (nth p 8))))

(define (answer? lis)
  (= 1 (numerator (calc lis))))

;(print (calc '(3 2 7 6 5 4 9 8 1)))

;(print (answer? '(3 2 7 6 5 4 9 8 1)))
;(print (answer? '(3 2 7 6 5 4 9 8 2)))


(print (komachi))
