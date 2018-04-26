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

(define (nth lis n)
  (let loop ((lis lis) (i 0))
    (cond
      ((null? lis) '())
      ((= i n) (car lis))
      (else (loop (cdr lis) (+ i 1))))))

(define (all=? lis)
  (cond
    ((null? lis) #t)
    (else (every (lambda (x) (= x (car lis))) lis))))

;(define (row-sum=? lis)
  ;(all=? (map (lambda (x) (apply + x)) (slices lis 3))))
;
;(define (column-sum=? lis)
  ;(= 
    ;(apply + (map car (slices lis 3)))
  ;)

(define (get-nths lis nths)
  (map (lambda (n) (nth lis n)) nths))

;(print (get-nths '(1 2 3 4 5 6 7 8 9) '(0 3 6)))

;(print (column-sum=? '(1 2 3 4 5 6 7 8 9)))

(define (answer? lis)
  (=
    (apply + (get-nths lis '(0 1 2)))
    (apply + (get-nths lis '(3 4 5)))
    (apply + (get-nths lis '(6 7 8)))
    (apply + (get-nths lis '(0 3 6)))
    (apply + (get-nths lis '(1 4 7)))
    (apply + (get-nths lis '(2 5 8)))
    (apply + (get-nths lis '(0 4 8)))
    (apply + (get-nths lis '(2 4 6)))))


(define (solve-48)
  ;(define all-perms (permutation '(iota 9 1)A))
  (let loop ((perms (permutation (iota 9 1))) (result '()))
    (cond
      ((null? perms) result)
      ((answer? (car perms)) (loop (cdr perms) (cons (car perms) result)))
      (else (loop (cdr perms) result)))))

(define (print-answer ans)
  (map print (slices ans 3))
  (print "\n"))


;(print-answer '(3 1 4 1 5 9 2 6 5))

(map print-answer (solve-48))

;(print (solve-48))
