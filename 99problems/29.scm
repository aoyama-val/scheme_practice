(define (delete-nth lis n)
  (let loop ((i 0) (lis lis) (ret '()))
    (cond ((null? lis) ret)
          (else
            (loop (+ i 1) (cdr lis) (if (equal? i n)
                                      ret
                                      (append ret (list (car lis)))))))))

(define (nth lis n)
  (let loop ((lis lis) (i 0))
    (cond
      ((null? lis) '())
      ((= i n) (car lis))
      (else (loop (cdr lis) (+ i 1))))))

;#?=(nth '(a b c d e f g) 2)

;(print (delete-nth '(a b c d e f g) 2))

(define (permutation n lis)
  (let loop ((i 0) (ret '()))
    (cond
      ((null? lis) ret)
      ((= 1 (length lis)) (list lis))
      ((= i (length lis)) ret)
      (else (loop (+ i 1) (append ret (map (lambda (x) (cons (nth lis i) x)) (permutation (- n 1) (delete-nth lis i)))))))))

;(define (permutation n lis)
  ;(let loop ((lis lis) (ret '()))
    ;(cond
      ;((null? lis) ret)
      ;(else (loop (cdr lis) (cons (map (lambda (x) (cons (car lis) x)) (permutation (cdr lis) (delete (car lis) lis)

(print (permutation 1 '(a)))
(print (permutation 2 '(a b)))
(print (permutation 3 '(a b c)))
;((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
