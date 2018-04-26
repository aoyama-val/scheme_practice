(define (prepend-all lis x)
  (map (lambda (y) (cons x y)) lis))

(display (prepend-all '((1) (2) (3)) 4))
(newline)


: nCrの組み合わせ
; (car lis)を含む場合と含まない場合に分けて再帰
(define (combination lis n)
  (cond ((= n 0) '(()))
        ((< (length lis) n) '())
        (else (append (prepend-all (combination (cdr lis) (- n 1)) (car lis))
                                    (combination (cdr lis) n)))))

(display (combination '() 1))
(newline)

(display (combination '(1) 1))
(newline)

(display (combination '(1 2) 1))
(newline)

(display (combination '(1 2) 2))
(newline)

(display (combination '(1 2 3) 1))
(newline)

(display (combination '(1 2 3) 2))
(newline)

(display (combination '(1 2 3) 3))
(newline)

(define lis4 (iota 4 1))
(display lis4)
(newline)

(display (combination lis4 1)) (newline)
(display (combination lis4 2)) (newline)
(display (combination lis4 3)) (newline)
(display (combination lis4 4)) (newline)


