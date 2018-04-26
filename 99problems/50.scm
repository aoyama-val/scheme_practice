(define (sieve n)
  (let loop ((result '()) (x 2))
    (cond
      ((> x n) (reverse result))
      ((any (lambda (a) (= a 0)) (map (lambda (p) (mod x p)) result))
       (loop result (+ x 1)))
      (else
        (loop (cons x result) (+ x 1))))))

;; 模範解答と同じアルゴリズム
;; 最初に2~100のリストを生成し、小さい順の素数で割れるものを除去していく
(define (sieve n)
  (define (not-divizable-by n)
    (lambda (m) (not (= (mod m n) 0))))
  (let loop ((candidates (iota 99 2)) (result '()))
    (cond
      ((null? candidates) (reverse result))
      (else
        (loop (filter (not-divizable-by (car candidates)) (cdr candidates))
              (cons (car candidates) result))))))

(print (sieve 100))
; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
