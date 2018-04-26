;; 関数型チックな書き方
(define (max lis)
  (find (lambda (x) (every (lambda (y) (>= x y)) lis)) lis))


;; 再帰を使うけどかっこ悪い書き方
(define (max lis)
  (cond 
    ((= (length lis) 0)
     #f)
    ((= (length lis) 1)
     (car lis))
    ((= (length lis) 2)
     (let ((a (car lis))
           (b (cadr lis)))
       (if (> a b)
         a
         b)))
    (else
      (max (list (car lis) (max (cdr lis)))))))
