(define (max-list lis)
  (if (null? lis)
    #f
    (let loop ((lis (cdr lis)) (ret (car lis)))
      (if (null? lis)
        ret
        (if (> (car lis) ret)
          (loop (cdr lis) (car lis))
          (loop (cdr lis) ret))))))

(define (min-list lis)
  (if (null? lis)
    #f
    (let loop ((lis (cdr lis)) (ret (car lis)))
      (if (null? lis)
        ret
        (if (< (car lis) ret)
          (loop (cdr lis) (car lis))
          (loop (cdr lis) ret))))))

(print (max-list '(5 6 4 7 3 8 2 9 1)))
; 9
(print (min-list '(5 6 4 7 3 8 2 9 1)))
;1
