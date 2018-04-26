; バージョン1
(define (flatten lis)
  (cond ((null? lis) '())
        (else (let loop ((ret '())
                         (lis lis))
                (if (null? lis)
                  ret
                  (let ((x (car lis)))
                    (loop (if (list? x)
                            (append ret (flatten x))
                            (append ret (list x)))
                          (cdr lis))))))))

; バージョン2
; アトムを渡した時
; (flatten 1) #=> (1)
; になる
(define (flatten lis)
  (cond ((null? lis) '())
        ((not (list? lis)) (list lis))
        (else (let loop ((ret '())
                         (lis lis))
                (if (null? lis)
                  ret
                  (loop (append ret (flatten (car lis)))
                        (cdr lis)))))))
; バージョン3
; ループを使わない版
(define (flatten-aux lis ret)
  (cond ((null? lis) ret)
        (else (flatten-aux (cdr lis) (append ret (flatten (car lis)))))))
(define (flatten lis)
  (if (not (list? lis))
    (list lis)
    (flatten-aux lis '())))

; バージョン4
; apply + appenを使った一番シンプルな版
(define (flatten lis)
  (cond
    ((pair? lis)
     (apply append (map flatten lis)))
    (else
      (list lis))))

(define (print a)
  (display a)
  (newline))

(print (flatten '()))
(print (flatten 1))
(print (flatten '(1 2 3)))
(print (flatten '(1 (2) 3)))
(print (flatten '(1 (2 (4 5 (6 (7))) 3) ((8) 9 (((((10 11)) 12)))))))
