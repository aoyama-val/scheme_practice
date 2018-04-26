(use srfi-1)  ; リスト処理
(use srfi-13) ; 文字列処理

; 可変長引数
; 改行つきでprint
(define print
  (lambda args
    (for-each (lambda (x) (display x) (newline)) args)))


; 再帰的にmap
; 2次元配列に便利
(define (map-recursive f lis)
  (cond ((null? lis) '())
        ((list? lis) (map (lambda (x) (map-recursive f x)) lis))
        (else (f lis))))
