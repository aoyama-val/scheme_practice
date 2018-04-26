
; 改行付き表示（可変長引数）
(define print
  (lambda args
    (begin
      (map (lambda (x) (begin (display x) (display " "))) args)
      (newline))))

; リストlisから要素xをすべて削除する
(define (delete x lis)
  (cond ((null? lis) '())
        ((= x (car lis)) (delete x (cdr lis)))
        (else (cons (car lis) (delete x (cdr lis))))))

(print (delete 3 '(1 2 3 3 2 3 4 2 1 4 1 5 3 3)))

; リストlisから最初のn個を削除する
(define (remove-first-n n lis)
  (cond ((null? lis) '())
        ((= n 0) lis)
        (else (remove-first-n (- n 1) (cdr lis)))))

(print (remove-first-n 3 '(1 2 3 4 5 6 7 8 9)))

; リストlisから最後のn個を削除する
(define (remove-last-n n lis)
  (reverse (remove-first-n n (reverse lis))))

(print (remove-last-n 3 '(1 2 3 4 5 6 7 8 9)))

; リストlisの最後にxを追加する
(define (push x lis)
  (reverse (cons x (reverse lis))))

; リストlisのi番目にxを追加する
(define (insert-at i x lis)
  (cond ((null? lis) '())
        ((= i 0) (cons x lis))
        (else (cons (car lis) (insert-at (- i 1) x (cdr lis))))))

(print (insert-at 0 "hoge" '(1 2 3 4 5 6 7)))
(print (insert-at 1 "hoge" '(1 2 3 4 5 6 7)))
(print (insert-at 3 "hoge" '(1 2 3 4 5 6 7)))

(define (number->list a)
  (cond ((= a 0) '())
        (else (cons 1 (number->list (- a 1))))))

; Pythonのrange
(define (range n)
  (cond ((= n 1) '(0))
        (else (push (- n 1) (range (- n 1))))))
(print (range 3) (range 10))

(print (number->list 0))
(print (number->list 1))
(print (number->list 2))

; リスト演算だけで足し算を定義したかったが、number->listの中で-を使っている
(define (add a b)
  (cond ((= b 0) a)
        (else (length (append (number->list a) (number->list b))))))

(print (add 4 5))
