; リストのイテレータを作る
; call/cc版
(define (make-iter2 l)
  (let ((cont '()) )
    (lambda ()
      ; 最初呼ばれたときはcontがnullなのでifの最初の式が実行される
      (if (null? cont)
        ; まずcall/ccでreturnの戻る地点を決めておく
          (call/cc (lambda (return)
                     (let loop ((lis l))
                       (if (null? lis)
                         (undefined)
                         (begin
                           ;loopでループするが、loop (cdr lis)で次の繰り返しを実行する前に
                           ; call/ccで現在の継続を保存した上でreturnしてしまう
                           (call/cc (lambda (c) (set! cont c) (return (car lis))))
                           (loop (cdr lis)))))))
          ; 2回目以降の呼び出しの場合は保存した継続から実行を続ける
          (cont)))))
(define it (make-iter2 '(1 2 3 4)))

(display (it)) (newline)
(display (it)) (newline)
(display (it)) (newline)
(display (it)) (newline)
(display (it)) (newline)
(display (it)) (newline)
(display (it)) (newline)
(display (it)) (newline)
(display (it)) (newline)
(display (it)) (newline)

(exit)

; リストのイテレータを作る
; 普通のクロージャ版
(define (make-iter1 lis)
  (let ((lis lis) (cont '()))
    (lambda ()
      (if (null? lis)
        (undefined)
        (begin0
          (car lis)
          (set! lis (cdr lis)))))))

