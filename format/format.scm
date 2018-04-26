; printfみたいなもの
(define (format f . args)
  (let loop ((i 0) (arg-i 0) (ret ""))
    (if (>= i (string-length f))
      ret
      (cond ((equal? (string-ref f i) #\%)
             (let ((arg (list-ref args arg-i)))
               (loop (+ i 2) (+ arg-i 1)
                     (string-append ret 
                                    (cond ((string? arg) arg)
                                          ((number? arg) (number->string arg))
                                          ((char? arg) (list->string (list arg)))
                                          (else "unknown type"))))))
            (else (loop (+ i 1) arg-i (string-append ret (list->string (list (string-ref f i))))))))))


(display (format "%s = %d %c\n" "hoge" 123 #\H))


; C#方式
(define (format2 f . args)
  (let loop ((i 0) (ret f))
    (if (>= i (length args))
      ret
      (loop (+ i 1) (regexp-replace-all (string->regexp (string-append "\\{" (number->string (+ i 1)) "\\}")) ret (list-ref args i))))))

(display (format2 "{1} = {2}\nagain {1} = {2}\n" "foo" "bar"))
(display (apply format2 '("{1} = {2}\nagain {1} = {2}\n" "foo" "bar")))
