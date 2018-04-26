;; ファイルを行のリストとして読み込む関数
(define (read-lines filename)
  (define ip (open-input-file filename))
  (let loop ((result '()))
    (let ((line (read-line ip)))
      (cond
        ((eof-object? line)
         (close-input-port ip)
         (reverse result))
        (else
          (loop (cons line result)))))))

(map print (read-lines "readlines.scm"))
