; Excel の列名・列番号変換
; A <-> 1
; Z <-> 26
; AA <-> 27

(define (ascii-char num)
	(string (integer->char num)))

(define (colnum2char num)
	(if (<= num 26)
		(ascii-char (+ 64 num))
		(string-append (colnum2char (truncate (/ (- num 1) 26))) (colnum2char (+ 1 (modulo (- num 1) 26))))
		))

(define (string-upcase s)
	(apply string-append (map string (map char-upcase (string->list s)))))

(define (colchar2num str)
	(let ((lis (string->list (string-upcase str))))
		(if (= (length lis) 1)
			(- (char->integer (car lis)) 64)
			(+ (* 26 (colchar2num (string (car lis))))
				 (colchar2num (string (cadr lis)))))))

(print (colchar2num "A"))
(print (colchar2num "Z"))
(print (colchar2num "AA"))
(print (colchar2num "AZ"))
(print (colchar2num "BA"))

;(define (print x)
	;(map display '(x "\n")))
;(print (colnum2char 1))
;(print (colnum2char 26))
;(print (colnum2char 27))
;(print (colnum2char 52))
;(print (colnum2char 53))
;(print (colnum2char 250))
