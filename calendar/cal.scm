;;; 日付関連関数集


(use srfi-1)

; not equal
(define (!= a b)
  (not (= a b)))

; うるう年判定
(define (leap-year? year)
  (and (= (mod year 4) 0)
       (or (!= (mod year 100) 0)
           (= (mod year 400) 0))))

; 月の日数
(define (days-in-month year month)
  (cond ((= month 1) 31)
        ((= month 2) (+ 28 (if (leap-year? year) 1 0)))
        ((= month 3) 31)
        ((= month 4) 30)
        ((= month 5) 31)
        ((= month 6) 30)
        ((= month 7) 31)
        ((= month 8) 31)
        ((= month 9) 30)
        ((= month 10) 31)
        ((= month 11) 30)
        ((= month 12) 31)))

; 1年の各月の日数のリスト
(define (days-in-each-month year)
  (map (lambda (month) (days-in-month year month)) (iota 12 1)))

;(print (days-in-each-month 2015))


; print
(define print
  (lambda args
    (for-each (lambda (x) (display x) (newline)) args)))

; 総和
(define (sum lis)
  (reduce + 0 lis))

;(print (sum (map (lambda (x) (days-in-month 2015 x)) (iota 12 1))))

; 指定した日がその年の何日目であるか
(define (how-manyth-day year month day)
  (+ day (sum (take (days-in-each-month year) (- month 1)))))

;(print (how-manyth-day 2015 9 25))

; start〜end間の閏年のリスト
(define (leap-years start end)
  (filter leap-year? (iota (+ 1 (- end start)) start)))

;(print (how-manyth-day 2015 9 25))
;(print (leap-years 1970 2015))

; unixタイムスタンプへ変換
(define (ymdhis2unixtime y m d h i s)
  ;86400*(365*45 + 11 + 31+28+31+30+31+30+31+31 + 24) + 60*60*(11-9) + 60*37+50
  (+ (* 86400 (+ (* 365 (- y 1970)) (length (leap-years 1970 y)) (- (how-manyth-day y m d) 1)))
     (* 60 60 (- h 9))
     (* 60 i)
     s))

; 曜日の名前のリスト
(define (weekday-names)
  '("日" "月" "火" "水" "木" "金" "土"))

; 曜日の名前
(define (weekday-name wd)
  (list-ref (weekday-names) wd))

; 曜日
(define (weekday y m d)
  (mod (+ 4 (quotient (ymdhis2unixtime y m d 9 0 0) (* 60 60 24))) 7))

(define (unixtime->weekday unixtime)
  (mod (+ 4 (quotient (- unixtime (* 9 60)) (* 60 60 24))) 7))

(print (weekday-name (weekday 2015 9 25)))

(print (sys-time))
(print (weekday-name (unixtime->weekday (sys-time))))

(define (unixtime->ymdhis ut)
  (let loop ((year 1970) (ut ut))
    (let ((seconds (if (leap-year? year)
                    (* 86400 366)
                    (* 86400 365))))
      (if (> ut seconds)
        (loop (+ 1 year) (- ut seconds))
        (cons year (unixtime->month year ut))))))

(define (unixtime->month year ut)
  (let loop ((month 1) (ut ut))
    (let ((seconds (* 86400 (days-in-month year month))))
      (if (> ut seconds)
        (loop (+ 1 month) (- ut seconds))
        (cons month (unixtime->day ut))))))

(define (unixtime->day ut)
  (let* ((day (+ (quotient ut 86400) 1))
         (hour (+ 9 (quotient (mod ut 86400) (* 60 60))))
         (minute (quotient (- ut (* 86400 (- day 1)) (* (- hour 9) 60 60)) 60))
         (second (mod ut 60)))
    (list day hour minute second)))

(print (unixtime->ymdhis 1443148670))
(print (unixtime->ymdhis (sys-time)))

(define (printf num)
  (string-append
    (if (< num 10)
      (string-append " " (number->string num))
      (number->string num))))


; カレンダー表示
(define (print-calendar y m)
  (display y)
  (display "/")
  (display m)
  (newline)
  (let ((days (days-in-month y m))
        (start-wd (weekday y m 1)))
    (map (lambda (x) (display "  ")) #?=(iota start-wd))
    (display (let loop ((i 1) (ret ""))
      (if (> i days)
        ret
        (begin
          (printf i)
          (loop (+ i 1) (string-append ret (printf i) (if (= (mod (weekday y m i) 7) 6) "\n" "")))))))))

(print-calendar 2015 8)

  ;(let ((today (unixtime->ymdhis (sys-time))))
    ;(let ((y (list-ref today 0))
          ;(m (list-ref today 1))
          ;(d (list-ref today 2))
          ;(h (list-ref today 3))
          ;(i (list-ref today 4))
          ;(s (list-ref today 5)))
      ;(for-each (lambda (x) (display x) (display " ")) 

;(print (length (leap-years 1970 today-year)))
;(print (ymdhis2unixtime 2015 9 25 11 37 50))

