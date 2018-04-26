(use srfi-19)

(define (today) (current-date))

(define (make-month m y)
  (make-date 0 0 0 0 1 m y (date-zone-offset (current-date))))

(define (next-month date)
  (if (= (date-month date) 12)
    (make-month 1 (+ (date-year date) 1))
    (make-month (+ (date-month date) 1) (date-year date))))

#?=(next-month (today))

#?=(date->modified-julian-day (today))

(display 
  (let ((today (current-date))
        (days-in-month ())
        )
    (date->string today "~Y/~m/~d"))
  )
