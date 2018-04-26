(define-syntax when
  (syntax-rules ()
                ((_ pred b1 ...)
                 (if pred (begin b1 ...)))))

(when #t
  (print "aa")
  (print "bb")
  (print "cc"))

(when #t
  (print "dd"))

(when #t)

(print "end")
