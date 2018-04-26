
(define-syntax mypush!
  (syntax-rules ()
    [(_ loc val)
     (set! loc (cons val loc))]
    [(_ . other)
     (syntax-error "malformed push!" (push! . other))]))

(define a '(1 2 3))

(print a)

(mypush! a 'hoge)

(print a)
