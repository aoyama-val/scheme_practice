;;; let*と同じもの

(define-syntax let-star
  (syntax-rules ()
                ((_ ((x y)) b1 ...)
                 (let ((x y))
                   b1
                   ...))))

(let-star ((x 3))
          (print "x=" x)
          (print "y="))

(define-syntax my-let*
  (syntax-rules ()
    ((_ ((p v)) b ...)
     (let ((p v)) b ...))
    ((_ ((p1 v1) (p2 v2) ...) b ...)
     (let ((p1 v1))
       (my-let* ((p2 v2) ...)
    b ...)))))
