(define-syntax define-not
  (syntax-rules ()
                ((_ pred)
                 (define (not-pred . x)
                   (not (pred 
