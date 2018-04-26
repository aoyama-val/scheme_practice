(load "./all.scm")

(define (vector-swap vec i j)
  (let ((tmp (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))


(define (quick-sort-vector-aux cp vec start end)
  (define left start)
  (define right end)
  (define pivot (vector-ref vec (quotient (- end start) 2)))
  (print "sort " start "," end " " vec)
  ;(print "enter something")
  ;(read)
  (cond
    ((>= start end) vec)
    (else
      (let loop ()
        (print "pivot = " pivot)
        (let loop1 ()
          (cond
            ((<= right left)
             #f)
            ((negative? (comp (vector-ref vec left) pivot)) ; x < pivotなら
             (inc! left)
             (loop1))
            (else
              #f)))
        (let loop2 ()
          (cond
            ((<= right left)
             #f)
            ((>= (comp (vector-ref vec right) pivot) 0)
             (dec! right)
             (loop2))
            (else
              #f)))
        (print (format "left = ~D right = ~D" left right))
        (cond
          ((< right 0) #f)
          ((>= left end) #f)
          ((<= right left)
           (begin
             (if (> left 0)
               (quick-sort-vector-aux cp vec 0 (- left 1)))
             (if (< left end)
               (quick-sort-vector-aux cp vec left end))
             ))
          (else
            (begin
              (print (format "swap vec[~D]=~D , vec[~D]=~D" left (vector-ref vec left) right (vector-ref vec right)))
              (vector-swap vec left right)
              (inc! left)
              (dec! right)
              (loop)
              )))
        )))
  vec)




(define (quick-sort-vector cp vec)
  (quick-sort-vector-aux cp vec 0 (- (vector-length vec) 1)))

;(define a #(5 6 4 7 3 8 2 9 1 0))
;(define a #(0))

(define a #(3 1 4))
(print a)
(print (quick-sort-vector comp a))

;(define a #(1 0))
;(print a)
;(print (quick-sort-vector comp a))

 ;#(0 1 2 3 4 5 6 7 8 9)

;(print a)
;#(0 1 2 3 4 5 6 7 8 9)
