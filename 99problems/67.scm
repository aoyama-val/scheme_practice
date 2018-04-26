(define (comp x y) (- x y))

(define (min-vector cp vec start end)
  (let loop ((i start) (min (vector-ref vec start)) (min-index start))
    (cond
      ((>= i end) min-index)
      ((negative? (cp (vector-ref vec i) min)) (loop (+ i 1) (vector-ref vec i) i))
      (else (loop (+ i 1) min min-index)))))

(define (select-sort-vector cp vec)
  (let loop ((i 0))
    (cond
      ((>= i (vector-length vec)) vec)
      (else
        (let* ((min-index (min-vector cp vec i (vector-length vec))) (tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec min-index))
          (vector-set! vec min-index tmp)
          (loop (+ i 1)))))))

(define a #(5 6 4 7 3 8 2 9 1 0))

;(print (min-vector comp a 9 9))

; a
(print (select-sort-vector comp a))
; #(0 1 2 3 4 5 6 7 8 9)
(print a)
; #(0 1 2 3 4 5 6 7 8 9)
