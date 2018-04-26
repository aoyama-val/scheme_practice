(define (comp x y) (- x y))

(define (min-vector cp vec start end)
  (let loop ((i start) (min (vector-ref vec start)) (min-index start))
    (cond
      ((>= i end) min-index)
      ((negative? (cp (vector-ref vec i) min)) (loop (+ i 1) (vector-ref vec i) i))
      (else (loop (+ i 1) min min-index)))))


(print (min-vector comp #(5 4 6 3 7 8 2 9 1) 1 4))
; 8
