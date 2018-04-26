; 1次元vector版

(use gauche.sequence)

(define rows 2)
(define cols 3)
(define empty (- (* rows cols) 1))

(define board1 (vector 3 4 2 0 1 5))
(define board2 (vector 0 1 2 3 4 5))
(define board3 (vector 0 5 3 1 2 4))

;; 汎用関数
(define print
  (lambda args
    (for-each (lambda (x) (display x) (newline)) args)))
(define (=? val)
  (lambda (x) (= x val)))

;; ボード関数
; 指定セルが空セルかどうか
(define (empty? board i j)
  (= empty (board-ref board i j)))

; ボード表示
(define (print-board board)
  (let loop1 ((i 0))
    (if (>= i rows)
      #f
      (let loop2 ((j 0))
        (if (>= j cols)
          (begin
            (newline)
            (loop1 (+ i 1)))
          (begin
            (display (board-ref board i j))
            (display " ")
            (loop2 (+ j 1))))))))

; i,j -> i
(define (ij->i i j)
  (+ (* i cols) j))

; セル取得
(define (board-ref board i j)
  (vector-ref board (ij->i i j)))

; セル設定
(define (board-set! board i j val)
  (vector-set! board (ij->i i j) val))

; 横1行をvectorで取得
(define (board-row board i)
  (vector-copy board (ij->i i 0) (ij->i (+ i 1) 0)))

(define (solved? board)
  (let loop ((i 0))
    (cond ((>= i rows) #t)
          ((not (solved-row? board i)) #f)
          (else (loop (+ i 1))))))

(define (solved-row? board i)
  (let ((row (board-row board i)))
    (let loop ((j 0))
      (cond ((>= j cols) #t)
            ((not (= (vector-ref row j) (+ (* i cols) j))) #f)
            (else (loop (+ j 1)))))))

(define (movable? board dir)
  (let* ((empty-pos (find-in-board board empty)) (empty-row (car empty-pos)) (empty-col (cdr empty-pos)))
    (cond ((equal? dir :up) (not (= 0 empty-row)))
          ((equal? dir :left) (not (= 0 empty-col)))
          ((equal? dir :down) (< empty-row (- rows 1)))
          ((equal? dir :right) (< empty-col (- cols 1))))))

(define (move board dir)
  (let* ((empty-pos (find-in-board board empty)) (empty-row (car empty-pos)) (empty-col (cdr empty-pos)))
    (cond ((equal? dir :up) (swap-empty board (- empty-row 1) empty-col))
          ((equal? dir :left) (swap-empty board empty-row (- empty-col 1)))
          ((equal? dir :down) (swap-empty board (+ empty-row 1) empty-col))
          ((equal? dir :right) (swap-empty board empty-row (+ empty-col 1))))))

(define (get-relative board i j dir)
    (cond ((equal? dir :up) (- i 1)
          ((equal? dir :left) (not (= 0 empty-col)))
          ((equal? dir :down) (< empty-row (- rows 1)))
          ((equal? dir :right) (< empty-col (- cols 1))))))

(define (swap-empty board i j)
  (let* ((empty-pos (find-in-board board empty)) (empty-row (car empty-pos)) (empty-col (cdr empty-pos)) (ret (vector-copy board)))
    (let ((tmp (board-ref board i j)))
      (board-set! ret i j empty)
      (board-set! ret empty-row empty-col tmp))
    ret))

(define (find-in-board board val)
  (let loop1 ((i 0))
    (cond ((>= i rows) #f)
          (else (let ((ret (find-with-index (=? val) (board-row board i))))
                  (if ret
                    (cons i ret)
                    (loop1 (+ i 1))))))))


#|
; テスト
(print-board board3)
(print (board-row board3 0))
(print (solved? board1))
(print (solved? board2))
(print (solved? board3))
(print "---")
(print (movable? board3 :up))
(print (movable? board3 :left))
(print (movable? board3 :down))
(print (movable? board3 :right))
(print "---")
(print-board (swap-empty board3 0 0))
(print-board (swap-empty board3 1 2))
|#

; ハッシュテーブル
(define h (hash-table 'equal?))
; キュー
(define q '())

; キューに入れる（同時にハッシュにも追加する）
; つまりここに入れたものは、後で（loopの中で）訪れることを予約する
(define (add-q obj prevs)
  (set! q (append q (list obj)))
  (hash-table-put! h obj prevs))

(define (remove-first)
  (set! q (cdr q)))

; 開始局面を登録
(add-q board1 '())

(print-board board1)

(define (try)
  (let loop ()
    (if (null? q)
      (display "見つかりませんでした")
      (let* ((board (car q)) (prevs (hash-table-get h board)))
        (remove-first)
        (if (solved? board)
          (begin
            (map (lambda (x) (print-board x) (display "-----\n")) (reverse prevs)))
          (begin
            (if (and (movable? board :up) (not (hash-table-exists? h (move board :up))))
              (add-q (move board :up) (cons board prevs)))
            (if (and (movable? board :left) (not (hash-table-exists? h (move board :left))))
              (add-q (move board :left) (cons board prevs)))
            (if (and (movable? board :down) (not (hash-table-exists? h (move board :down))))
              (add-q (move board :down) (cons board prevs)))
            (if (and (movable? board :right) (not (hash-table-exists? h (move board :right))))
              (add-q (move board :right) (cons board prevs)))
            (loop)))))))

(print q)
(print (hash-table-exists? h 123))

(try)


;(print-board (move board1 :up))
;(print-board (move board1 :left))
