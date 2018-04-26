;;; クラス定義のサンプル

; リファレンス・マニュアル
; http://practical-scheme.net/gauche/man/gauche-refj_68.html

; Schemeではメンバ変数のことを「スロット」と呼ぶ


; クラス定義
; 第2引数は継承元クラス
; x yはスロット名
(define-class <2d-point> ()
                ((x :init-value 0.0 :init-keyword :x :accessor x-of)
                 (y :init-value 0.0 :init-keyword :y :accessor y-of)))

; インスタンス作成はmake関数を使う
(define a-point (make <2d-point>))
(define b-point (make <2d-point> :x 50.0 :y -10.0))

; スロットに代入
(slot-set! a-point 'x 10.0)  ;; a-point のスロット x を 10.0 に設定

; スロット参照
(slot-ref a-point 'x)

; 代入、参照はこれでもできる
;gosh> (x-of a-point)
;0.0
;gosh> (x-of b-point)
;50.0
;gosh> (set! (y-of a-point) 3.33)
;#<undef>
;gosh> (y-of a-point)
;3.33

; メソッド定義
(define-method move-by! ((pt <2d-point>) dx dy)
                 (inc! (x-of pt) dx)
                   (inc! (y-of pt) dy))

; メソッド呼び出し
(move-by! b-point 1.4 2.5)
