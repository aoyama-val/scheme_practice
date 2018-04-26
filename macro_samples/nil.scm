; https://www.shido.info/lisp/scheme_syntax.html

;マクロとは式の変換です。 式が評価される前に、または、コンパイル時に式が変換されます。 そして、変換後
;の式が初めからソースコードに書いてあったかのように処理が行われます。Common Lisp のマクロ定義はかなり
;複雑ですが、R6RS に準拠した Scheme では syntax-rules という形式によって比較的簡単に定義できます。
;syntax-rules を使うと変数補足などのわずらわしいことを気にしないで、 ”この式をこういう式に変換しろ”
;ということを直接的に書くことができます。ただし、syntax-rules で記述できないマクロを書くのは Common
;Lisp より複雑になります。

;簡単な例を示して説明しましょう。 [code 1] は変数に '() を代入するマクロです。
(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))

(define a 123)
(print a)
(nil! a)
(print a)
