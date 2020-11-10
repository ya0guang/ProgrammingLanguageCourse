#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))

#|
'((5))
`run` will try to search the possible values for q. 
First it sees "(== 5 q)" and then this constrain must hold because run will `conj` the constrains.
Besides, outmost `conde` should hold. Walking through `conde`s, and it holds when checks `[(== q 5)]`.
Therefore, racket tells us q can be `5` and there is no other possibility.

|#

;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))

#|
'(((_0 _1)))
The snippet creates two fresh Vars, a and be and package them into q to ask for potential values of q.
`(absento 'tag q)` specifies that 'tag should not be included in q;
`(symbolo a)` forces a to be a symbol
Racket says `(_0  _1)` is one possible solution to our question where _0 and _1 are two placeholders for something.
|#


;; 3 What do the following miniKanren constraints mean?
;; a == : two args of `==` should always be logically equivalent
;; b =/= : two args of `=/=` should never be logically equivalent
;; c absento : the 1st arg should be absent in the 2nd arg
;; d numbero : the arg should be a number
;; e symbolo : the arg should be a symbol

;; Part II goes here.

(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))

(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))

(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))

(defrel (assoco x ls out)
    (fresh (a d aa da)
        (== `(,a . ,d) ls)
        (== `(,aa . ,da) a)
        (conde
            [(== aa x)
            (== a out)]
            [(=/= aa x)   ; never use `=\=`!!! Racket will not throw an error even if mk doesn't export this :(
            (assoco x d out)])))

(defrel (reverseo ls out)
    (conde
        [(== '() ls)
        (== '() out)]
        [(fresh (a d res)
            (== `(,a . ,d) ls)
            (reverseo d res)
            (appendo res `(,a) out))]))

(defrel (stuttero ls out)
    (conde
        [(== '() ls)
        (== '() out)]
        [(fresh (a d res)
            (== `(,a . ,d) ls)
            (== `(,a ,a . ,res) out)
            (stuttero d res))]))

; ; tests for assoco
; (run* q (assoco 'x '() q))
; ;()

; (run* q (assoco 'x '((x . 5)) q))
; ;((x . 5))

; (run* q (assoco 'x '((y . 6) (x . 5)) q))
; ;((x . 5))

; (run* q (assoco 'x '((x . 6) (x . 5)) q))
; ;((x . 6))

; (run* q (assoco 'x '((x . 5)) '(x . 5)))
; ;(_0)

; (run* q (assoco 'x '((x . 6) (x . 5)) '(x . 6)))
; ;(_0)

; (run* q (assoco 'x '((x . 6) (x . 5)) '(x . 5)))
; ;()

; (run* q (assoco q '((x . 6) (x . 5)) '(x . 5)))
; ;()

; (run* q (assoco 'x '((x . 6) . ,q) '(x . 6)))
; ;(_0)

; (run 5 q (assoco 'x q '(x . 5)))
; #;
; (((x . 5) . _0)
;   ((_0 . _1) (x . 5) . _2)
;   ((_0 . _1) (_2 . _3) (x . 5) . _4)
;   ((_0 . _1) (_2 . _3) (_4 . _5) (x . 5) . _6)
;   ((_0 . _1) (_2 . _3) (_4 . _5) (_6 . _7) (x . 5) . _8))

; (run 5 q (fresh (x y z)
;                 (assoco x y z)
;                 (== `(,x ,y ,z) q)))
; #;
; ((_0 ((_0 . _1) . _2) (_0 . _1))
;   (_0 ((_1 . _2) (_0 . _3) . _4) (_0 . _3))
;   (_0 ((_1 . _2) (_3 . _4) (_0 . _5) . _6) (_0 . _5))
;   (_0 ((_1 . _2) (_3 . _4) (_5 . _6) (_0 . _7) . _8) (_0 . _7))
;   (_0 ((_1 . _2) (_3 . _4) (_5 . _6) (_7 . _8) (_0 . _9) . _10) (_0 . _9)))

; tests for reverso
; (run* q (reverseo '() q))
; ;(())

; (run* q (reverseo '(a) q))
; ;((a))

; (run* q (reverseo '(a b c d) q))
; ;((d c b a))

; (run* q (fresh (x) (reverseo `(a b ,x c d) q)))
; ;((d c _0 b a))

; (run* x (reverseo `(a b ,x d) '(d c b a)))
; ;(c)

; (run* x (reverseo `(a b c d) `(d . ,x)))
; ;((c b a))

; (run* q (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x)))))
; ;(c)

; (run 10 q (fresh (x y) (reverseo x y) (== `(,x ,y) q)))
; #;
; ((() ())
;   ((_0) (_0))
;   ((_0 _1) (_1 _0))
;   ((_0 _1 _2) (_2 _1 _0))
;   ((_0 _1 _2 _3) (_3 _2 _1 _0))
;   ((_0 _1 _2 _3 _4) (_4 _3 _2 _1 _0))
;   ((_0 _1 _2 _3 _4 _5) (_5 _4 _3 _2 _1 _0))
;   ((_0 _1 _2 _3 _4 _5 _6) (_6 _5 _4 _3 _2 _1 _0))
;   ((_0 _1 _2 _3 _4 _5 _6 _7) (_7 _6 _5 _4 _3 _2 _1 _0))
;   ((_0 _1 _2 _3 _4 _5 _6 _7 _8) (_8 _7 _6 _5 _4 _3 _2 _1 _0)))

; ; tests for stuttero
; (run 1 q (stuttero q '(1 1 2 2 3 3)))
; ;((1 2 3))

; (run* q (stuttero q '(1 1 2 2 3 3)))
; ;((1 2 3))

; (run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero a `(1 ,b ,c 2 3 ,d))))
; ;(((1 2 3) 1 2 3))

; (run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero `(,b 1) `(,c . ,d))))
; ;((_0 _1 _1 (_1 1 1)))

; (run 1 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
; ;((_0 () (_0 _0)))

; (run 2 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
; ;((_0 () (_0 _0)) (_0 (_1) (_0 _0 _1 _1)))

; Brainteaser

(defrel (lengtho ls n)
    (conde
        [(== '() ls)
        (== '() n)]
        [(fresh (a d m)
            (== `(,a . ,d) ls)
            (lengtho d m)
            (pluso '(1) m n))]))

; ; tests for lengtho
; (run 1 q (lengtho '() q))
; ;(())

; (run 1 q (lengtho '(a b) q))
; ;((0 1))

; (run 1 q (lengtho '(a b c) q))
; ;((1 1))

; (run 1 q (lengtho '(a b c d e f g) q))
; ;((1 1 1))

; (run 1 q (lengtho q (build-num 0)))
; ;(())

; (run 1 q (lengtho q (build-num 5)))
; ;((_0 _1 _2 _3 _4))

; (run 10 q (fresh (x y) (lengtho x y) (== `(,x ,y) q)))
; #;
; ((() ())
;   ((_0) (1))
;   ((_0 _1) (0 1))
;   ((_0 _1 _2) (1 1))
;   ((_0 _1 _2 _3) (0 0 1))
;   ((_0 _1 _2 _3 _4) (1 0 1))
;   ((_0 _1 _2 _3 _4 _5) (0 1 1))
;   ((_0 _1 _2 _3 _4 _5 _6) (1 1 1))
;   ((_0 _1 _2 _3 _4 _5 _6 _7) (0 0 0 1))
;   ((_0 _1 _2 _3 _4 _5 _6 _7 _8) (1 0 0 1)))