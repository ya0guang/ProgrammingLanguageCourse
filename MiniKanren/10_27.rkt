#lang racket
(require "mk.rkt")

(define (lookup vars vals y)
  (match `(,vars ,vals)
    [`(() ()) (error "oops")]
    [`((,x . ,vars^) (,a . ,vals^))
     (if (eqv? x y)
         a
         (lookup vars^ vals^ y))]))

(defrel (lookupᵒ vars vals var val)
  (fresh (x vars^ a vals^)
    (== `(,x . ,vars^) vars)
    (== `(,a . ,vals^) vals)
    (condᵉ
     [(== x var) (== a val)]
     [(=/= x var) (lookupᵒ vars^ vals^ var val)])))

#;
(run 1 var
  (lookupᵒ '(y z x) '(cat dog 42) var 'dog))

#|
'((x . 42) (y . cat))
--->
'(x y)
'(42 cat)
|#

(define (valof exp vars vals)
  (match exp
    [`,y
     #:when (symbol? y)
     (lookup vars vals y)]
    [`(λ (,x) ,body)
     `(closure ,x ,body ,vars ,vals)]
    [`(quote ,x) x]
    [`(list . ,exps) (valof* exps vars vals)]
    [`(,rator ,rand)
     (match (valof rator vars vals)
       [`(closure ,x ,body ,vars^ ,vals^)
        (match (valof rand vars vals)
          [`,arg
           (valof body `(,x . ,vars^) `(,arg . ,vals^))])])]))

(defrel (valofᵒ exp vars vals o)
  (condᵉ
   [(symbolᵒ exp) (lookupᵒ vars vals exp o)]
   [(== `(quote ,o) exp)
    (absentᵒ 'closure o)]
   [(fresh (exps)
      (== `(list . ,exps) exp)
      (valof*ᵒ exps vars vals o))]
   [(fresh (a d)
      (== `(cons ,a ,d) exp)
      (fresh (ao do)
        (== `(,ao . ,do) o)
        (valofᵒ a vars vals ao)
        (valofᵒ d vars vals do)))]
   [(fresh (pr)
      (== `(car ,pr) exp)
      (fresh (d)
        (valofᵒ pr vars vals `(,o . ,d))))]
   [(fresh (pr)
      (== `(cdr ,pr) exp)
      (fresh (a)
        (valofᵒ pr vars vals `(,a . ,o))))]
   [(fresh (x body)
      (=/= 'λ x)
      (=/= 'quote x)
      (symbolᵒ x)
      (== `(λ (,x) ,body) exp)
      (== `(closure ,x ,body ,vars ,vals) o))]
   [(fresh (rator rand)
      (== `(,rator ,rand) exp)
      (fresh (x body vars^ vals^ arg)
        (valofᵒ rator vars vals `(closure ,x ,body ,vars^ ,vals^))
        (valofᵒ rand vars vals arg)
        (valofᵒ body `(,x . ,vars^) `(,arg . ,vals^) o)))]))

(define (valof* exps vars vals)
  (match exps
    ['() '()]
    [`(,e . ,es)
     (let ([v (valof e vars vals)]
           [vs (valof* es vars vals)])
       `(,v . ,vs))]))

(defrel (valof*ᵒ exps vars vals o)
  (condᵉ
   [(== '() exps) (== '() o)]
   [(fresh (e es)
      (== `(,e . ,es) exps)
      (fresh (v vs)
        (== `(,v . ,vs) o)
        (valofᵒ e vars vals v)
        (valof*ᵒ es vars vals vs)))]))

(defrel (quine e)
  (valofᵒ e '() '() e))

#;
(run 1 q
  (valofᵒ `(cdr (cons ((λ (x) x) 'cat) ((λ (x) x) 'dog)))
         '()
         '()
         q))

#;
(run 10 o
  (quine o))

(defrel (twine e₁ e₂)
  (=/= e₁ e₂)
  (valofᵒ e₁ '() '() e₂)
  (valofᵒ e₂ '() '() e₁))

#;
(run 10 (e₁ e₂)
  (twine e₁ e₂))
