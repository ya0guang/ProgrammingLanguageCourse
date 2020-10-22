#lang racket
(require "mk.rkt")

(define ekual?
  (λ (p₁ p₂)
    (cond
      [(and (pair? p₁) (pair? p₂)) (and (ekual? (car p₁) (car p₂)) (ekual? (cdr p₁) (cdr p₂)))]
      [else (eqv? p₁ p₂)])))

#;
(ekual? '(cat ((dog ())) (42))
        '(cat ((dog ())) (apple)))

#|The function unify tests whether two things can be made ekual?|#

#|== construct a goal that invokes the unify function|#

(define goal₁
  (≡ 'cat 'cat))
(define goal₂
  (≡ 'cat 'dog))

#;
(run 1 q
  goal₁)
#;
(run 1 q
  goal₂)
#;
(run 1 q
  (≡ q 'cat))
#;
(run 10 q
  (≡ q 'cat))

(define goal₃
  (condᵉ
   [goal₁]
   [goal₂]))

#;
(run 1 q
  goal₃)

#;
(run 4 q
  (condᵉ
   [(== q 'cat)]
   [(== q 42)]
   [(== q 'dog)
    (== q 'budgie)]))

#;
(run 2 q
  (fresh (x y z)
    (== q `(,x ,y ,z))
    (condᵉ
     [(== `(,x dog ,z) `(cat ,y 42))]
     [(== `(,x dog ,z) `(cat ,y cow))])))

#;
(run 2 (x y z)
  (condᵉ
   [(== `(,x dog ,z) `(cat ,y 42))]
   [(== `(,x dog ,z) `(cat ,y cow))]))

(define (append xs ys)
  (match xs
    ['() ys]
    [`(,a . ,d) (cons a (append d ys))]))

#|
(append '(1 2 3) '(cat dog)) -> '(1 2 3 cat dog)
(appendᵒ '(1 2 3) '(cat dog) '(1 2 3 cat dog))
|#

(defrel (appendᵒ xs ys o)
  (condᵉ
   [(fresh (a d res)
      (== `(,a . ,d) xs)
      (== `(,a . ,res) o)
      (appendᵒ d ys res))]
   [(== '() xs) (== ys o)]))

#;
(run 7 (xs ys)
  (appendᵒ xs ys '(1 2 3 cat dog)))

(run 1000 (xs ys zs)
  (appendᵒ xs ys zs))
