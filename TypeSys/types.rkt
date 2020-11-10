#lang racket
(require "mk.rkt")

#;
(((λ (x) (x x))
  (λ (!)
    (λ (n)
      (if (zero? n)
          1
          (* n ((! !) (sub1 n)))))))
 5)

#|
A judgment J looks like

Γ ⊢ e : τ

where Γ is a typing environment, e is an expression, and τ is a type

a type is
- Nat
- Bool
- (→ τ₁ τ₂), if τ₁ is a type and τ₂ is a type

E.g., times is (→ Nat (→ Nat Nat))

|#

#|
An inference rule looks like

J₀ J ... (premises)
-----------------------
J (the conclusion)


E.g.,

Γ ⊢ nexp : Nat
---------------------------- sub1
Γ ⊢ (sub1 nexp) :  Nat
reads as "if nexp is a Nat in Γ, then (sub1 nexp) is a Nat in Γ"


exp is a number
---------------------- number
Γ ⊢ exp : Nat


(x , τ) ∈ Γ
-------------------------- var
Γ ⊢ x : τ


Γ ⊢ test : Bool
Γ ⊢ then : τ
Γ ⊢ else : τ
------------------------------ if
Γ ⊢ (if test then else) : τ




--------------- false
Γ ⊢ #f : Bool

--------------- true
Γ ⊢ #t : Bool


Γ, x:τ₁   ⊢   body : τ₂
-------------------------------- lambda
Γ ⊢ (λ (x) body) : (→ τ₁ τ₂) 



Γ ⊢ f : (→ τ₁ τ₂)
Γ ⊢ a : τ₁
---------------------------- app
Γ ⊢ (f a) : τ₂



Γ ⊢ e : Nat
--------------------------- zero
Γ ⊢ (zero? e) : Bool



Γ ⊢ nexp₁ : Nat
Γ ⊢ nexp₂ : Nat
--------------------------------- mult
Γ ⊢ (* nexp₁ nexp₂) : Nat



 Γ, x:τ  ⊢ body : τ
---------------------------------- recursion
 Γ ⊢ (fix (λ (x) body)) : τ

|#

#|
Type inference does NOT involve running the program. (in this lecture)
|#

(defrel (∈ Γ x τ)
  (fresh (xa τa d)
    (== `((,xa . ,τa) . ,d) Γ)
    (conde
     [(== xa x) (== τa τ)]
     [(=/= xa x) (∈ d x τ)])))

(defrel (⊢ Γ e τ)
  (conde
   [(fresh (nexp₁ nexp₂)
      (== `(* ,nexp₁ ,nexp₂) e)
      (⊢ Γ nexp₁ 'Nat)
      (⊢ Γ nexp₂ 'Nat))]
   [#|zero|#
    (fresh (nexp)
      (== `(zero? ,nexp) e)
      (== 'Bool τ)
      (⊢ Γ nexp 'Nat))]
   [#|sub1|#
    (fresh (nexp)
      (== `(sub1 ,nexp) e)
      (== 'Nat τ)
      (⊢ Γ nexp 'Nat))]
   [#|number|#
    (numbero e)
    (== 'Nat τ)]
   [#|var|#
    (symbolo e)
    (∈ Γ e τ)]
   [#|true|#
    (== #t e)
    (== 'Bool τ)]
   [#|false|#
    (== #f e)
    (== 'Bool τ)]
   [#|if|#
    (fresh (test then else)
      (== `(if ,test ,then ,else) e)
      (⊢ Γ test 'Bool)
      (⊢ Γ then τ)
      (⊢ Γ else τ))]
   [#|lambda|#
    (fresh (x body τx τb)
      (== `(λ (,x) ,body) e)
      (symbolo x)
      (== `(→ ,τx ,τb) τ)
      (⊢ `((,x . ,τx) . ,Γ) body τb))]
   [#|app|#
    (fresh (rator rand τx)
      (== `(,rator ,rand) e)
      (⊢ Γ rator `(→ ,τx ,τ))
      (⊢ Γ rand τx))]
   [#|recursion|#
    (fresh (x body)
      (== `(fix (λ (,x) ,body)) e)
      (⊢ `((,x . ,τ) . ,Γ) body τ))]))

#;
(run 1 q
  (⊢ `((x . ,q)) '(sub1 x) q))
#;
(run 1 q
  (⊢ '() '(sub1 #f) q))

(run 1 q
  (⊢ '() '(λ (x) x) q))

#;
((fix
  (λ (!)
    (λ (n)
      (if (zero? n)
          1
          (* n (! (sub1 n)))))))
 5)

#;
(run 1 t
  (⊢ '()
     `(fix (λ (!)
            (λ (n)
              (if (zero? n)
                  1
                  (* n (! (sub1 n)))))))
     t))

(run 1 t
  (⊢ '()
     `((fix (λ (!)
               (λ (n)
                 (* n (! (sub1 n))))))
       5)
     t))
