#lang pie

#|
Installing the Π language in Dr.Racket
File > Package Manager > Package Source: pie > Install

if you have Racket 7.9 or higher, do this first
DrRacket, open "File" -> "Package Manager...", click
the "Settings" tab, and add "https://pkgs.racket-lang.org/".
|#

#|
Per Martin Lof
|#

#|
Dependent type programs are total, they always terminate
|#

#;
(claim append
  (Π ([n Nat]
      [m Nat])
    (→ (Vec Nat n) (Vec Nat m)
       (Vec Nat (+ n m)))))


#|
Judgements
1, _ is a type (The rule of formation)
E.g. Nat is a type

2, _ is a _    (The rule of construction)
E.g. 42 is a Nat

3, _ is the same _ as _ (The rule of sameness)
E.g. 5 is the same Nat as (+ 1 4) 

4, _ is the same type as _
E.g. Nat is the same type as Nat
|#

#|
The Atom type
- The formation rule:
Atom is a type
- The constructor:
A quote followed by a mixture letters and hyphens is an Atom 
- Sameness:
Two Atoms are the same if they are built by the same constructor
|#

(claim apple Atom)
(define apple
  'apple)

#|
The Pair type, (Pair A D)
- The formation rule:
If A is a type and D is a type, then (Pair A D) is a type

- The constructor:
(cons a d) is a (Pair A D), if a is an A and d is a D

a : A
d : D
--------------------------
(cons a d) : (Pair A D)

- The eliminators:

pr : (Pair A D)
-----------------------------
(car pr) : A

pr : (Pair A D)
-----------------------------
(cdr pr) : D

- The sameness rule:

a₁ is the same A as a₂
d₁ is the same D as d₂
------------------------------------------------------
(cons a₁ d₁) is the same (Pair A D) as (cons a₂ d₂)
|#

(claim three-cats
  (Pair (Pair Atom Atom) Atom))
(define three-cats
  (cons (cons 'cat 'cat) 'cat))

(claim two-cats
  (Pair Atom Atom))
(define two-cats
  (car three-cats))

#;
(check-same Atom 'cat (car two-cats))

#|
The Nat type
- formation rule: Nat is a type
- constructors:

-------------------------
zero : Nat

n : Nat
--------------------------
(add1 n) : Nat

- eliminators:

target : Nat
base : X
step : (→ Nat X)
-------------------------------
(which-Nat target base step) : X

-- (which-Nat zero base step) is the same X as base
-- (which-Nat (add1 n) base step) is the same X as (step n)

-- (iter-Nat zero base step) is the same X as base
-- (iter-Nat (add1 n) base step) is the same X as
(step (iter-Nat n base step))
|#

(claim zero?
  (→ Nat
    Atom))
(define zero?
  (λ (n)
    (which-Nat n
      'true
      (λ (_) 'false))))

(claim sub1
  (→ Nat
    Nat))
(define sub1
  (λ (n)
    (which-Nat n
      0
      (λ (n-1) n-1))))

(claim +
  (→ Nat Nat
     Nat))
(define +
  (λ (n m)
    (iter-Nat n
      m
      (λ (almost)
        #|
        if n is (add1 k),
        then almost is (+ k m)
        |#
        (add1 almost)))))

(claim *
  (→ Nat Nat
     Nat))
(define *
  (λ (n m)
    (iter-Nat n
      0
      (λ (almost)
        (+ m almost)))))

(claim nine
  Nat)
(define nine
  (add1 (+ 3 5)))

#|
 the (→ A B) type
-- formation rule:
-- constructor:
-- eliminators:
-- sameness: α, β, η  
|#

(claim id₁
  (→ Nat
    Nat))
(define id₁
  (λ (n) n))

(claim id₂
  (→ Nat
    Nat))
(define id₂
  (λ (x)
    ((the (→ Nat Nat) (λ (x) (+ 0 x))) x)))

(claim id₃
  (→ (Pair Nat Nat)
    (Pair Nat Nat)))
(define id₃
  (λ (pr)
    pr))

(claim id₄
  (→ (Pair Nat Nat)
    (Pair Nat Nat)))
(define id₄
  (λ (pr)
    (cons (car pr) (cdr pr))))

(check-same (→ (Pair Nat Nat)
              (Pair Nat Nat))
            id₃ id₄)

#|
Normal forms and values
A normal form is most direct way to write an expression.
E.g. (+ 3 5) is NOT a normal form, but 8 is

A value is an expression that has a constructor at the top.
E.g. (add1 (+ 3 5)) is a value
|#


#|
- eliminators
- (List A)
- (Vec A ℓ)
- Π
|#

#|
(rec-Nat target base step)

(rec-Nat zero base step) is the same X as base

(rec-Nat (add1 k) base step) is the same X as
(step k (rec-Nat k base step))

|#

(claim fact
  (→ Nat
    Nat))
(define fact
  (λ (n)
    (rec-Nat n
      1
      (λ (k fact-of-k)
        (* (add1 k) fact-of-k)))))

#;
(fact 5)

#|
(List A)
1, formation:
 (List A) is a type if A is a type
2, constructors:

 nil : (List A)

 a : A
 l : (List A)
------------------------------
 (∷ a l) : (List A)

3, eliminators:
(rec-List target base step)

(rec-List nil base step) is the same X as base

(rec-List (∷ a l) base step) is the same X as
(step a l (rec-List l base step))


4, sameness:


a₁ is the same A a₂
l₁ is the same (List A) l₂
------------------------------------------------
(∷ a₁ l₁) is the same (List A) as (∷ a₂ l₂)
|#

(claim gimme-five-nats
  (→ Nat
    (List Nat)))
(define gimme-five-nats
  (λ (n)
    (:: n (:: n (:: n (:: n (:: n nil)))))))

(claim gimme-five-atoms
  (→ Atom
    (List Atom)))
(define gimme-five-atoms
  (λ (n)
    (:: n (:: n (:: n (:: n (:: n nil)))))))

#|
 (Π ([a A]) B)
1, formation
A is a U
B is a U if a is an A
----------------------------------
 (Π ([a A]) B) is a U

2, constructors

body is a B if x is an A 
---------------------------------
(λ (x) body) is a (Π ([a A]) B)

3, eliminators


4, sameness
|#

(claim gimme-five
  (Π ([A U])
    (→ A
      (List A))))
(define gimme-five
  (λ (A n)
    (:: n (:: n (:: n (:: n (:: n nil)))))))

#;
(gimme-five Nat 42)
#;
(gimme-five Atom 'cat)

(claim length
  (Π ([A U])
    (→ (List A)
      Nat)))
(define length
  (λ (A)
    (λ (as)
      (rec-List as
        0
        (λ (a l length-of-l)
          (add1 length-of-l))))))

#;
(length Atom (gimme-five Atom 'cat))

#|
(Vec A ℓ)
1, formation:

A : U
ℓ : Nat
--------------------
(Vec A ℓ) : U

A is a type parameter
ℓ is a type index
E.g., (vec:: 'cat vecnil) (Vec Atom 0) -> (Vec Atom 1)


2, constructors:

 vecnil : (Vec A 0)


 a : A
 v : (Vec A ℓ)
------------------------------
 (vec:: a v) : (Vec A (add1 ℓ))


3, eliminators:
4, sameness:
|#

(claim repeat
  (Π ([A U])
    (Π ([a A])
      (Π ([n Nat])
        (List A))))
  #;
  (Π ([A U]
      [a A]
      [n Nat])
    (List A)))
(define repeat
  (λ (A a n)
    (rec-Nat n
      #|Annotations are needed for the constructors.|#
      (the (List A) nil)
      (λ (k repeat-of-k)
        (:: a repeat-of-k)))))

#;
(repeat Atom 'cat 10)

#|
The induction principle of Nat

(ind-Nat target motive base step)


 motive : (→ Nat U)
 base   : (motive 0)
 step   : (Π ([k Nat] [almost (motive k)])
            (motive (add1 k)))
-------------------------------------------------------
(ind-Nat target motive base step) : (motive target)

|#

(claim repeat/vec
  (Π ([A U])
    (Π ([a A])
      (Π ([n Nat])
        (Vec A n)))))
(define repeat/vec
  (λ (A a n)
    (ind-Nat n
      (λ (k) (Vec A k))
      (the (Vec A 0) vecnil)
      (λ (k repeat-of-k)
        (vec:: a repeat-of-k)))))

(claim append
  (Π ([A U]
      [ℓ₁ Nat]
      [ℓ₂ Nat])
    (→ (Vec A ℓ₁) (Vec A ℓ₂)
       (Vec A (+ ℓ₁ ℓ₂)))))
(define append
  (λ (A ℓ₁ ℓ₂)
    (ind-Nat ℓ₁
      (λ (k)
        (→ (Vec A k) (Vec A ℓ₂)
           (Vec A (+ k ℓ₂))))
      (λ (xs ys) ys)
      (λ (k append-of-k)
        (λ (xs ys)
          (vec:: (head xs) (append-of-k (tail xs) ys)))))))

#;
(append Atom 5 3 (repeat/vec Atom 'cat 5) (repeat/vec Atom 'pie 3))

#;
(claim head
  (Π ([k Nat])
    (→ (Vec A (add1 k))
      A)))

#;
(repeat/vec Atom 'cat 5)
#;
(Vec Nat 3)
#;
(Vec Nat 4)
