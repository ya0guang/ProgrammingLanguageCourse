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

