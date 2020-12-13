#lang pie

(claim nth
  (Π ((A U))
    (→ (List A) A Nat
       A)))
(define nth
  (λ (A ls)
    (rec-List ls
      (the (→ A Nat
              A)
           (λ (default index)
             default))
      (λ (a d almost)
        #|almost : (→ A Nat A)|#
        (λ (default index)
          (which-Nat index
            a
            (λ (index-1)
              (almost default index-1))))))))

(claim +
  (→ Nat Nat
     Nat))
(define +
  (λ (n m)
    (iter-Nat n
      m
      (λ (almost)
        (add1 almost)))))


#;
(nth Nat (:: 1 (:: 2 (:: 3 nil))) 42 100)

#|
+ is commutative
∀ n,m ∈ ℕ. (+ n m) =Nat= (+ m n)
|#

#|
the Equality (identity) type
1, formation

 A : U
 x : A
 y : A
-----------------------
(= A x y) : U

2, constructor

 x : A
----------------
(same x) : (= A x x)

3, eliminators


- congruence

 eq : (= A x y)
 f  : (→ A B)
-------------------------------- 
 (cong eq f) : (= B (f x) (f y)) 

4, sameness

|#

(claim four-is-four
  (= Nat 4 (+ 1 3)))
(define four-is-four
  (same 4))

(claim four-is-five
  (= Nat 4 5))

#|

target : Nat
motive : (→ Nat U)
base   : (motive 0)
step   : (Π ([k Nat] [ih (motive k)]) (motive (add1 k)))
--------------------------------------------------
(ind-Nat target motive base step) : (motive target)
|#

(claim n=n+0
  (Π ([n Nat])
    (= Nat n (+ n 0))))
(define n=n+0
  (λ (n)
    (ind-Nat n
      (λ (hole) (= Nat hole (+ hole 0)))
      #|(= Nat 0 (+ 0 0)) |#
      #| automatically reduces to |#
      #|(= Nat 0 0) |#
      (same 0)
      (λ (k ih)
        #|
        automatic reducion can happen
        only if the eliminator's target has a constructor on it (not a variable).
        E.g.
        (+ 3 n) can be reduced to (add1 (add1 (add1 n)))
        (+ n 3) can NOT be reduced to anything
        (+ (add1 n) m) can be reduced (add1 (+ n m))
        |#
        #|
        k    : Nat
        ih   : (= Nat k (+ k 0))
        -------------------------------
        want-pie-sees : (= Nat (add1 k) (add1 (+ k 0)))
        want-we-see   : (= Nat (add1 k) (+ (add1 k) 0))
        |#
        (cong ih (+ 1))
        ))))

(claim plus1-is-add1
  (Π ([n Nat])
    (= Nat ((+ 1) n) (add1 n))))
(define plus1-is-add1
  (λ (n)
    #|
    (= Nat ((+ 1) n) (add1 n))
    (= Nat (+ 1 n) (add1 n))
    (= Nat (add1 n) (add1 n))
    |#
    (same (add1 n))))

(claim n=0+n
  (Π ([n Nat])
    (= Nat n (+ 0 n))))
(define n=0+n
  (λ (n)
    #|
    (= Nat n (+ 0 n))
    (= Nat n n)
    |#
    (same n)))

(claim add1-jumps-in
  (Π ([n Nat]
      [m Nat])
    (= Nat (add1 (+ n m)) (+ n (add1 m)))))
(define add1-jumps-in
  (λ (n m)
    (ind-Nat n
      (λ (hole) (= Nat (add1 (+ hole m)) (+ hole (add1 m))))
      #|(= Nat (add1 (+ 0 m)) (+ 0 (add1 m)))|#
      #|(= Nat (add1 m) (add1 m))|#
      (same (add1 m))
      (λ (k ih)
        #|
         k : Nat
        ih : (= Nat (add1 (+ k m)) (+ k (add1 m)))
        ---------------------------------
        want : (= Nat (add1 (add1 (+ k m))) (add1 (+ k (add1 m))))
        |#
        (cong ih (+ 1))))))

#|


target : (= A from to)
motive : (→ A U)
base   : (motive from)
---------------------------
(replace target motive base) : (motive to)
|#

(claim +-comm
  (Π ([n Nat]
      [m Nat])
    (= Nat (+ n m) (+ m n))))
(define +-comm
  (λ (n m)
    (ind-Nat n
      (λ (hole) (= Nat (+ hole m) (+ m hole)))
      #|
      (= Nat (+ 0 m) (+ m 0))
      (= Nat m (+ m 0))
      |#
      (n=n+0 m)
      (λ (k ih)
        (replace (add1-jumps-in m k)
           (λ (hole) (= Nat (add1 (+ k m)) hole))
           (cong ih (+ 1)))
        #|
        k  : Nat
        ih : (= Nat (+ k m) (+ m k))
        (cong ih (+ 1)) : (= Nat (add1 (+ k m)) (add1 (+ m k)))
        ----------------------------------------
        want : (= Nat (add1 (+ k m)) (+ m (add1 k)))
        |#
        ))))

