#lang pie


; Part I
(claim length
  (Π ([A U])
    (→ (List A)
      Nat)))    
(define length
  (λ (A xs)
    (rec-List xs
      0
      (λ (a d so-far)
        (add1 so-far)))))

(claim map
  (Π ([A U]
      [B U])
    (→ (→ A B) (List A)
       (List B))))      
(define map
  (λ (A B)
    (λ (f xs)
      (rec-List xs
        (the (List B) nil)
        (λ (a d so-far)
          (:: (f a) so-far))))))

(claim list->vec
  (Π ([A U]
      [l (List A)])
    (Vec A (length A l))))   
(define list->vec
  (λ (A l)
    (ind-List l
      (λ (hole) (Vec A (length A hole)))
      vecnil
      (λ (a d so-far)
        (vec:: a so-far)))))

(claim list-append
  (Π ([A U])
    (→ (List A) (List A)
       (List A))))     
(define list-append
  (λ (A xs ys)
    (rec-List xs
      ys
      (λ (a d so-far)
        (:: a so-far)))))

; 1. Prove that map does not change the length of a list.

(claim map-same-length
  (Π ([A U]
      [B U]
      [l (List A)]
      [f (→ A B)])
    (= Nat (length A l) (length B (map A B f l)))))
(define map-same-length
    (lambda (A B)
        (lambda (l f) 
            (ind-List l 
                (lambda (hole) (= Nat (length A hole) (length B (map A B f hole))))
                #|
                (length A nil) == (length B (map A B f nil))
                0 == (length B nil)
                0 == 0
                |#
                (same 0)
                #|
                (length A (cons x l)) == (length B (map A B f (cons x l)))
                (length A (cons x l)) == (length B (cons (f x) (map A B f l)))
                (add1 (length A l)) == (add1 (length B (map A B f l)))
                ih: (length A l) == (length B (map A B f l))
                --------------------------------------------------------------
                (add1 (length A (cons x l))) == (add1 (length B (map A B f (cons x l))))
                |#
                (lambda (_ _ ih) (cong ih 
                    (the (-> Nat Nat) (lambda (x) (add1 x)))))))))

; 2, Prove that list-append is associative.

(claim list-append-assoc
  (Π ([A U]
      [l1 (List A)]
      [l2 (List A)]
      [l3 (List A)])
    (= (List A)
       (list-append A (list-append A l1 l2) l3)
       (list-append A l1 (list-append A l2 l3)))))
(define list-append-assoc
    (lambda (A l1 l2 l3)
        (ind-List l1
            (lambda (hole) (= (List A) (list-append A (list-append A hole l2) l3)
                (list-append A hole (list-append A l2 l3))))
            #|
            (list-append A (list-append A nil l2) l3) == (list-append A nil (list-append A l2 l3))
            (list-append A l2 l3) == (list-append A l2 l3)
            |#
            (same (list-append A l2 l3))
            #|
            (list-append A (list-append A (cons x l) l2) l3) == (list-append A (cons x l) (list-append A l2 l3))
            (list-append A (cons x (list-append A l l2)) l3) == (cons x (list-append A (l (list-append A l2 l3))))
            (cons x (list-append A (list-append A l l2)) l3) == (cons x (list-append A (l (list-append A l2 l3))))
            ih: (list-append A l (list-append A l2 l3)) == (list-append A (list-append A l l2) l3)
            --------------------------------------------------------------------
            (cons x (list-append A l (list-append A l2 l3))) == (cons x (list-append A (list-append A l l2) l3))
            |#
            (lambda (a d ih) (cong ih 
                (the (-> (List A) (List A)) (lambda (x) (:: a x))))))))

; Part II

(claim +
  (→ Nat Nat
     Nat))
(define +
  (λ (n m)
    (iter-Nat n
      m
      (λ (so-far)
        (add1 so-far)))))

(claim Even
  (→ Nat
    U))
(define Even
  (λ (n)
    (Σ ([half Nat])
      (= Nat n (+ half half)))))

(claim *
  (→ Nat Nat
     Nat))
(define *
  (λ (x y)
    (iter-Nat x
      0
      (+ y))))

; 3, Prove the associativity of +.

(claim +-asso
  (Π ([a Nat]
      [b Nat]
      [c Nat])
    (= Nat (+ (+ a b) c) (+ a (+ b c)))))
(define +-asso
    (lambda (a b c)
        (ind-Nat a 
            (lambda (hole) (= Nat (+ (+ hole b) c) (+ hole (+ b c))))
            #|
            (+ (+ 0 b) c) == (+ 0 (+ b c))
            (+ b c) == (+ b c)
            |#
            (same (+ b c))
            #|
            (+ (+ (add1 a) b) c) == (+ (add1 a) (+ b c))
            (+ (add1 (+ a b)) c) == (add1 (+ a (+ b c)))
            (add1 (+ (+ a b) c)) == (add1 (+ a (+ b c)))
            ih: (+ (+ a b) c) == (+ a (+ b c))
            |#
            (lambda (k ih) (cong ih 
                (the (-> Nat Nat) (lambda (x) (add1 x)))))
            )))


; 4, Define the type Square that describes a perfect square number.

(claim Square
  (→ Nat
    U))

(define Square
    (lambda (n)
        (Sigma ([x Nat])
            (= Nat n (* x x)))))

; ; 5, Prove that 36 is a perfect square.

(claim 36-square
  (Square 36))
(define 36-square
    (cons 6 (same 36)))


; Part III

; 6, Define the vec-length function. Do it the hard way: use ind-Vec, do not merely return ℓ.

(claim vec-length
  (Π ([A U]
      [l Nat])
    (→ (Vec A l)
      Nat)))
(define vec-length
    (lambda (A l v)
        (ind-Vec l v
            (lambda (_ _) Nat)
            0
            (lambda (_ _ _ almost) (add1 almost)))))

; ; test
; (vec-length Nat 2 (vec:: 6 (vec:: 17 vecnil)))

; 7, Prove that your vec-length is correct.

(claim vec-length-correct
  (Π ([A U]
      [l Nat]
      [v (Vec A l)])
    (= Nat (vec-length A l v) l)))
(define vec-length-correct
    (lambda (A l v) 
        (ind-Vec l v
            (lambda (hl hv) (= Nat (vec-length A hl hv) hl))
            #|
            (vec-length A 0 vecnil) == 0
            |#
            (same 0)
            #|
            (vec-length A (add1 k) (vec:: x v)) == (add1 (vec-length A k v))
            ih: (vec-length A l v) == l
            ----------------------------------------------
            (vec-length A (add1 k) (vec:: x v)) == (add1 l)
            |#
            (lambda (_ _ _ ih) (cong ih
                (the (-> Nat Nat) (lambda (x) (add1 x)))))
            )))

; Brainteasers

; 8, Define the function vec→list.

(claim vec->list
  (Π ([A U]
      [l Nat])
    (→ (Vec A l)
      (List A))))
(define vec->list
    (lambda (A l v)
        (ind-Vec l v
            (lambda (_ _) (List A))
            (the (List A) nil)
            (lambda (k a d almost)
                (:: a almost)))))

; test
; (vec->list Nat 2 (vec:: 6 (vec:: 17 vecnil)))

; 9, Prove that vec→list doesn't change the length.

(claim vec->list-same-len
  (Π ([A U]
      [l Nat]
      [v (Vec A l)])
    (= Nat (length A (vec->list A l v)) l)))
(define vec->list-same-len
    (lambda (A l v)
        (ind-Vec l v
            (lambda (hl hv) (= Nat (length A (vec->list A hl hv)) hl))
            #|
            (length A (vec->list A l vecnil)) == 0
            (length A (the (List A) nil)) == 0
            0 == 0
            |#
            (same 0)
            #|
            (length A (vec->list A l (vec:: x v))) == (length A (:: x (vec->list A l v)))
            (length A (:: x (vec->list A l v))) == (add1 (length A (vec->list A l v)))
            ih: (length A (vec->list A l v)) == l
            -----------------------------------------------------------
            (length A (vec->list A l (vec:: x v))) == (add1 l)
            |#
            (lambda (_ _ _ ih) (cong ih 
                (the (-> Nat Nat) (lambda (x) (add1 x))))))))


; 10, Prove that the addition of two even numbers is even.

; prove lemmas first
; the snippet for +-comm is borrowed from 11_17 lecture notes


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

(claim a+b+c+d=a+c+b+d
    (Pi ([a Nat]
         [b Nat]
         [c Nat]
         [d Nat])
        (= Nat (+ (+ a b) (+ c d)) (+ (+ a c) (+ b d)))))
(define a+b+c+d=a+c+b+d
    (lambda (a b c d)
        (ind-Nat a
            (lambda (hole) (= Nat (+ (+ hole b) (+ c d)) (+ (+ hole c) (+ b d))))
            #|
            (+ (+ 0 b) (+ c d)) == (+ (+ 0 c) (+ b d))
            reduce +: (+ (+ 0 b) (+ c d)) == (+ b (+ c d))
            +-comm: (+ b (+ c d)) == (+ (+ c d) b)
            +-asso: (+ (+ c d) b) == (+ c (+ d b))
            +-comm: (+ c (+ d b)) == (+ c (+ b d))
            n=n+0: (+ c (+ b d)) == (+ (+ 0 c) (+ b d))
            -----------------------------------------------
            (+ (+ 0 b) (+ c d)) == (+ (+ 0 c) (+ b d))
            |#
            (replace 
                (cong (+-comm d b) 
                    (the (-> Nat Nat) (lambda (x) (+ c x))))
                (lambda (hole) (= Nat (+ b (+ c d)) hole))
                (replace (+-asso c d b)
                    (lambda (hole) (= Nat (+ b (+ c d)) hole))
                    (replace (+-comm b (+ c d))
                        (lambda (hole) (= Nat (+ b (+ c d)) hole))
                        (same (+ b (+ c d))))))
            #|
            (cong ih (+ 1)): (add1 (+ (+ a b) (c + d))) == (add1 (+ (+ a c) (+ b d)))
            (+ (add1 (+ a b)) (c + d)) == (+ (add1 (+ a c)) (+ b d))
            (+ (+ (add1 a) b) (c + d)) == (+ (+ (add1 a) c) (+ b d))            
            |#
            (lambda (k ih)
                (cong ih (+ 1 )))
            )))


(claim even+even->even
  (Π ([n Nat]
      [m Nat])
    (-> (Even n) (Even m)
      (Even (+ n m)))))
(define even+even->even
    (lambda (n m en em)
        (cons (+ (car en) (car em))
            #|
            (+ n m) == (+ n m)
            (+ n m) == (+ (+ hn hn) m)
            (+ n m) == (+ (+ hn hn) (+ hm hm))
            a+b+c+d=a+c+b+d: (+ n m) == (+ (+ hn hm) (+ hn hm))
            |#
            (replace (a+b+c+d=a+c+b+d (car en) (car en) (car em) (car em))
                (lambda (hole) (= Nat (+ n m) hole))
                (replace (cdr em)
                    (lambda (hole) (= Nat (+ n m) (+ (+ (car en) (car en)) hole)))
                    (replace (cdr en)
                        (lambda (hole) (= Nat (+ n m) (+ hole m)))
                        (same (+ n m))))))))
