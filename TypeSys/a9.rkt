#lang racket
(require "mk.rkt")


; steal from lecture note
(defrel (∈ Γ x τ)
  (fresh (xa τa d)
    (== `((,xa . ,τa) . ,d) Γ)
    (conde
     [(== xa x) (== τa τ)]
     [(=/= xa x) (∈ d x τ)])))


(defrel (!- env exp t)
    (conde
        ; primary types
        [#| number |#
        (numbero exp)
        (== 'Nat t)]
        [#| true |#
        (== #t exp)
        (== 'Bool t)]
        [#| false |#
        (== #f exp)
        (== 'Bool t)]
        [#| var |#
        (symbolo exp)
        (∈ env exp t)]
        ; func with one arg
        [#| zero? |#
        (fresh (e1)
            (== `(zero? ,e1) exp)
            (== 'Bool t)
            (!- env e1 'Nat))]
        [#| sub1 |#
        (fresh (e1)
            (== `(sub1 ,e1) exp)
            (== 'Nat t)
            (!- env e1 'Nat))]
        [#| not |#
        (fresh (e1)
            (== `(not ,e1) exp)
            (== 'Bool t)
            (!- env e1 'Bool))]
        [#| car |#
        (fresh (ep td)
            (== `(car ,ep) exp)
            (!- env ep `(pairof ,t ,td)))]
        [#| cdr |#
        (fresh (ep ta)
            (== `(cdr ,ep) exp)
            (!- env ep `(pairof ,ta ,t)))]
        ; func with two args
        [#| plus |#
        (fresh (e1 e2)
            (== `(+ ,e1 ,e2) exp)
            (== 'Nat t)
            (!- env e1 'Nat)
            (!- env e2 'Nat))]
        [#| multi |#
        (fresh (e1 e2)
            (== `(* ,e1 ,e2) exp)
            (== 'Nat t)
            (!- env e1 'Nat)
            (!- env e2 'Nat))]
        [#| cons |#
        (fresh (e1 e2 t1 t2)
            (== `(cons ,e1 ,e2) exp)
            (== `(pairof ,t1 ,t2) t)
            (!- env e1 t1)
            (!- env e2 t2))]
        ; func with three args
        [#| if |#
        (fresh (etest ethen eelse)
            (== `(if ,etest ,ethen ,eelse) exp)
            (!- env etest 'Bool)
            (!- env ethen t)
            (!- env eelse t))]
        ; lambda calculas
        [#| abstraction |#
        (fresh (arg body ta tb)
            (== `(lambda (,arg) ,body) exp)
            (symbolo arg)
            (== `(,ta -> ,tb) t)
            (!- `((,arg . ,ta) . ,env) body tb))]
        [#| app |#
        (fresh (erator erand targ)
            (== `(,erator ,erand) exp)
            (!- env erator `(,targ -> ,t))
            (!- env erand targ))]
        ; recursion
        [#| fix |#
        (fresh (arg body)
            (== `(fix (lambda (,arg) ,body)) exp)
            (!- `((,arg . ,t) . ,env) body t))]
        ; dessert
        [#| let |#
        (fresh (arg sub body ta)
            (== `(let ([,arg ,sub]) ,body) exp)
            (symbolo arg)
            (!- `((,arg . ,ta) . ,env) body t)
            (!- env sub ta))]))

; test cases
; zero? sub1 lambda not
; Nat Bool

; (run* q
;     (!- '()  '(+ 3 5) q))

; (run* q (!- '() #t q))
; ; (Bool)
; (run* q (!- '() 17 q))
; ; (Nat)
; (run* q (!- '() '(zero? 24) q))
; ; (Bool)
; (run* q (!- '() '(zero? (sub1 24)) q))
; ; (Bool)
; (run* q (!- '() '(not (zero? (sub1 24))) q))
; ; (Bool)
; (run* q
;     (!- '() '(zero? (sub1 (sub1 18))) q))
; ; (Bool)
; (run* q
;     (!- '()  '(lambda (n) (if (zero? n) n n)) q))
; ; ((Nat -Nat))
; (run* q
;     (!- '() '((lambda (n) (zero? n)) 5) q))
; ; (Bool)
; (run* q
;     (!- '() '(if (zero? 24) 3 4) q))
; ; (Nat)
; (run* q
;     (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
; ; (Bool)
; (run* q
;     (!- '() '(lambda (x) (sub1 x)) q))
; ; ((Nat -> Nat))
; (run* q
;     (!- '() '(lambda (a) (lambda (x) (+ a x))) q))
; ; ((Nat -> (Nat -> Nat)))
; (run* q
;     (!- '() '(lambda (f)
;                (lambda (x)
;                  ((f x) x)))
;          q))
; ; (((_0 -> (_0 -> _1)) -> (_0 -> _1)))
; (run* q
;     (!- '() '(sub1 (sub1 (sub1 6))) q))
; ; (Nat)
; (run 1 q
;     (fresh (t)
;       (!- '() '(lambda (f) (f f)) t)))
; ; ()
; (length (run 20 (q)
;              (fresh (lam a b)
;                (!- '() `((,lam (,a) ,b) 5) 'Nat)
;                (== `(,lam (,a) ,b) q))))
; ; 20
; (length (run 30 q (!- '() q 'Nat)))
; ; 30
; (length (run 30 q (!- '() q '(Nat -Nat))))
; ; 30
; (length (run 500 q (!- '() q '(Nat -Nat))))
; ; (last (run 500 q (!- '() q '(Nat -Nat))))
; ; 500
; ;; At this point, stop and take a look at maybe the 500th 
; ;; program you generate
; ;; (last (run 500 q (!- '() q '(Nat -Nat))))
; ;; You should be amazed at how quickly it's generating them.
; ;; If it isn't fast, consider reordering your clauses. 
; (length (run 30 q (!- '() q '(Bool -Nat))))
; ; 30
; (length (run 30 q (!- '() q '(Nat -(Nat -Nat)))))
; ; 30
; (length (run 100 q
;              (fresh (e t)
;                (!- '() e t)
;                (== `(,e ,t) q))))
; ; 100
; (length (run 100 q
;              (fresh (g e t)
;                (!- g e t)
;                (== `(,g ,e ,t) q))))
; ; 100
; (length
;    (run 100 q
;      (fresh (g v)
;        (!- g `(var ,v) 'Nat)
;        (== `(,g ,v) q))))
; ; 100
; (run 1 q
;        (fresh (g)
; 	 (!- g
; 	      '((fix (lambda (!)
; 		       (lambda (n)
; 			 (if (zero? n)
; 			     1
; 			     (* n (! (sub1 n)))))))
; 		5)
; 	      q)))
; ; (Nat)
; (run 1 q
;        (fresh (g)
; 	 (!- g
; 	      '((fix (lambda (!)
; 		       (lambda (n)
; 			 (* n (! (sub1 n))))))
; 		5)
; 	      q)))
; ; (Nat)

; Brainteaser

; introducing pairof
; cons

; test cases
; (run* q (!- '() '(cons (zero? 1) (zero? 0)) q))
; ; ((pairof Bool Bool))
; (run* q (!- '() '(cons (zero? 1) (cons (zero? 1) (zero? 0))) q))
; ; ((pairof Bool (pairof Bool Bool)))
; (run* t (!- '() '(lambda (x) (cons x x)) t))
; ; ((_.0 -> (pairof _.0 _.0)))
; (run* t (!- '() '(lambda (x) (lambda (y) (cons (zero? x) (+ x y)))) t))
; ; ((Nat -> (Nat -> (pairof Bool Nat))))

; car cdr
; again, test cases
; (run* t (!- '() '(lambda (x) (zero? (car x))) t))
; ; (((pairof Nat _.0) -> Bool))
; (run* t (!- '() '((lambda (x) (zero? (car x))) (cons 0 1)) t))
; ; (Bool)
; (run* t (!- '() '((lambda (x) (zero? (car x))) (cons 0 #f)) t))
; ; (Bool)
; (run* t (!- '() '((lambda (x) (car x)) (cons (cons 0 0) #f)) t)) 
; ; ((pairof Nat Nat))
; (run* t (!- '() '((lambda (x) (zero? (car x))) (cons #f 0)) t))
; ; ()
; ;; a function that accepts a pair of anything and an Nat
; (run* t (!- '() '(lambda (x) (zero? (cdr x))) t))
; ; (((pairof _.0 Nat) -> Bool))
; (run* t (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 1)) t))
; ; (Bool)
; (run* t (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 #f)) t))
; ; ()
; (run* t (!- '() '((lambda (x) (zero? (cdr x))) (cons #f 0)) t))
; ; (Bool)  

; Just Dessert

; test cases
; (run* q
;     (!- '() '(let ([f (lambda (x) x)])
;                        (if (f #t) (f (cons (f 4) 5)) (f (cons 5 (f 6)))))
;          q))
; ; ((pairof Nat Nat))
; (run* q
;     (!- '() '(let ([f (lambda (x) #t)])
;                (if #t (f (f 5)) (f #t)))
;         q))
; ; (Bool)


; ; my program doesn't work for this!
; (run* q
;     (!- '() '(let ([f (lambda (x) #t)]) (if #t (f #t) (f 5)))
;          q))
; ; but works for this
; (run* q
;     (!- '() '(let ([f (lambda (x) #t)]) (if #t (f #t) (f #t)))
;          q))
; ; so I'm guessing this is because it doens't recognize polymorphism here.