#lang typed/racket

; Try typed racket
; (: plus (-> Integer Integer Integer))
; (define (plus n m)
;     (cond 
;         ((zero? m) n)
;         (else (add1 (plus n (sub1 m))))))


; Assignment 1
; Author: Hongbo Chen
; Email: hc50@iu.edu

; countdown 
(: countdown (-> Natural (Listof Natural)))
(define (countdown n)
    (cond
        ((zero? n) (list 0))
        (else (cons n (countdown (sub1 n))))))

; insertR 
(: insertR (All (A) (-> A A (Listof A) (Listof A))))
(define (insertR s1 s2 ls)
    (cond 
        ((null? ls) ls)
        (else (append 
                (cond 
                    ((eqv? s1 (car ls)) (list s1 s2))
                    (else (list (car ls))))
                (insertR s1 s2 (cdr ls))))))

; remv-1st
(: remv-1st (All (A) (-> A (Listof A) (Listof A))))
(define (remv-1st s ls)
    (cond
        ((null? ls) ls)
        ((eqv? s (car ls)) (cdr ls))
        (else (cons (car ls) (remv-1st s (cdr ls))))))

; list-index-ofv
(: list-index-ofv (All (A) (-> A (Listof A) Natural)))
(define (list-index-ofv s ls)
    (cond
        ((eqv? s (car ls)) 0)
        (else (add1 (list-index-ofv s (cdr ls))))))

; filter
(: filter (All (A) (-> (-> A Boolean) (Listof A) (Listof A))))
(define (filter p l)
    (cond ((null? l) l)
        (else 
            (append  
                (cond 
                    ((p (car l)) (list (car l)))
                    (else (list))) 
                (filter p (cdr l))))))

; zip
(: zip (All (A B) (-> (Listof A) (Listof B) (Listof (Pair A B)))))
(define (zip l1 l2)
    (cond
        ((null? l1) (list))
        ((null? l2) (list))
        (else 
            (cons 
                (cons (car l1) (car l2)) 
                (zip (cdr l1) (cdr l2))))))

; map
(: map (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (map p ls)
    (cond 
        ((null? ls) ls)
        (else 
            (cons
                (p (car ls))
                (map p (cdr ls))))))

; append
(: append (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (append ls1 ls2)
    (cond 
        ((null? ls1) ls2)
        (else (cons (car ls1) (append (cdr ls1) ls2)))))

; reverse
(: reverse (All (A) (-> (Listof A) (Listof A))))
(define (reverse ls)
    (cond 
        ((null? ls) ls)
        (else 
            (append 
                (reverse (cdr ls)) 
                (list (car ls))))))

; fact
(: fact (-> Natural Natural))
(define (fact n)
    (cond
        ((zero? n) 1)
        (else (* n (fact (sub1 n))))))

; fib
;;; should not have problem when using Natural, works with Integer
(: fib (-> Integer Integer))
(define (fib n)
    (cond
        ((zero? n) 0)
        ((zero? (sub1 n)) 1)
        (else (+ (fib (sub1 n)) (fib (sub1 (sub1 n)))))))


; 12
; 
; ((w . (x . ())) y (z . ()))
;
; (equal? '((w . (x . ())) y (z . ())) '((w x) y (z)))
; - : Boolean
; #t

; binary->natural 
(: binary->natural (-> (Listof (U Zero One)) Natural))
(define (binary->natural ls)
    (cond 
        ((null? ls) 0)
        (else (+ (car ls) (* 2 (binary->natural (cdr ls)))))))

; minus
; we cannot assume the return value is Natural
(: minus (-> Integer Natural Integer))
(define (minus m n)
    (cond
        ((zero? n) m)
        (else (minus (sub1 m) (sub1 n)))))

; div
(: div (-> Integer Natural Number))
(define (div m n)
    (cond 
        ((zero? m) 0)
        (else (add1 (div (minus m n) n)))))

; append-map
(: append-map (All (A) (-> (-> A (Listof A)) (Listof A) (Listof A))))
(define (append-map p ls)
    (cond 
        ((null? ls) ls)
        (else 
            (append
                (p (car ls))
                (append-map p (cdr ls))))))


; (: include (All (A) (-> A (Listof A) (Listof A))))
; (define (include e s)
;     (cond 
;         ((null? s) s)
;         (else 
;             (append 
;                 ((eqv? e (car s)) (list (car s)))
;                 (include e (cdr s))))))

; helper function to set-difference: member 
(: member (All (A) (-> A (Listof A) (U Boolean (Listof A)))))
(define (member e ls)
    (cond
        ((null? ls) #f)
        ((equal? e (car ls)) ls)
        (else (member e (cdr ls)))))

; set-difference
(: set-difference (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (set-difference s1 s2)
    (cond 
        ((null? s1) s1)
        (else 
            (append
                (cond 
                    ((not (member (car s1) s2)) (list (car s1)))
                    (else (list)))
                (set-difference (cdr s1) s2)))))

; powerset
(: powerset (All (A) (-> (Listof A) (Listof (Listof A)))))
(define (powerset s)
    (cond 
        ((null? s) (list s))
        (else 
            (append
                (map 
                    (lambda ([x : (Listof A)]) (append (list (car s)) x)) 
                    (powerset (cdr s)))
                (powerset (cdr s))))))


; (: map (All (A B) (-> (-> A B) (Listof A) (Listof B))))
; (: append-map (All (A) (-> (-> A (Listof A)) (Listof A) (Listof A))))
; cartesian-product
(: cartesian-product (All (A) (-> (Listof (Listof A)) (Listof (Listof A)))))
(define (cartesian-product lss)
    (cond 
        ((null? lss) `(()))
        (else 
            (append-map 
                (lambda ([x : (Listof A)])
                    (map 
                        (lambda ([y : A])
                            (cons y x))
                        (car lss))) 
                (cartesian-product (cdr lss)) ))))

; ------------------BEGIN-----------------
; Obsolete version of cartesian-product
; helper function for `helper`
(: hhelper (All (A) (-> A (Listof (Listof A)) (Listof (Listof A)))))
(define (hhelper el lss)
    (cond
        ((null? lss) `())
        (else 
            (append 
                (list (cons el (car lss)))
                (hhelper el (cdr lss))))))

; helper function for cartesian-product
(: helper (All (A) (-> (Listof A) (Listof (Listof A)) (Listof (Listof A)))))
(define (helper ls lss)
    (cond
        ((null? ls) `())
        (else 
            (append
                (hhelper (car ls) lss)
                (helper (cdr ls) lss)))))


(: cartesian-product-obs (All (A) (-> (Listof (Listof A)) (Listof (Listof A)))))
(define (cartesian-product-obs lss)
    (cond
        ((null? lss) `(())) 
        (else (helper (car lss) (cartesian-product-obs (cdr lss))))))

; ----------------END-------------------

; insertR-fr
(: insertR-fr (All (A) (-> A A (Listof A) (Listof A))))
(define (insertR-fr s1 s2 ls)
    (foldr 
        (lambda ([x : A] [y : (Listof A)]) 
            (cond 
                ((eqv? x s1) (cons x (cons s2 y)))
                (else (cons x y)))) 
        `() ls))

; filter-fr
(: filter-fr (All (A) (-> (-> A Boolean) (Listof A) (Listof A))))
(define (filter-fr p l)
    (foldr 
        (lambda ([x : A] [y : (Listof A)])
            (cond 
                ((p x) (cons x y))
                (else y)))
        `() l))

; map-fr
(: map-fr (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (map-fr p ls)
    (foldr
        (lambda ([x : A] [y : (Listof B)])
            (cons (p x) y))
        `() ls))

; append-fr
(: append-fr (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (append-fr ls1 ls2)
    (foldr
        (lambda ([x : A] [y : (Listof A)])
            (cons x y))
        ls2 ls1))

; reverse-fr
(: reverse-fr (All (A) (-> (Listof A) (Listof A))))
(define (reverse-fr ls)
    (foldr
        (lambda ([x : A] [y : (Listof A)])
            (append y (list x)))
        `() ls))

; binary->natural-fr
(: binary->natural-fr (-> (Listof (U Zero One)) Natural))
(define (binary->natural-fr ls)
    (foldr
        (lambda ([x : (U Zero One)] [y : Natural])
            (+ x (* 2 y)))
        0 ls))

; append-map-fr
(: append-map-fr (All (A) (-> (-> A (Listof A)) (Listof A) (Listof A))))
(define (append-map-fr p ls)
    (foldr
        (lambda ([x : A] [y : (Listof A)])
            (append (p x) y))
        `() ls))

; set-difference-fr
(: set-difference-fr (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (set-difference-fr s1 s2)
    (foldr
        (lambda ([x : A] [y : (Listof A)])
            (cond
                ((not (member x s2)) (cons x y))
                (else y)))
        `() s1))

; powerset-fr
(: powerset-fr (All (A) (-> (Listof A) (Listof (Listof A)))))
(define (powerset-fr s)
    (foldr
        (lambda ([x : A] [y : (Listof (Listof A))])
            (append y
                (map (lambda ([z : (Listof A)]) 
                    (cons x z))
                    y)))
        `(()) s))


; Problem 21
(: collatz (-> Natural Natural))
(define (collatz n)
    (cond
        ((zero? (sub1 n)) 1)
        ((even? n) (collatz (quotient n 2)))
        ((odd? n) (collatz (+ (* n 3) 1)))
        (else (error "Invalid value "))))

; Dessert

; ((lambda (x) 
;     (list x (list 'quote x)))
;     '(lambda (x) (list x (list 'quote x))))