#lang typed/racket

;(+ 3 5)
; is β= to
;8

;(= Nat (length (:: x xs)) (length (map f (:: x xs))))
; is β= to
;(= Nat (add1 (length xs)) (add1 (length (map f xs))))


;(λ (x) x)
; is α= to
;(λ (y) y)

; Normalization by evaluation
; http://davidchristiansen.dk/tutorials/nbe/
; αβ-equivalence


(define-type Exp
  (U Symbol #|x|#
     (List 'λ (List Symbol) Exp) #|(λ (x) body)|#
     (List Exp Exp) #|(rator rand)|#
     'zero
     (List 'add1 Exp)
     (List 'rec-Nat Exp Exp Exp)
     ))

#|
zero
(add1 n)
(rec-Nat target base step)
|#

(define-type DExp
  (U Integer
     (List 'λ DExp) #|(λ body)|#
     (List DExp DExp) #|(rator rand)|#
     'zero
     (List 'add1 DExp)
     (List 'rec-Nat DExp DExp DExp)))

(define-type Env
  (Listof (Pairof Symbol Val)))

(define-type Val
  (U (List 'clos Exp Symbol Env) #|(clos body x env)|#
     (List 'Neu Neutral)
     'ZERO
     (List 'ADD1 Val)))
(define-type Neutral
  (U (List 'NeuVar Symbol)
     (List 'NeuApp Neutral Val)
     (List 'NeuRecNat Neutral Val Val)))

(: do-ap (→ Val Val Val))
(define do-ap
  (λ (clos v)
    (match clos
      [`(clos ,body ,x ,env)
       (valof body `((,x . ,v) . ,env))]
      [`(Neu ,neu)
       `(Neu (NeuApp ,neu ,v))])))

(: do-rec-nat (→ Val Val Val Val))
(define do-rec-nat
  (λ (n base-v step-v)
    (match n
      ['ZERO base-v]
      [`(ADD1 ,n)
       (let ([clos (do-ap step-v n)]
             [almost (do-rec-nat n base-v step-v)])
         (do-ap clos almost))]
      [`(Neu ,neu)
       `(Neu (NeuRecNat ,neu ,base-v ,step-v))])))

(: valof (→ Exp Env Val))
(define valof
  (λ (exp env)
    (match exp
      ['zero 'ZERO]
      [`(add1 ,body) `(ADD1 ,(valof body env))]
      [`(rec-Nat ,target ,base ,step)
       (let ([n (valof target env)]
             [base-v (valof base env)]
             [step-v (valof step env)])
         (do-rec-nat n base-v step-v))]
      [`,y
       #:when (symbol? y)
       (cond
         [(assv y env) => cdr]
         [else (error "oops")])]
      [`(λ (,x) ,body)
       `(clos ,body ,x ,env)]
      [`(,rator ,rand)
       (let ([clos (valof rator env)]
             [v (valof rand env)])
         (do-ap clos v))])))


#;
(valof '(λ (x) (λ (y) (y x)))
       '())

; (λ (x) x)
; ->
; (clos x x '())
; ->
; (λ (x) x)

(: read-back (→ Val Exp))
(define read-back
  (λ (val)
    (match val
      ['ZERO 'zero]
      [`(ADD1 ,v) `(add1 ,(read-back v))]
      [`(clos ,body ,x ,env)
       `(λ (,x)
          ,(read-back (valof body `((,x . (Neu (NeuVar ,x))) . ,env))))]
      [`(Neu ,neu)
       (read-back-neutral neu)])))

(: read-back-neutral (→ Neutral Exp))
(define read-back-neutral
  (λ (neu)
    (match neu
      [`(NeuVar ,x) x]
      [`(NeuApp ,neu ,v)
       `(,(read-back-neutral neu) ,(read-back v))]
      [`(NeuRecNat ,neu ,base-v ,step-v)
       `(rec-Nat ,(read-back-neutral neu) ,(read-back base-v) ,(read-back step-v))])))

(: normalize (→ Exp Exp))
(define normalize
  (λ (exp)
    (read-back (valof exp '()))))

#;
(normalize '(λ (x) ((λ (y) y) x)))  
#;
(normalize '((λ (x) x) (λ (z) z)))  

(: deBruijnize (→ Exp (Listof (Pairof Symbol Integer))
                  DExp))
(define deBruijnize
  (λ (exp env)
    (match exp
      ['zero 'zero]
      [`(add1 ,body) `(add1 ,(deBruijnize body env))]
      [`(rec-Nat ,target ,base ,step)
       `(rec-Nat ,(deBruijnize target env) ,(deBruijnize base env) ,(deBruijnize step env))]
      [`,y
       #:when (symbol? y)
       (cond
         [(assv y env) => cdr]
         [else (error "oops")])]
      [`(λ (,x) ,body)
       `(λ ,(deBruijnize body `((,x . ,(length env)) . ,env)))]
      [`(,rator ,rand)
       `(,(deBruijnize rator env) ,(deBruijnize rand env))])))

#;
(deBruijnize (normalize '(λ (x) ((λ (y) y) x))) '())  
#;
(deBruijnize (normalize '((λ (x) x) (λ (z) z))) '())  

(: check-same (→ Exp Exp Boolean))
(define check-same
  (λ (e₁ e₂)
    (let ([e₁ (deBruijnize (normalize e₁) '())]
          [e₂ (deBruijnize (normalize e₂) '())])
      (equal? e₁ e₂))))

#;
(check-same '(λ (x) ((λ (y) y) x)) '((λ (x) x) (λ (z) z)))

#;
(normalize '(rec-Nat (add1 (add1 zero)) zero (λ (k) (λ (almost) (add1 almost)))))
