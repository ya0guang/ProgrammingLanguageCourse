#lang typed/racket

(struct Var
  ([name : Symbol])
  #:transparent)

(define-type Substitution
  (Listof (Pairof Var Term)))

(define-type Term
  (U Var
     Number
     Symbol
     Null
     (Pairof Term Term)))

(: unify (→ Term Term Substitution
            (U Substitution False)))
(define unify
  (λ (t₁ t₂ σ)
    (let ([t₁ (walk t₁ σ)]
          [t₂ (walk t₂ σ)])
      (cond
        [(eqv? t₁ t₂) σ]
        [(Var? t₁) (ext-s t₁ t₂ σ)]
        [(Var? t₂) (ext-s t₂ t₁ σ)]
        [(and (pair? t₁) (pair? t₂))
         (let ([σ^ (unify (car t₁) (car t₂) σ)])
           (and σ^ (unify (cdr t₁) (cdr t₂) σ^)))]
        [else #f]))))

(: walk (→ Term Substitution Term))
(define walk
  (λ (t σ)
    (cond
      [(Var? t)
       (cond
         [(assv t σ)
          =>
          (λ ([pr : (Pairof Var Term)])
            (walk (cdr pr) σ))]
         [else t])]
      [else t])))

(: occurs? (→ Var Term Substitution
              Boolean))
(define occurs?
  (λ (x t σ)
    (let ([t (walk t σ)])
      (cond
        [(Var? t) (eqv? t x)]
        [(pair? t) (or (occurs? x (car t) σ)
                       (occurs? x (cdr t) σ))]
        [else #f]))))

#|there shouldn't be a loop in a substitution|#
(: ext-s (→ Var Term Substitution
            (U Substitution False)))
(define ext-s
  (λ (x t σ)
    (cond
      [(occurs? x t σ) #f]
      [else `((,x . ,t) . ,σ)])))

(: empty-s Substitution)
(define empty-s '())

#;
(let ([x (Var 'x)]
      [y (Var 'y)]
      [z (Var 'z)]
      [q (Var 'q)])
  (unify `(,x . cat) `(42 . ,y) empty-s))
#;
(let ([x (Var 'x)]
      [y (Var 'y)]
      [z (Var 'z)]
      [q (Var 'q)])
  (unify `(,x . ,y) `(,y . (,x . ,x)) empty-s))

#;
(let ([x₁ (Var 'x)]
      [x₂ (Var 'x)])
  (eqv? x₁ x₂))


(define-type Goal
  (→ Substitution Stream))

(define-type Stream
  (U Null
     (Pairof Substitution Stream)
     (→ Stream)))

(: ≡ (→ Term Term Goal))
(define ≡
  (λ (t₁ t₂)
    (λ (σ)
      (let ([σ^ (unify t₁ t₂ σ)])
        (cond
          [σ^ `(,σ^)]
          [else '()])))))

(: take (→ Stream Number
           (Listof Substitution)))
(define take
  (λ ($ n)
    (cond
      [(zero? n) '()]
      [(null? $) '()]
      [(pair? $) (cons (car $) (take (cdr $) (sub1 n)))]
      [else (take ($) n)])))

(: append$ (→ Stream Stream
              Stream))
(define append$
  (λ ($₁ $₂)
    (cond
      [(null? $₁) $₂]
      [(pair? $₁) (cons (car $₁) (append$ (cdr $₁) $₂))]
      [else (λ () (append$ $₂ ($₁)))])))

(: disj₂ (→ Goal Goal
            Goal))
(define disj₂
  (λ (g₁ g₂)
    (λ (σ)
      (append$ (g₁ σ) (g₂ σ)))))

(: append-map$ (→ Goal Stream
                  Stream))
(define append-map$
  (λ (g $)
    (cond
      [(null? $) '()]
      [(pair? $) (append$ (g (car $)) (append-map$ g (cdr $)))]
      [else (λ () (append-map$ g ($)))])))

(: conj₂ (→ Goal Goal
            Goal))
(define conj₂
  (λ (g₁ g₂)
    (λ (σ)
      (append-map$ g₂ (g₁ σ)))))

(let ([x (Var 'x)]
      [y (Var 'y)]
      [z (Var 'z)]
      [q (Var 'q)])
  (take ((disj₂ (conj₂ (≡ x 'cat) (≡ y 42))
                (conj₂ (≡ x 'dog) (≡ x 'cat)))
         empty-s)
        2))


#;
(take (append$ (nats 0) (nats 0)) 10)
