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

(: walk* (→ Term Substitution Term))
(define walk*
  (λ (t σ)
    (let ([t (walk t σ)])
      (cond
        [(pair? t)
         (cons (walk* (car t) σ)
               (walk* (cdr t) σ))]
        [else t]))))



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

(: take$ (→ Number Stream 
            (Listof Substitution)))
(define take$
  (λ (n $)
    (cond
      [(zero? n) '()]
      [(null? $) '()]
      [(pair? $) (cons (car $) (take$ (sub1 n) (cdr $)))]
      [else (take$ n ($))])))

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

(: succeed Goal)
(define succeed
  (λ (s)
    `(,s)))

(define-syntax conj
  (syntax-rules ()
    [(conj)
     succeed]
    [(conj g₀ g ...)
     (conj₂ g₀ (conj g ...))]))

#;
(let ([x (Var 'x)]
      [y (Var 'y)]
      [z (Var 'z)]
      [q (Var 'q)])
  (take$ ((disj₂ (conj₂ (≡ x 'cat) (≡ y 42))
                 (conj₂ (≡ x 'dog) (≡ x 'cat)))
          empty-s)
         2))

(: call/fresh (→ Symbol (→ Var Goal)
                 Goal))
(define call/fresh
  (λ (x f)
    (f (Var 'x))))

(define-syntax fresh
  (syntax-rules ()
    [(fresh () g ...)
     (conj g ...)]
    [(fresh (x₀ x ...) g ...)
     (call/fresh 'x₀
                 (λ (x₀)
                   (fresh (x ...) g ...)))]))

(: reify (→ Term (→ Substitution Term)))
(define reify
  (λ (t)
    (λ (s)
      (let ([t (walk* t s)])
        (let ([r (reify-s t empty-s)])
          (walk* t r))))))

(: reify-s (→ Term Substitution
              Substitution))
(define reify-s
  (λ (t r)
    (let ([t (walk t r)])
      (cond
        [(Var? t) (let ([n (length r)])
                    (let ([name (string->symbol (string-append "_" (number->string n)))])
                      `((,t . ,name) . ,r)))]
        [(pair? t) (let ([r (reify-s (car t) r)])
                     (reify-s (cdr t) r))]
        [else r]))))

#;
(let ([x (Var 'x)]
      [y (Var 'y)]
      [z (Var 'z)]
      [w (Var 'z)])
  (let ([σ `((,x . 42) (,y . dog))])
    ((reify `((,x . (,w . ,z)) ((,y . ,y) . (,w . cat))))
           σ)))

(define-syntax run
  (syntax-rules ()
    [(run n (x₀ x ...) g ...)
     (run n q
       (fresh (x₀ x ...)
         (≡ `(,x₀ ,x ...) q)
         g ...))]
    [(run n q g ...)
     (let ([q (Var 'q)])
       (map (reify q) (run-goal n (conj g ...))))]))

(: run-goal (→ Integer Goal
               (Listof Substitution)))
(define run-goal
  (λ (n g)
    (take$ n (g empty-s))))

#;
(take$ 2
       ((fresh (x y z q)
          (disj₂ (conj₂ (≡ x 'cat) (≡ y 42))
                 (conj₂ (≡ x 'dog) (≡ x 'cat))))
        empty-s))

(run 2 (x y z)
  (disj₂ (conj₂ (≡ x 'cat) (≡ y 42))
         (conj₂ (≡ y 'dog) (≡ x 'budgie))))



