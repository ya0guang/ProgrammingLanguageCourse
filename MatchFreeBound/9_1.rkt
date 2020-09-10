#lang typed/racket

#|
': quote
`: quasiquote
,: unquote
|#

#|
1, you can build lists using quotes, quasiquotes, and unquotes
|#

(define foo
  (λ (little lamb)
    `(Mary had a ,little ,lamb whose ,(cond [(eqv? lamb 'lamb) 'fleece]
                                            [(eqv? lamb 'lion) 'mane]
                                            [else 'skin])
           was as white as snow)))

(foo 'big 'lamb)
(foo 'big 'lion)
(foo 'little 'cat )

(: length (All (A) (→ (Listof A) Number)))
#;
(define length
  (λ (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))
#|
2, you can also use quotes, quasiquotes, and unquotes
   to build patterns to match
  E.g., the above length function can be written to the following.
  (cons a d) and `(,a . ,d) share the same effect
|#
(define length
  (λ (l)
    (match l
      [`() 0]
      [`(,a . ,d) (add1 (length d))])))

#|
The grammar of our first language, L0
- Any number is in L0
- (plux e₁ e₂) is in L0, if e₁ and e₂ are both in L0
- (monus e₁ e₂) is in L0, if e₂ and e₂ are both in L0
|#
(define-type L0
  (U Number
     (List 'plux L0 L0)
     (List 'monus L0 L0)))

(: bar₁ (Listof Symbol))
(define bar₁
  (list 'cat 'dog 'a))

(: bar₂ (List Symbol Symbol Symbol))
(define bar₂
  (list 'cat 'dog 'a))

(: bar₃ (List Symbol Number Symbol))
(define bar₃
  (list 'cat 42 'b))

(: L0? (→ Any Boolean))
(define L0?
  (λ (e)
    (match e
      [`,n
       #:when (number? n)
       #t]
      [`(plux ,e₁ ,e₂)
       (and (L0? e₁) (L0? e₂))]
      [`(monus ,e₁ ,e₂)
       (and (L0? e₁) (L0? e₂))]
      [`,whatever
       #f])))

(: valof (→ L0 Number))
(define valof
  (λ (e)
    (match e
      [`,n
       #:when (number? n)
       n]
      [`(plux ,e₁ ,e₂)
       (+ (valof e₁) (valof e₂))]
      [`(monus ,e₁ ,e₂)
       (- (valof e₁) (valof e₂))])))

#;
(valof '(monus 5 (plux 10 2)))

(: λ? (→ Any Boolean))
(define λ?
  (λ (e)
    (match e
      [`,y
       #:when (symbol? e)
       #t]
      [`(λ (,x) ,body)
       (λ? body)]
      [`(,rator ,rand)
       (and (λ? rator) (λ? rand))]
      [`,whatever
       #f])))
