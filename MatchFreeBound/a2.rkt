#lang typed/racket

; Part 1

; 1 list-ref 
(: list-ref (All (A) (→ (Listof A) Number A)))
(define list-ref
    (lambda (ls n)
        (letrec
            ([nth-cdr : (→ Number (Listof A))
                (lambda (n)
	                  ;; complete the definition
                    (cond
                        ; should check the validity of n here, but type check fails
                        ; ((>= n (length ls)) (error "Invalid value "))
                        ((zero? n) ls)
                        (else (cdr (nth-cdr (sub1 n))))))])
    (car (nth-cdr n)))))


; 2 union
(: union (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (union ls1 ls2)
    (cond
        ((null? ls1) ls2)
        ((memv (car ls1) ls2) (union (cdr ls1) ls2))
        (else (union (cdr ls1) (cons (car ls1) ls2)))))


; 3 extend 
(: extend (All (A) (-> A (-> A Boolean) (-> A Boolean))))
(define (extend x pred)
    (lambda ([y : A])
        (cond
            ((eqv? x y) #t)
            (else (pred y)))))


; 4 walk-symbol 
(: walk-symbol (All (A B) (-> A (Listof (Pair A B)) (U A B))))
(define (walk-symbol x ls)
    (cond
        ((assv x ls) => (lambda (pr) (walk-symbol (cdr pr) ls)))
        (else x)))


; Part 2

; skeleton for this part: 

    ; (match e
    ;     [`,y #:when (symbol? y) {XXX}]
    ;     [`(lambda (,x) ,body) 
    ;         {XXX}]
    ;     [`(,rator ,rand) {XXX}])

; from class notes
(define-type Exp
  (U Symbol
     (List 'lambda (List Symbol) Exp)
     (List Exp Exp)))
    
(define-type Exp-lumbda
  (U Symbol
     (List 'lumbda (List Symbol) Exp-lumbda)
     (List Exp-lumbda Exp-lumbda)))

; 5 lambda->lumbda
(: lambda->lumbda (-> Exp Exp-lumbda))
(define (lambda->lumbda e)
    (match e
        [`,y #:when (symbol? y) y]
        [`(lambda (,x) ,body) 
            (cond
                ((eqv? x `lambda) `(lumbda (lambda) ,(lambda->lumbda body)))
                (else `(lumbda (,x) ,(lambda->lumbda body))))]
        [`(,rator ,rand) (list (lambda->lumbda rator) (lambda->lumbda rand))]))


; 6 var-occurs?
(: var-occurs? (-> Symbol Exp Boolean))
(define (var-occurs? s e)
    (match e
        [`,y #:when (symbol? y)
            (eqv? y s)]
        [`(lambda (,x) ,body)
            (var-occurs? s body)]
        [`(,rator ,rand)
            (or (var-occurs? s rator) (var-occurs? s rand))]))

; 7 vars
(: vars (-> Exp (Listof Symbol)))
(define (vars e)
    (match e
        [`,y #:when (symbol? y) `(,y)]
        [`(lambda (,x) ,body) 
            (vars body)]
        [`(,rator ,rand) 
            (append (vars rator) (vars rand))]))


; 8 unique-vars
(: unique-vars (-> Exp (Listof Symbol)))
(define (unique-vars e)
    (match e
        [`,y #:when (symbol? y) `(,y)]
        [`(lambda (,x) ,body) 
            (unique-vars body)]
        [`(,rator ,rand) 
            (union (unique-vars rator) (unique-vars rand))]))


; 9 var-occurs-free?
(: var-occurs-free? (-> Symbol Exp Boolean))
(define (var-occurs-free? s e)
    (match e
        [`,y #:when (symbol? y) (eqv? s y)]
        [`(lambda (,x) ,body) 
            (and (not (eqv? s x))
                (var-occurs-free? s body))]
        [`(,rator ,rand) 
            (or (var-occurs-free? s rator)
                (var-occurs-free? s rand))]))


; 10 var-occurs-bound?
(: var-occurs-bound? (-> Symbol Exp Boolean))
(define (var-occurs-bound? s e)
    (match e
        [`,y #:when (symbol? y) #f]
        [`(lambda (,x) ,body) 
            (or (and (eqv? s x)
                    (var-occurs? x body))
                (var-occurs-bound? s body))]
        [`(,rator ,rand) 
            (or (var-occurs-bound? s rator)
                (var-occurs-bound? s rand))]))


; 11 unique-free-vars

; filter version
; (: unique-free-vars (-> Exp (Listof Symbol)))
; (define (unique-free-vars e)
;     (filter (lambda ([x : Symbol]) (var-occurs-free? x e)) (unique-vars e)))

(: unique-free-vars (-> Exp (Listof Symbol)))
(define (unique-free-vars e)
    (match e
        [`,y #:when (symbol? y) `(,y)]
        [`(lambda (,x) ,body) 
            (remv x (unique-free-vars body))]
        [`(,rator ,rand) 
            (union (unique-free-vars rator) (unique-free-vars rand))]))


; 12 unique-bound-vars

; filter version
; (: unique-bound-vars (-> Exp (Listof Symbol)))
; (define (unique-bound-vars e)
;     (filter (lambda ([x : Symbol]) (var-occurs-bound? x e)) (unique-vars e)))


(: unique-bound-vars (-> Exp (Listof Symbol)))
(define (unique-bound-vars e)
    (match e
        [`,y #:when (symbol? y) `()]
        [`(lambda (,x) ,body) 
            (cond
                ((var-occurs? x body) (union `(,x) (unique-bound-vars body)))
                (else (unique-bound-vars body)))]
        [`(,rator ,rand) 
            (union (unique-bound-vars rator) (unique-bound-vars rand))]))


; 13 lex
(define-type Exp-lex
  (U (List 'var Natural)
     (List 'lambda Exp-lex)
     (List Exp-lex Exp-lex)))

; helper function from a1
; list-index-ofv
(: list-index-ofv (All (A) (-> A (Listof A) Natural)))
(define (list-index-ofv s ls)
    (cond
        ((eqv? s (car ls)) 0)
        (else (add1 (list-index-ofv s (cdr ls))))))


(: lex (-> Exp (Listof Symbol) Exp-lex))
(define (lex e ls) 
    (match e
        [`,y #:when (symbol? y) 
            `(var ,(list-index-ofv y ls))]
        [`(lambda (,x) ,body) 
            `(lambda ,(lex body (cons x ls)))]
        [`(,rator ,rand) 
            `(,(lex rator ls) ,(lex rand ls))]))


; 14 walk-symbol-update
(begin
    (: a-list (Listof (Pairof Symbol (Boxof Any))))
    (define a-list
        (map (lambda ([pr : (Pairof Symbol Any)])
            (cons (car pr) (box (cdr pr))))
            '((c . 15) (e . f) (b . c) (a . b)))))

(: walk-symbol-update (-> Any (Listof (Pairof Symbol (Boxof Any))) Any))
(define (walk-symbol-update x ls)
    (cond
        ((assv x ls) => 
            (lambda (pr)
                (let ([y (walk-symbol-update (unbox (cdr pr)) ls)])
                    (set-box! (cdr pr) y)
                    y)))
        (else x)))


; 15 occurs-both?
(: var-occurs-both? (-> Symbol Exp (Values Boolean Boolean)))
(define (var-occurs-both? s e)
    (match e
        [`,y #:when (symbol? y) 
            (values (eqv? s y) #f)]
        [`(lambda (,x) ,body) 
            (let-values ([(f b) (var-occurs-both? s body)])
                (values
                    (and (not (eqv? s x)) f)
                    (or (and (eqv? s x) (var-occurs? x body)) b)))]
        [`(,rator ,rand)
            (let-values ([(frator brator) (var-occurs-both? s rator)]
                            [(frand brand) (var-occurs-both? s rand)])
                (values (or frator frand) (or brator brand)))]))
