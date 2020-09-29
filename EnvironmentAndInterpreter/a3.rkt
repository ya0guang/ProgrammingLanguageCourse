#lang typed/racket
(require racket/trace)
 

; Part 1

(define-type Closure
    (→ Meaning Meaning))

(define-type Env
    (→ Symbol Meaning))

(define-type Meaning
    (U Number Boolean Closure))

(define-type Exp
    (U Symbol
        Number
        Boolean
        (List 'zero? Exp)
        (List 'sub1 Exp)
        (List '* Exp Exp)
        (List 'if Exp Exp Exp)
        (List 'begin2 Exp Exp)
        (List 'set! Symbol Exp)
        (List 'let (List (List Symbol Exp)) Exp)
        (List 'lambda (List Symbol) Exp)
        (List Exp Exp)))



; Your interpreter must handle the following forms: 
; numbers, booleans, variables, lambda-abstraction, application,
; zero?, sub1, *, if, and let.

(: value-of (-> Exp Env 
            Meaning))
(define value-of
    (lambda (exp env)
        (match exp
            [`,y
                #:when (symbol? y)
                (env y)]
            [`,n
                #:when (number? n)
                n]
            [`,b
                #:when (boolean? b)
                b]
            [`(zero? ,e)
                (match (value-of e env)
                    [`,n #:when (number? n) (zero? n)]
                    [`,whatever (error "NaN in zero?" whatever)])]
            [`(sub1 ,e)
                (match (value-of e env)
                    [`,n #:when (number? n) (sub1 n)]
                    [`,whatever (error "NaN in sub1" whatever)])]
            [`(lambda (,x) ,body)
                (lambda ([arg : Meaning]) 
                    (value-of body (lambda (y) (if (eqv? y x) arg (env y)))))]
            [`(* ,e1 ,e2)
                (match (value-of e1 env)
                    [`,n1 #:when (number? n1)
                        (match (value-of e2 env)
                            [`,n2 #:when (number? n2) (* n1 n2)]
                            [`,whatever (error "NaN in *:n2" whatever)])]
                    [`,whatever (error "NaN in *:n1" whatever)])]
            [`(begin2 ,e1 ,e2)
                (begin (value-of e1 env) (value-of e2 env))]    
            ; set! has some problems: it cannot really change the value since box or
            ; association list is not used here. It can only "add" a value and this
            ; modification will disappear outside the scope of the function call this
            ; time. With box or *-ds, I believe real set! can be implemented.
            [`(set! ,s ,val) #:when (symbol? s)
                (begin
                    (set! env (lambda ([y : Symbol]) (if (eqv? y s) (value-of val env) (env y))))
                    #t)]
            [`(if ,test ,et ,el)
                (if (value-of test env) (value-of et env) (value-of el env))]
            [`(let ([,s ,v]) ,e)
                (value-of e (lambda (y) (if (eqv? y s) (value-of v env) (env y))))]
            [`(,rator ,rand)
                (match (value-of rator env)
                    [`,f
                        #:when (procedure? f)
                        (f (value-of rand env))]
                    [`,whatever
                        (error "oops, not a function")])])))

; Test cases

; (value-of `(zero? 3) init-env)
; (value-of `(sub1 3) init-env)
; (value-of `(* 3 8) init-env)
; (value-of `(if (zero? 8) 10 20) init-env)
; (value-of `(let ([x 10]) (* x 3)) init-env)

; (print "Tests for value-of")

; (value-of
;    '((lambda (x) (if (zero? x)
;                      #t
;                      #f)) 0)
;     (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of 
;    '((lambda (x) (if (zero? x) 
;                      12 
;                      47)) 
;      0) 
;    (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of
;    '(let ([y (* 3 4)])
;       ((lambda (x) (* x y)) (sub1 6)))
;    (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of
;    '(let ([x (* 2 3)])
;       (let ([y (sub1 x)])
;         (* x y)))
;    (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of
;    '(let ([x (* 2 3)])
;       (let ([x (sub1 x)])
;         (* x x)))
;    (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of 
;    '(let ((! (lambda (x) (* x x))))
;       (let ((! (lambda (n)
;                  (if (zero? n) 1 (* n (! (sub1 n)))))))
;         (! 5)))
;    (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of
;    '(((lambda (f)
;         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
;       (lambda (f)
;         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
;      5)
;    (lambda (y) (error 'value-of "unbound variable ~s" y)))

; Representation independent: function

(: empty-env-fn (-> Env))
(define empty-env-fn
    (lambda () (lambda (y) (error "oops, unound " y))))

(: extend-env-fn (-> Symbol Meaning Env Env))
(define (extend-env-fn x arg env) 
;   (lambda (y) (if (eqv? y x) arg (env y)))
    (lambda (y) 
        (if (eqv? y x) arg (apply-env-fn env y))))

(: apply-env-fn (-> Env Symbol Meaning))
(define (apply-env-fn env y)
    (env y))

(: value-of-fn (-> Exp Env 
            Meaning))
(define value-of-fn
    (lambda (exp env)
        (match exp
            [`,y
                #:when (symbol? y)
                (apply-env-fn env y)]
            [`,n
                #:when (number? n)
                n]
            [`,b
                #:when (boolean? b)
                b]
            [`(zero? ,e)
                (match (value-of-fn e env)
                    [`,n #:when (number? n) (zero? n)]
                    [`,whatever (error "NaN in zero?" whatever)])]
            [`(sub1 ,e)
                (match (value-of-fn e env)
                    [`,n #:when (number? n) (sub1 n)]
                    [`,whatever (error "NaN in sub1" whatever)])]
            [`(lambda (,x) ,body)
                (print "Abstraction")
                (lambda ([arg : Meaning]) 
                    (value-of-fn body (extend-env-fn x arg env)))]
            [`(* ,e1 ,e2)
                (match (value-of-fn e1 env)
                    [`,n1 #:when (number? n1)
                        (match (value-of-fn e2 env)
                            [`,n2 #:when (number? n2) (* n1 n2)]
                            [`,whatever (error "NaN in *:n2" whatever)])]
                    [`,whatever (error "NaN in *:n1" whatever)])]
            [`(if ,test ,et ,el)
                (if (value-of-fn test env) (value-of-fn et env) (value-of-fn el env))]
            [`(let ([,s ,v]) ,e)
                (value-of-fn e (extend-env-fn s (value-of-fn v env) env))]
            [`(,rator ,rand)
                (print "Application")
                (match (value-of-fn rator env)
                    [`,f
                        #:when (procedure? f)
                        (f (value-of-fn rand env))]
                    [`,whatever
                        (error "oops, not a function")])])))


; (value-of-fn
;    '((lambda (x) (if (zero? x)
;                      #t
;                      #f))
;      0)
;    (empty-env-fn))

; (value-of-fn 
;    '((lambda (x) (if (zero? x) 
;                      12 
;                      47)) 
;      0) 
;    (empty-env-fn))

; (value-of-fn
;    '(let ([y (* 3 4)])
;       ((lambda (x) (* x y)) (sub1 6)))
;    (empty-env-fn))

; (value-of-fn
;    '(let ([x (* 2 3)])
;       (let ([y (sub1 x)])
;         (* x y)))
;    (empty-env-fn))

; (value-of-fn
;    '(let ([x (* 2 3)])
;       (let ([x (sub1 x)])
;         (* x x)))
;    (empty-env-fn))

; (value-of-fn
;    '(((lambda (f)
;         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
;       (lambda (f)
;         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
;      5)
;    (empty-env-fn))


(define-type Env-ds
    (Listof (Pairof Symbol Meaning-ds)))

(define-type Closure-ds
    (List 'closure Symbol Exp Env-ds))

(define-type Meaning-ds
    (U Number Boolean Closure-ds))

(: empty-env-ds (-> Env-ds))
(define empty-env-ds
    (lambda () '() ))

(: extend-env-ds (-> Symbol Meaning-ds Env-ds Env-ds))
(define (extend-env-ds x arg env)
    `((,x . ,arg) . ,env))

(: apply-env-ds (-> Env-ds Symbol Meaning-ds))
(define (apply-env-ds env y)
    (match env
        ['()
            (error "oooooops, unound " y)]
        [`((,x . ,arg) . ,env) 
            (if (eqv? y x) arg (apply-env-ds env y))]))

(: apply-closure-ds (→ Closure-ds Meaning-ds Meaning-ds))
(define (apply-closure-ds clos arg)
    (match clos
      [`(closure ,x ,body ,env)
       (value-of-ds body (extend-env-ds x arg env))]))

; borrowing from class note doesn't help
; (define apply-closure-ds
;   (λ (clos arg)
;     (match clos
;       [`(closure ,x ,body ,env)
;        (value-of-ds body (extend-env-ds x arg env))])
;         ))


(: make-closure-ds (→ Symbol Exp Env-ds Closure-ds))
(define (make-closure-ds x body env)
    `(closure ,x ,body ,env))

; closure version
(: value-of-ds (-> Exp Env-ds 
            Meaning-ds))
(define value-of-ds
    (lambda (exp env)
        (match exp
            [`,y
                #:when (symbol? y)
                (apply-env-ds env y)]
            [`,n
                #:when (number? n)
                n]
            [`,b
                #:when (boolean? b)
                b]
            [`(zero? ,e)
                (match (value-of-ds e env)
                    [`,n #:when (number? n) (zero? n)]
                    [`,whatever (error "NaN in zero?" whatever)])]
            [`(sub1 ,e)
                (match (value-of-ds e env)
                    [`,n #:when (number? n) (sub1 n)]
                    [`,whatever (error "NaN in sub1" whatever)])]
            [`(lambda (,x) ,body)
                (print "Abstraction")
                (make-closure-ds x body env)]
            [`(* ,e1 ,e2)
                (match (value-of-ds e1 env)
                    [`,n1 #:when (number? n1)
                        (match (value-of-ds e2 env)
                            [`,n2 #:when (number? n2) (* n1 n2)]
                            [`,whatever (error "NaN in *:n2" whatever)])]
                    [`,whatever (error "NaN in *:n1" whatever)])]
            [`(if ,test ,et ,el)
                (if (value-of-ds test env) (value-of-ds et env) (value-of-ds el env))]
            [`(let ([,s ,v]) ,e)
            ;                  (lambda (y) (if (eqv? y x) arg (env y)))
            ;                  (lambda (y) (if (eqv? y s) (value-of-ds v env) (env y)))
                (value-of-ds e (extend-env-ds s (value-of-ds v env) env))]
            [`(,rator ,rand)
                (print "Application")
                (match (value-of-ds rator env)
                    [`(closure ,x ,body ,env)
                        (apply-closure-ds `(closure ,x ,body ,env) (value-of-ds rand env))]
                    [`,whatever
                        (error "oops, not a closure")])])))


; test 


; (trace value-of-ds)
; (trace value-of-fn)





; (value-of-ds
;    '(((lambda (f)
;         (lambda (n) (if (zero? n) 1 ((f f) (sub1 n)))))
;       (lambda (f)
;         (lambda (n) (if (zero? n) 1 ((f f) (sub1 n))))))
;      1)
;    (empty-env-ds))


; (value-of-fn
;    '(((lambda (f)
;         (lambda (n) (if (zero? n) 1 ((f f) (sub1 n)))))
;       (lambda (f)
;         (lambda (n) (if (zero? n) 1 ((f f) (sub1 n))))))
;      1)
;    (empty-env-fn))


; TODO: unsolved ds test case
; (value-of-ds
;    '(((lambda (f)
;         (lambda (n1) (if (zero? n1) 1 (* n1 ((f f) (sub1 n1))))))
;       (lambda (g)
;         (lambda (n2) (if (zero? n2) 1 (* n2 ((g g) (sub1 n2)))))))
;      5)
;    (empty-env-ds))

; 5 Please check `value-of` function above
; tests for begin2 and set!

; (value-of
;     '(* (begin2 1 1) 3)
;     (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of
;     '((lambda (a)
;         ((lambda (p)
;            (begin2
;              (p a)
;              a))
; 	 (lambda (x) (set! x 4))))
;       3)
;      (lambda (y) (error 'value-of "unbound variable ~s" y)))

    
; (value-of
;    '((lambda (f)
;        ((lambda (g)
;           ((lambda (z) (begin2
;                         (g z)
;                         z))
;            55))
;         (lambda (y) (f y)))) (lambda (x) (set! x 44)))
;    (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of
;     '((lambda (x)
;         (begin2 (set! x 5) x))
;       6)
;     (lambda (y) (error 'value-of "unbound variable ~s" y)))


; (value-of 
;     '(let ([a 3]) 
;         (begin2 (begin2 a (set! a 4)) a))
;     (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of 
;     '((lambda (x)
;         (begin2
;           ((lambda (y)
; 	     (begin2
; 	       (set! x 0)
; 	       98))
;            99)
;           x))
;       97)
;     (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of 
;     '((lambda (y)
;         (let ((x (begin2
;                    (set! y 7)
;                    8)))
;           (begin2
;             (set! y 3)
;               ((lambda (z) y)
;                x))))
;       4)
;     (lambda (y) (error 'value-of "unbound variable ~s" y)))

; (value-of 
;     '(let ((a 5))
;        (let ((y (begin2 (set! a (sub1 a)) 6)))
;          (begin2
;            (* y y)
;            a)))
;     (lambda (y) (error 'value-of "unbound variable ~s" y)))

; 6

; typed version of value-of-lex
; note I changed "zero" to "zero?" for semantic consistency

(: value-of-lex (-> Exp Env-lex 
            Meaning))
(define value-of-lex
    (lambda (exp env)
        (match exp
            [`,b
                #:when (boolean? b)
                b]
            [`(zero? ,e)
                (match (value-of-lex e env)
                    [`,n #:when (number? n) (zero? n)]
                    [`,whatever (error "NaN in zero?" whatever)])]
            [`(const ,n)
                #:when (number? n)
                n]
            [`(sub1 ,e)
                (match (value-of-lex e env)
                    [`,n #:when (number? n) (sub1 n)]
                    [`,whatever (error "NaN in sub1" whatever)])]
            [`(mult ,e1 ,e2)
                (match (value-of-lex e1 env)
                    [`,n1 #:when (number? n1)
                        (match (value-of-lex e2 env)
                            [`,n2 #:when (number? n2) (* n1 n2)]
                            [`,whatever (error "NaN in *:n2" whatever)])]
                    [`,whatever (error "NaN in *:n1" whatever)])]
            [`(if ,test ,et ,el)
                (if (value-of-lex test env) (value-of-lex et env) (value-of-lex el env))]
            [`(var ,num)
                #:when (number? num)
                (apply-env-lex env num)]
            [`(lambda ,body) 
                (lambda ([a : Meaning]) (value-of-lex body (extend-env-lex a env)))]
            [`(,rator ,rand)
                (match (value-of-lex rator env)
                    [`,f
                        #:when (procedure? f)
                        (f (value-of-lex rand env))]
                    [`,whatever
                        (error "oops, not a function")])])))

; untyped version of value-of-lex

; (define value-of-lex
;   (lambda (exp env)
;     (match exp
;       [`(const ,expr) expr]
;       [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
;       [`(zero ,x) (zero? (value-of-lex x env))]
;       [`(sub1 ,body) (sub1 (value-of-lex body env))]
;       [`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env))]
;       [`(var ,num) (apply-env-lex env num)]
;       [`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env)))]
;       [`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))])))

(define-type Env-lex
    (Listof Meaning))

(: empty-env-lex (-> Env-lex))
(define empty-env-lex 
  (lambda () '()))

(: extend-env-lex (-> Meaning Env-lex Env-lex))
(define (extend-env-lex m env)
    (cons m env))

(: apply-env-lex (-> Env-lex Number Meaning))
(define (apply-env-lex env num)
    (cond
        ((zero? num) (car env))
        (else (apply-env-lex (cdr env) (sub1 num)))))

; test cases for value-of-lex

; (value-of-lex '((lambda (var 0)) (const 5)) (empty-env-lex))

; (value-of-lex '((lambda ((lambda (mult (var 0) (var 1))) (const 4))) (const 5)) (empty-env-lex))

; Dessert 7

; (define c0 (lambda (f) (lambda (x) x)))

; (define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))

; ((c5 add1) 1)

; ((c0 add1) 1)

; (define c+ (lambda (m) 
;                (lambda (n) 
;                  (lambda (a) (lambda (b) ((m a) ((n a) b)))))))


; (let ((c10 ((c+ c5) c5)))
;     ((c10 add1) 0))

; (define csub1
;     (lambda (n) 
;         (lambda (a) (lambda (b) (let ([(a b) b]) ((n a) b))))))