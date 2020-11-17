#lang racket
(require "monads.rkt")


; Maybe Monad
(define (findf-maybe p ls)
    (cond 
        [(null? ls) (Nothing)]
        [(p (car ls)) (Just (car ls))]
        [else (findf-maybe p (cdr ls))]))

; tests: pass
; (findf-maybe symbol? '(1 2 c))
; ; (Just 'c)
; (findf-maybe boolean? '(#f 1 2 c)) 
; ; (Just #f)
; (findf-maybe number? '(a b c))
; ; (Nothing)

; Writer Monad
(define (partition-writer p ls)
    (cond
        [(null? ls) (inj-writer '())]
        [(p (car ls))
        (go-on ([lsw (partition-writer p (cdr ls))])
            (inj-writer (cons (car ls) lsw)))]
        [else
        (go-on ([_ (tell (car ls))]) 
            (partition-writer p (cdr ls)))]

    #;
    (cond 
        [(null? ls) (inj-writer '())]
        ; p hold: add to log
        [(p (car ls)) 
        (bind-writer (tell (car ls))
            (lambda (_) (partition-writer p (cdr ls))))]
        ; p doesn't hold: add to val
        [else 
        (bind-writer (partition-writer p (cdr ls))
            (lambda (lsw) (inj-writer (cons (car ls) lsw))))]
        )))

; tests: pass
; (run-writer (partition-writer even? '(1 2 3 4 5 6 7 8 9 10)))
; ;((1 3 5 7 9) . (2 4 6 8 10))
 
; (run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))
; ; ((2 4 6 8 10) . (1 3 5 7 9))

(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(zero? (sub1 n)) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))

(define (powerXpartials x n)
    (cond
        [(zero? n) (inj-writer 1)]
        [(zero? (sub1 n)) (inj-writer x)]
        [(odd? n) 
        (go-on ([y (powerXpartials x (sub1 n))]
                [_ (tell y)])
            (inj-writer (* x y)))]
        [(even? n) 
        (go-on ([y (powerXpartials x (/ n 2))]
                [_ (tell y)])
            (inj-writer (* y y)))]))

; tests: pass
; (run-writer (powerXpartials 2 6))
; ; ((2 4 8) . 64)
 
; (run-writer (powerXpartials 3 5))
; ; ((3 9 81) . 243)
 
; (run-writer (powerXpartials 5 7))
; ; ((5 25 125 15625) . 78125)

; State Monad
; tree: A; accumulator: S
(define (replace-with-count s tr)
    (cond
        [(null? tr) (inj-state '())]
        [(symbol? tr)
        (if (eqv? s tr)
            (go-on ([acc (get)]
                    [_ (put (add1 acc))])
                (inj-state acc))
            (inj-state tr))]
        [(pair? tr)
        (go-on ([lt (replace-with-count s (car tr))]
                [rt (replace-with-count s (cdr tr))])
                (inj-state (cons lt rt)))]))

; tests: pass
; ((run-state (replace-with-count 'o '(a o (t o (e o t ((n . m) . o) . f) . t) . r))) 0)
; ; (4 . (a 0 (t 1 (e 2 t ((n . m) . 3) . f) . t) . r))
 
; ((run-state (replace-with-count 'o '(((h (i s . o) . a) o s o e . n) . m))) 0)
; ; (3 . ((h (i s . 0) . a) 1 s 2 e . n) . m))
 
; ((run-state (replace-with-count 'o '(o (h (o s . o) . o) . o))) 1)
; ; (6 . (1 (h (2 s . 3) . 4) . 5))

; Mixed Monads Problems

(define traverse
    (lambda (inj bind f)
        (letrec
            ((trav
                (lambda (tree)
                    (cond
                    [(pair? tree)
                        (go-on ([a (trav (car tree))]
                                [d (trav (cdr tree))])
                        (inj (cons a d)))]
                    [else (f tree)]))))
            trav)))

(define (reciprocal n)
    (cond
        [(zero? n) (Nothing)]
        [else (Just (/ 1 n))]))

; tests: pass
; (reciprocal 0)
; ; (Nothing)
 
; (reciprocal 2)
; ; (Just 1/2)

(define traverse-reciprocal
    (traverse Just bind-maybe reciprocal))
 
; (traverse-reciprocal '((1 . 2) . (3 . (4 . 5))))
; ; (Just ((1 . 1/2) . (1/3 . (1/4 . 1/5))))
 
; (traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))
; ; (Nothing)

(define (halve n)
    (cond
        [(even? n) (inj-writer (/ n 2))]
        [(odd? n) 
        (go-on ([_ (tell n)])
            (inj-writer n))]))

; tests: pass
; (run-writer (halve 6))
; ; (() . 3)
 
; (run-writer (halve 5))
; ; ((5) . 5)

(define traverse-halve
    (traverse inj-writer bind-writer halve))
 
; (run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))
; ;((1 3 5) . ((1 . 1) . (3 . (2 . 5))))

(define (state/sum n)
    (cond
        [(number? n) 
        (go-on ([v (get)]
                [_ (put (+ v n))])
            (inj-state v))]))

; tests: pass
; ((run-state (state/sum 5)) 0)
; ; (5 . 0)
 
; ((run-state (state/sum 2)) 0)
; ; (2 . 0)
 
; ((run-state (state/sum 2)) 3)
; ; (5 . 3)

(define traverse-state/sum
    (traverse inj-state bind-state state/sum))
 
; ((run-state (traverse-state/sum '((1 . 2) . (3 . (4 . 5))))) 0)
; ; (15 . ((0 . 1) 3 6 . 10))

; Brainteaser

; List monad

#|
borrowed from lecture note:
(Listof A) is a monad
- bind  (→ (Listof A) (→ A (Listof B)) (Listof B)) is append-map
- inj (→ A (Listof A)) is (λ (a) (cons a '()))
- empty-list (→ (Listof A)) is (λ () '())

            => (bind (inj a) f) -> (apppend-map `(,a) f)
- left-id: (append-map `(,a) f) ≡ (f a)
           (append (f a) '())
           (f a)

|#

#|
- right-id:
(bind ma inj) = (append-map ma inj)
              = (append (inj ma) '())
              = (append (cons ma '()) '())
              = (append ma '())
              = ma

- my anwser for associativity:
(bind (bind ma f) g) = (append-map (append-map ma f) g)
                     = (append-map (append (f ma) '()) g)
                     = (append (g (append (f ma) '())) '())
                     = (append (g (f ma)) '())
                     = (append (append (g (f ma) '())) '())
                     = (append (bind (f ma) g) '())
                     = (append ((λ (a) (bind (f a) g)) ma) '())
                     = (bind ma (λ (a) (bind (f a) g)))

|#

; Continuation monad


; (define value-of
;     (lambda (expr env)
;         (match expr
;             [(? number?) expr]
;             [(? boolean?) expr]      
;             [(? symbol?) (apply-env env expr)]
;             [`(* ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
;             [`(sub1 ,x) (sub1 (value-of x env))]
;             [`(zero? ,x) (zero? (value-of x env))]
;             [`(if ,test ,conseq ,alt) (if (value-of test env)
;                                         (value-of conseq env)
;                                         (value-of alt env))]
;             [`(capture ,k-id ,body) (callcc (lambda (k)
;                                                 (value-of body (extend-env k-id k env))))]
;             [`(return ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
;             [`(lambda (,id) ,body) (closure id body env)]
;             [`(,rator ,rand) (apply-proc (value-of rator env) (value-of rand env))])))

(define value-of-cps
    (lambda (expr env)
        (match expr
            [(? number?) (inj-k expr)]
            [(? boolean?) (inj-k expr)]      
            [(? symbol?) (inj-k (apply-env env expr))]
            [`(* ,x1 ,x2) 
            (go-on ([e1 (value-of-cps x1 env)]
                    [e2 (value-of-cps x2 env)])
                (inj-k (* e1 e2)))]
            [`(sub1 ,x) 
            (go-on ([e1 (value-of-cps x env)])
                (inj-k (sub1 e1)))]
            [`(zero? ,x) 
            (go-on ([e1 (value-of-cps x env)])
                (inj-k (zero? e1)))]
            [`(if ,test ,conseq ,alt)
            (go-on ([et (value-of-cps test env)])
                (if et (value-of-cps conseq env) (value-of-cps alt env)))]
            [`(capture ,k-id ,body)             
            (callcc (lambda (k)
                    (value-of-cps body (extend-env k-id k env))))]
            [`(return ,k-exp ,v-exp) 
            (go-on ([ek (value-of-cps k-exp env)]
                    [ev (value-of-cps v-exp env)])
                (ek ev))]
            [`(lambda (,id) ,body) (inj-k (closure id body env))]
            [`(,rator ,rand) 
            (go-on ([etor (value-of-cps rator env)]
                    [end (value-of-cps rand env)])
                (apply-proc etor end))])))


(define empty-env
  (λ ()
    (λ (y) (error "oops, unound " y))))

(define apply-env
  (λ (env y)
    (env y)))

(define extend-env
  (λ (x arg env)
    (λ (y) (if (eqv? y x) arg (apply-env env y)))))

(define apply-proc
  (λ (clos arg)
    (clos arg)))

(define closure
  (λ (x body env)
    (λ (arg)
      (value-of-cps body (extend-env x arg env)))))

; the test uses run-cont
(define run-cont run-k)


; some tests 

; (value-of 'x (empty-env))

; (value-of '(((lambda (a) (lambda (b) (* a b))) 3) 5)
;        (empty-env))

; tests: pass

; ((run-cont (value-of-cps '9 (empty-env))) (lambda (v) v))
; ((run-cont (value-of-cps '#f (empty-env))) (lambda (v) v))
; ((run-cont (value-of-cps '(* 3 (sub1 4)) (empty-env))) (lambda (v) v))
; ((run-cont (value-of-cps '(if (zero? 3) 4 5) (empty-env))) (lambda (v) v))


(define fact-5
    '((lambda (f)
        ((f f) 5))
      (lambda (f)
        (lambda (n)
          (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))
 

; ((run-cont (value-of-cps fact-5 (empty-env))) (lambda (v) v))
; ; 120
 
(define capture-fun
    '(* 3 (capture q (* 2 (return q 4)))))
 
; ((run-cont (value-of-cps capture-fun (empty-env))) (lambda (v) v))
; 12