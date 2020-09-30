#lang racket
(require racket/trace)

; 1 last-non-zero
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
	       ((null? ls) ls)
               ((zero? (car ls)) (k (last-non-zero (cdr ls))))
  	       (else (cons (car ls) (last-non-zero (cdr ls))))))))
	(last-non-zero ls)))))

; (trace last-non-zero)
; (last-non-zero '(0))
; (last-non-zero '(1 2 3 0 4 5))
; (last-non-zero '(0 4 5))
; (last-non-zero '(1 0 2 3 0 4 5))
; (last-non-zero '(1 2 3 4 5))

; 2 lex

(define (apply-env-lex env num)
    (cond
        ((zero? num) (car env))
        (else (apply-env-lex (cdr env) (sub1 num)))))

(define (extend-env-lex m env)
    (cons m env))

(define empty-env-lex 
    (lambda () '()))


; (define value-of-lex
;     (lambda (exp env)
;         (match exp
;             [`(zero? ,e)
;                 (zero? (value-of-lex e env))]
;             [`(const ,n)
;                 n]
;             [`(sub1 ,e)
;                 (sub1 (value-of-lex e env))]
;             [`(mult ,e1 ,e2)
;                 (* (value-of-lex e1 env) (value-of-lex e2 env))]
;             [`(if ,test ,et ,el)
;                 (if (value-of-lex test env) (value-of-lex et env) (value-of-lex el env))]
;             [`(letcc ,body)
;                 (let/cc k
;                     (value-of-lex body (extend-env-lex k env)))]
;             [`(throw ,k ,v)
;                 ((value-of-lex k env) (value-of-lex v env))]
;             [`(var ,num)
;                 #:when (number? num)
;                 (apply-env-lex env num)]
;             [`(lambda ,body) 
;                 (lambda (a) (value-of-lex body (extend-env-lex a env)))]
;             [`(,rator ,rand)
;                 ((value-of-lex rator env) (value-of-lex rand env))])))

(define value-of-lex
    (lambda (exp env)
        (match exp
            [`(zero? ,e)
                `(zero ,(value-of-lex e env))]
            [`(const ,n)
                n]
            [`(sub1 ,e)
                (sub1 (value-of-lex e env))]
            [`(mult ,e1 ,e2)
                `(mult ,(value-of-lex e1 env) ,(value-of-lex e2 env))]
            [`(if ,test ,et ,el)
                (if (value-of-lex test env) (value-of-lex et env) (value-of-lex el env))]
            [`(letcc ,body)
                (let/cc k
                    (value-of-lex body (extend-env-lex k env)))]
            [`(throw ,k ,v)
                ((value-of-lex k env) (value-of-lex v env))]
            [`(var ,num)
                #:when (number? num)
                (apply-env-lex env num)]
            [`(lambda ,body) 
                (lambda (a) (value-of-lex body (extend-env-lex a env)))]
            [`(,rator ,rand)
                `(app ,(value-of-lex rator env) ,(value-of-lex rand env))])))

; test for original value-of-lex
; (value-of-lex '((lambda (var 0)) (const 5)) (empty-env-lex))
; (value-of-lex '((lambda ((lambda (mult (var 0) (var 1))) (const 4))) (const 5)) (empty-env-lex))
; (value-of-lex '((lambda (zero? (sub1 (var 0)))) (const 1)) (empty-env-lex))
; (value-of-lex '((lambda (if (zero? (var 0)) (const 1) (const 2))) (const 0)) (empty-env-lex))
; (value-of-lex '((lambda (if (zero? (var 0)) (const 1) (const 2))) (const 1)) (empty-env-lex))
; (value-of-lex '(letcc (((var 0) (const 11)) ((var 0) (const 22)))) (empty-env-lex))
; (value-of-lex '(throw (lambda (var 0)) (const 5)) (empty-env-lex))

; 3 The interpreter

(define value-of-cps
    (lambda (expr env k)
        (match expr
            [`(const ,expr) (apply-k k expr)]
            [`(mult ,x1 ,x2) 
                (value-of-cps x1 env 
                    (lambda (v1) (value-of-cps x2 env 
                        (lambda (v2) (apply-k k (* v1 v2))))))]
            [`(sub1 ,x) 
                (value-of-cps x env 
                    (lambda (v) (apply-k k (sub1 v))))]
            [`(zero ,x) 
                (value-of-cps x env
                    (lambda (v) (apply-k k (zero? v))))]
            [`(if ,test ,conseq ,alt) 
                (value-of-cps test env
                    (lambda (t) (if t (value-of-cps conseq env k) (value-of-cps alt env k))))]
            [`(letcc ,body) 
                (value-of-cps body (extend-env k env) k)]
            [`(throw ,k-exp ,v-exp) 
                (value-of-cps k-exp env 
                    (lambda (vk) (value-of-cps v-exp env vk)))]
            [`(let ,e ,body) 
                (value-of-cps e env
                    (lambda (a) (value-of-cps body (extend-env a env) k)))]
            [`(var ,y) (apply-env env y k)]
            [`(lambda ,body) 
                (apply-k k (lambda (a k^1) 
                    (value-of-cps body (extend-env a env) k^1)))]
            [`(app ,rator ,rand) 
                (value-of-cps rator env
                    (lambda (clos) (value-of-cps rand env
                        (lambda (arg) (apply-closure clos arg k)))))])))

(define apply-env
    (lambda (env y k^)
        (env y k^)))

(define extend-env
    (lambda (a^ env^)
        (lambda (y k^) (if (zero? y) (apply-k k^ a^) (apply-env env^ (sub1 y) k^)))))

(define empty-env
    (lambda ()
        (lambda (y k^)
            (error 'value-of-cps "unbound identifier"))))

(define apply-closure
    (lambda (clos arg k)
        (clos arg k)))

(define apply-k
    (lambda (k^ v)
        (k^ v)))


    
(define empty-k
    (lambda ()
        (lambda (v)
            v)))

; (trace value-of-cps)

(value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k))
; 5
(value-of-cps '(app (lambda (app (lambda (mult (var 0) (var 1))) (const 4))) (const 5)) (empty-env) (empty-k))
; 20
(value-of-cps '(app (lambda (zero (sub1 (var 0)))) (const 1)) (empty-env) (empty-k))
; #t
(value-of-cps '(app (lambda (if (zero (var 0)) (const 1) (const 2))) (const 0)) (empty-env) (empty-k))
; 1
(value-of-cps '(app (lambda (if (zero (var 0)) (const 1) (const 2))) (const 1)) (empty-env) (empty-k))
; 2

(value-of-cps '(letcc (mult (const 5) (const 11))) (empty-env) (empty-k))
; (value-of-cps '(letcc (throw (var 0) (const 5))) (empty-env) (empty-k))
; don't know how to use letcc here, but I think I do it right
; so I borrowed some test cases from: 
; https://github.com/alanzplus/EOPL/blob/03f2ad5aabb63364fa73a5d93f31517e590ebd54/Indiana-CS311/a7-Continuations%20and%20Representation%20Independence/a7-tests.rkt

(value-of-cps '(letcc (throw (var 0) (const 5))) (empty-env) (empty-k))

(value-of-cps '(letcc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k))

(value-of-cps '(var 11) (empty-env) (empty-k))
