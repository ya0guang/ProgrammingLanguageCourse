#lang racket


(require macro-debugger/stepper-text)

;; We will learn how to write macros
#|
 Lets take `and` for example
 (and #f Ω) this should return #f and omega function shouldn't be evalutated.
 we wouldn't want to do this behaviour in our interpreter.
 So we can use macros to transform program to different structure
|#

(define-syntax andd
  (syntax-rules ()
    [(andd) #t]
    [(andd e0) e0]
    [(andd e1 e2) (if e1 e2 #f)]))


(andd #f #t)
(andd #t 1)


(define-syntax orr
  (syntax-rules ()
    [(orr) #f]
    [(orr e0) e0]
    [(orr e0 e1 ...) (let ([v e0])
                       (if v v (orr e1 ...)))]))

(orr 5)
(orr 5 6 7 8 9 10)
(orr (display "hello \n") 4 5)

(define-syntax condd
  (syntax-rules (else)
    [(condd) (void)]
    [(condd [else e]) e]
    [(condd [e0 e1 ...][e0^ e1^ ...] ...) (if e0 (begin e1 ...) (condd [e0^ e1^ ...] ...))]))


(condd
  [#f 42]
  [#t (display "hi \n")]
  [else (display "foo")])

(condd
  [#f 42]
  [#f (display "hi \n")]
  [else (display "foo")])

