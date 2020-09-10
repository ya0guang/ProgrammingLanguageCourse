#lang racket

#|
How to write a natural recursion program.
1, write down the boilerplate
e.g.
(define +
  (λ (a b)
    (cond
      [... ...]
      [else ...])))

2, fill in the test for the base case,
if you recur on numbers, then it's zero?
e.g.
(define +
  (λ (a b)
    (cond
      [(zero? a) ...]
      [else ...])))

3, think hard about the result of the base case
e.g.
(define +
  (λ (a b)
    (cond
      [(zero? a) b]
      [else ...])))

4, write down the natural recursion
e.g.
(define +
  (λ (a b)
    (cond
      [(zero? a) b]
      [else ...(+ (sub1 a) b)...])))

5, make up examples that guide you from the natural recursive result to the real result
e.g.
3 plus 5 should be 8
with our current recursion, (+ (sub1 3) 5) is 7
then we should probably wrap the natural recursion with add1

6, write the wrapper
e.g.
(define +
  (λ (a b)
    (cond
      [(zero? a) b]
      [else (add1 (+ (sub1 a) b))])))
|#

(define +
  (λ (a b)
    (cond
      [(zero? a) b]
      #|
      (+ (sub1 a) b) is the "natural recursion" part.
      If your base case is testing on the arugment a,
      then make the argument a smaller,
      and recursively call the function with other arugments unchanged.
      |#
      [else (add1 (+ (sub1 a) b))])))

(define *
  (λ (a b)
    (cond
      [(zero? a) 0]
      [else (+ (* (sub1 a) b) b)])))

(define ↑
  (λ (a b)
    (cond
      [(zero? b) 1]
      [else (* (↑ a (sub1 b)) a)])))

(define sum
  (λ (l)
    (cond
      [(null? l) 0]
      [else (+ (car l) (sum (cdr l)))])))

(define length
  (λ (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))
