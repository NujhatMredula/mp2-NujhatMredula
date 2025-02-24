#lang racket

(provide (all-defined-out)) ; export all top-level definitions

;;;;; Part 1: HOFs (and some utility functions)

(define (deep-map fn lst)
  (void))


(define (my-curry fn . rest)
  (void))


(define (lookup key lst)
  #f)


(define (update key value lst)
  #f)


(define (make-object name)
  (void))


;;;;; Part 2: Symbolic differentiation (no automated tests!)

(define (diff var exp)
  (void))


;;;;; Part 3: Meta-circular evaluator

(define (my-eval rexp)
  (let my-eval-env ([rexp rexp]
                    [env '()])           ; environment (assoc list)
    (cond [(symbol? rexp)                ; variable
           void]
          [(eq? (first rexp) 'lambda)    ; lambda expression
           void]
          [else                          ; function application
           void])))


;;;;; Part 4: Free variables

(define (free sexp)
  '())


;;;;; Extra credit: algebraic simplification (add your own tests)

;; Implemented features:
;; - ...
(define (simplify exp)
  (void))
