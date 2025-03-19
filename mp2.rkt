#lang racket

(provide (all-defined-out)) ; export all top-level definitions

;;;;; Part 1: HOFs (and some utility functions)

(define (deep-map fn lst)
  (map (lambda (x) (if (list? x) (deep-map fn x) (fn x))) lst))

(define (my-curry fn . args)
  (lambda more-args (apply fn (append args more-args))))

(define (lookup key lst)
  (cond [(null? lst) #f]
        [(equal? (caar lst) key) (cdar lst)]
        [else (lookup key (cdr lst))]))

(define (update key value lst)
  (cond [(null? lst) (list (cons key value))]
        [(equal? (caar lst) key) (cons (cons key value) (cdr lst))]
        [else (cons (car lst) (update key value (cdr lst)))]))

(define (make-object name)
  (lambda (msg)
    (cond [(eq? msg 'name) name]
          [else 'unknown-message])))

;;;;; Part 2: Symbolic differentiation (no automated tests!)

(define (diff var exp)
  (cond [(number? exp) 0]
        [(symbol? exp) (if (eq? exp var) 1 0)]
        [(eq? (first exp) '+) (list '+ (diff var (second exp)) (diff var (third exp)))]
        [(eq? (first exp) '*)
         (list '+
               (list '* (second exp) (diff var (third exp)))
               (list '* (diff var (second exp)) (third exp)))]
        [else 'unknown-expression]))

;;;;; Part 3: Meta-circular evaluator

(define (my-eval rexp)
  (let my-eval-env ([rexp rexp]
                    [env '()])
    (cond [(symbol? rexp) (lookup rexp env)]
          [(eq? (first rexp) 'lambda)
           (list 'closure (second rexp) (third rexp) env)]
          [(list? rexp)
           (let* ([fn (my-eval-env (first rexp) env)]
                  [args (map (lambda (arg) (my-eval-env arg env)) (cdr rexp))])
             (if (eq? (first fn) 'closure)
                 (let ([params (second fn)]
                       [body (third fn)]
                       [closure-env (fourth fn)])
                   (my-eval-env body (append (map cons params args) closure-env)))
                 (apply fn args))))
          [else rexp])))

;;;;; Part 4: Free variables

(define (free sexp)
  (cond [(symbol? sexp) (list sexp)]
        [(not (list? sexp)) '()]
        [(eq? (first sexp) 'lambda) (free (third sexp))]
        [else (remove-duplicates (append (free (first sexp)) (free (cdr sexp))))]))

;;;;; Extra credit: algebraic simplification (add your own tests)

(define (simplify exp)
  (cond [(number? exp) exp]
        [(symbol? exp) exp]
        [(eq? (first exp) '+)
         (let ([a (simplify (second exp))] [b (simplify (third exp))])
           (cond [(and (number? a) (number? b)) (+ a b)]
                 [(equal? a 0) b]
                 [(equal? b 0) a]
                 [else (list '+ a b)]))]
        [(eq? (first exp) '*)
         (let ([a (simplify (second exp))] [b (simplify (third exp))])
           (cond [(or (equal? a 0) (equal? b 0)) 0]
                 [(equal? a 1) b]
                 [(equal? b 1) a]
                 [else (list '* a b)]))]
        [else exp]))
