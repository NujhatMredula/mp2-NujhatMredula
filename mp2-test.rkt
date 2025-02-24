#lang racket

(require rackunit
         "mp2.rkt")

(test-case "test-deep-map"
           (check-equal? (deep-map add1 '()) '())
           (check-equal? (deep-map add1 (range 10)) (range 1 11))
           (check-equal? (deep-map add1 '(1 . 2)) '(2 . 3))
           (check-equal? (deep-map string-length '("hello" ("how" . "are") (("you") "today?")))
                         '(5 (3 . 3) ((3) 6)))
           (check-equal? (deep-map void '(())) '(()))
           (check-equal? (deep-map void '(() (()) ((() ())))) '(() (()) ((() ()))))
           (check-equal? (deep-map (lambda (x) (* 2 (+ x 1)))
                                   '(1 2 (3 . 4) (5 6 . 7) 8))
                         '(4 6 (8 . 10) (12 14 . 16) 18)))

(test-case "test-currying"
           (define f1 (lambda (x) (+ x 100)))
           (define f2 (lambda (x y) (* x y)))
           (define f3 (lambda (x y z) (* x (+ y z))))
           (check-equal? ((my-curry (lambda () #t))) #t)
           (check-equal? (my-curry f1 1) 101)
           (check-equal? ((my-curry f1) 1) 101)
           (check-equal? (my-curry f2 1 2) 2)
           (check-equal? ((my-curry f2) 1 2) 2)
           (check-equal? (((my-curry f2) 1) 2) 2)
           (check-equal? (my-curry f3 2 3 4) 14)
           (check-equal? ((my-curry f3 2 3) 4) 14)
           (check-equal? ((my-curry f3 2) 3 4) 14)
           (check-equal? (((my-curry f3 2) 3) 4) 14)
           (check-equal? ((((my-curry f3) 2) 3) 4) 14))

(test-case "lookup"
           (check-equal? (lookup 'a '((a . apple) (b . bee) (c . cat)))
                         'apple)
           (check-equal? (lookup 2 '((1 . "one") (2 "two" "three") (4 "four" "five")))
                         '("two" "three"))
           (check-equal? (lookup 'foo '((a . apple) (2 . "two")))
                         #f))

(test-case "update"
           (check-match (update 'a 'apple '((b . bee) (c . cat)))
                        (list-no-order '(a . apple) '(b . bee) '(c . cat)))
           (check-match (update 'a "auto" '((a . apple) (b . bee) (c . cat)))
                        (list-no-order '(a . "auto") '(b . bee) '(c . cat)))
           (check-match (update 1 (list 100 200 300) '())
                        (list '(1 100 200 300))))

(test-case "test-make-object"
           (define obj (make-object 'foo))
           (obj 'set 'name 'bar)
           (obj 'set 'x 42)
           (obj 'update 'x (lambda (x) (* x 100)))
           (obj 'set 'y 100)
           (check-equal? (obj 'get 'name) 'bar)
           (check-equal? (obj 'get 'x) 4200)
           (check-equal? (obj 'get 'y) 100))

(test-case "test-my-eval-1"
           (define rexp '(lambda (x) x))
           (define val (my-eval rexp))
           (define res (val 42))
           (check-equal? res 42))

(test-case "test-my-eval-2"
           (define rexp '((lambda (x) x) (lambda (y) y)))
           (define val (my-eval rexp))
           (define res (val 42))
           (check-equal? res 42))

(test-case "test-my-eval-3"
           (define exp '(lambda (f) 
                          (lambda (x) 
                            (f (f x)))))
           (define val (my-eval exp))
           (define res ((val (lambda (x) (* x x))) 8))
           (check-equal? res 4096))

(test-case "test-free-1"
           (define exp '(lambda (x) y))
           (check-equal? (free exp) '(y)))

(test-case "test-free-1"
           (define exp '((lambda (x) y) (lambda (y) z)))
           (check-equal? (list->set (free exp)) (set 'y 'z)))

(test-case "test-free-2"
           (define exp '(lambda (x)
                          (lambda (y)
                            ((lambda (z) (x (w z))) (y z)))))
           (check-equal? (list->set (free exp)) (set 'w 'z)))
