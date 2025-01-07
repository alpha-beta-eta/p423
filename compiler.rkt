#lang racket
(require "parse-scheme.rkt"
         "convert-complex-datum.rkt"
         "uncover-assigned.rkt"
         "purify-letrec.rkt"
         "convert-assignments.rkt"
         "optimize-direct-call.rkt"
         "remove-anonymous-lambda.rkt"
         "sanitize-binding-forms.rkt"
         "uncover-free.rkt"
         
         )
(define compil
  (compose
   
   uncover-free
   sanitize-binding-forms
   remove-anonymous-lambda
   optimize-direct-call
   convert-assignments
   purify-letrec
   uncover-assigned
   convert-complex-datum
   parse-scheme
   ))
#;
(compil
 '(letrec ((even? (lambda (n)
                    (if (= n 0)
                        #t
                        (odd? (- n 1)))))
           (odd? (lambda (n)
                   (if (= n 0)
                       #f
                       (even? (- n 1))))))
    (even? 88)))
#;
(compil
 '(letrec ((f (lambda ()
                (set! f g)
                1))
           (g (lambda () 0)))
    (f)))
#;
(compil
 '(let ((counter (let ((x 0))
                   (lambda ()
                     (set! x (+ x 1))
                     x))))
    (counter)
    (counter)
    (counter)))
#;
(compil
 '(let ((x 0) (y 1) (z 2))
    (let ((counter (lambda ()
                     (set! x (+ x 1))
                     (set! z (+ z 2))
                     (cons x z))))
      (counter)
      (counter)
      (counter))))
#;
(compil
 '(((lambda (x)
      (lambda (y)
        (+ x y)))
    1) 2))
#;
(compil
 '((lambda (x y)
     (set! x (* x x))
     (set! y (* y y))
     (+ x y))
   3 4))
#;
(compil
 '(let ((compose (lambda (f g)
                   (lambda (x)
                     (f (g x))))))
    ((compose (lambda (x) (* x x))
              (lambda (x) (+ x 3)))
     13)))
#;
(compil
 '(let ((even? (void))
        (odd? (void)))
    (set! even? (lambda (n)
                  (if (= n 0)
                      #t
                      (odd? (- n 1)))))
    (set! odd? (lambda (n)
                 (if (= n 0)
                     #f
                     (even? (- n 1)))))
    (even? 88)))
#;
(compil '(begin 1 (begin 2 (begin (begin 3 4) 5) 6) 7))
#;
(compil
 '(((lambda (h)
      ((lambda (f) (f f))
       (lambda (g)
         (h (lambda (x) ((g g) x))))))
    (lambda (fact)
      (lambda (n)
        (if (= n 0)
            1
            (* n (fact (- n 1)))))))
   10))
