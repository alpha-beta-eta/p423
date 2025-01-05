#lang racket
(require "parse-scheme.rkt"
         "convert-complex-datum.rkt"
         "uncover-assigned.rkt"
         "purify-letrec.rkt"
         
         )
(define compil
  (compose
   
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