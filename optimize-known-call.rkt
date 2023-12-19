#lang racket
(provide optimize-known-call)
(require "utils.rkt")
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<label> (lambda (<uvar> <uvar>*)
;;                               (bind-free (<uvar> <uvar>*) <exp>)))*)
;;             (closures ((<uvar> <label> <uvar>*)*) <exp>))
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp> <exp>*)
(define (optimize-known-call exp)
  (define (lookup rator env)
    (cond ((assq rator env) => cdr)
          (else rator)))
  (define ((optimize env) exp)
    (match exp
      ((quote ,i) exp)
      (,x (guard (symbol? x)) x)
      ((if ,q ,a ,e)
       `(if ,((optimize env) q)
            ,((optimize env) a)
            ,((optimize env) e)))
      ((begin . ,exp*)
       (cons 'begin (map (optimize env) exp*)))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (let ((e* (map (optimize env) e*))
                  (body ((optimize env) body)))
              (Let x* e* body)))))
      ((letrec ,bds (closures ,closures ,body))
       (define env^
         (append
          (map
           (lambda (c)
             (match c
               ((,name ,label . ,free*)
                (cons name label))))
           closures)
          env))
       (: bds
          (lambda (x* e*)
            (let ((e* (map (lambda (e)
                             (match e
                               ((lambda ,x*
                                  (bind-free ,y* ,body))
                                `(lambda ,x*
                                   (bind-free
                                    ,y* ,((optimize env^) body))))))
                           e*))
                  (body ((optimize env^) body)))
              (Letrec
               x* e*
               `(closures ,closures ,body))))))
      ((,prim . ,rands)
       (guard (prim? prim))
       (cons prim (map (optimize env) rands)))
      ((,rator . ,rands)
       (let ((rator ((optimize env) rator))
             (rands (map (optimize env) rands)))
         (if (symbol? rator)
             (cons (lookup rator env) rands)
             (cons rator rands))))))
  ((optimize '()) exp))
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<label> (lambda (<uvar> <uvar>*)
;;                               (bind-free (<uvar> <uvar>*) <exp>)))*)
;;             (closures ((<uvar> <label> <uvar>*)*) <exp>))
;;        |  (<prim> <exp>*)
;;        |  (<label> <exp> <exp>*)
;;        |  (<exp> <exp> <exp>*)