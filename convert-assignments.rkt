#lang racket
(provide convert-assignments)
(require "utils.rkt")
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (set! <uvar> <exp>)
;;        |  (begin <exp>+)
;;        |  (lambda (<uvar>*) (assigned (<uvar>*) <exp>))
;;        |  (let ((<uvar> <exp>)*) (assigned (<uvar>*) <exp>))
;;        |  (letrec ((<uvar> (lambda (<uvar>*) (assigned (<uvar>*) <exp>)))*) <exp>)
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp>*)
(define (convert-assignments exp)
  (define (Let x* e* body)
    (if (null? x*)
        body
        `(let ,(map list x* e*) ,body)))
  (define ((apply-env env) x)
    (cond ((assq x env) => cdr)
          (else x)))
  (define (convert env)
    (lambda (exp)
      (match exp
        ((quote ,i) exp)
        (,x (guard (symbol? x)) ((apply-env env) x))
        ((if ,q ,a ,e)
         (cons 'if (map (convert env) (cdr exp))))
        ((set! ,uvar ,exp)
         `(set-box! ,uvar ,((convert env) exp)))
        ((begin . ,exp*)
         (cons 'begin (map (convert env) exp*)))
        ((lambda ,x* (assigned ,u* ,body))
         (define t*
           (map (lambda (x) (unique-symbol 't)) u*))
         (define ctx (map cons u* t*))
         (define y*
           (map (apply-env ctx) x*))
         (define box*
           (map (lambda (t) `(box ,t)) t*))
         (define env^
           (append (map (lambda (u)
                          (cons u `(unbox ,u))) u*)
                   env))
         (define body^ ((convert env^) body))
         `(lambda ,y*
            ,(Let u* box* body^)))
        ((let ,bds (assigned ,u* ,body))
         (: bds
            (lambda (x* e*)
              (let* ((e* (map (convert env) e*))
                     (env^ (append
                            (map (lambda (u)
                                   (cons u `(unbox ,u))) u*)
                            env))
                     (body ((convert env^) body)))
                (define t*
                  (map (lambda (x) (unique-symbol 't)) u*))
                (define ctx (map cons u* t*))
                (define y*
                  (map (apply-env ctx) x*))
                (define box*
                  (map (lambda (t) `(box ,t)) t*))
                (Let y* e*
                     (Let u* box* body))))))
        ((letrec ,bds ,body)
         (: bds
            (lambda (x* e*)
              (let ((e* (map (convert env) e*))
                    (body ((convert env) body)))
                (Letrec x* e* body)))))
        ((,prim . ,rands)
         (guard (prim? prim))
         (cons prim (map (convert env) rands)))
        ((,rator . ,rands)
         (map (convert env) exp)))))
  ((convert '()) exp))
;; <exp> ::= (quote <immediate>)
;;        |  <uvar>
;;        |  (if <exp> <exp> <exp>)
;;        |  (begin <exp>+)
;;        |  (lambda (<uvar>*) <exp>)
;;        |  (let ((<uvar> <exp>)*) <exp>)
;;        |  (letrec ((<uvar> (lambda (<uvar>*) <exp>))*) <exp>)
;;        |  (<prim> <exp>*)
;;        |  (<exp> <exp>*)