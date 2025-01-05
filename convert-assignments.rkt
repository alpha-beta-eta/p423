#lang racket
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