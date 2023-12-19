#lang racket
(require "utils.rkt")
;; <program> ::= (letrec ((<label> (lambda () <body>))*) <body>)
;; <tail> ::= (if <pred> <tail> <tail>)
;;         |  (begin <effect>* <tail>)
;;         |  (<triv> <var>*)
;; <pred> ::= (true)
;;         |  (false)
;;         |  (if <pred> <pred> <pred>)
;;         |  (begin <effect>* <pred>)
;;         |  (<relop> <triv> <triv>)
;; <effect> ::= (nop)
;;           |  (if <pred> <effect> <effect>)
;;           |  (begin <effect>* <effect>)
;;           |  (set! <var> <simple>)
;;           |  (return-point <label>
;;                (begin (set! <var> <triv>)* (<triv> <var>*)))
;;           |  (mset! <triv> <triv> <triv>)
;; <simple> ::= <triv>
;;           |  (mref <triv> <triv>)
;;           |  (<binop> <triv> <triv>)
;; <body> ::= (locals (<uvar>*)
;;              (new-frames (<frame>*) <tail>))
;; <frame> ::= (<uvar>*)
;; <triv> ::= <int> | <var> | <label>
;; <var> ::= <uvar> | <loc>
;; <loc> ::= <reg> | <fvar>
;; <binop> ::= + | - | * | sra | logand
;; <relop> ::= = | < | > | <= | >=
