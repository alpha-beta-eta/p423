#lang racket
(require "parse-scheme.rkt"
         "convert-complex-datum.rkt"
         "uncover-assigned.rkt"
         )
(define compiler
  (compose
   
   uncover-assigned
   convert-complex-datum
   parse-scheme
   ))
