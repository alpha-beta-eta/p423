#lang racket
(require "utils.rkt"
         "parse-scheme.rkt"
         "convert-complex-datum.rkt"
         "uncover-assigned.rkt"
         "purify-letrec.rkt"
         "convert-assignments.rkt"
         "optimize-direct-call.rkt"
         "remove-anonymous-lambda.rkt"
         "sanitize-binding-forms.rkt"
         "uncover-free.rkt"
         "convert-closures.rkt"
         "optimize-known-call.rkt"
         "introduce-procedure-primitives.rkt"
         "lift-letrec.rkt"
         "normalize-context.rkt"
         "specify-representation.rkt"
         "uncover-locals.rkt"
         "remove-let.rkt"
         "verify-uil.rkt"
         "remove-complex-operas.rkt"
         "flatten-set.rkt"
         "impose-calling-conventions.rkt"
         "expose-allocation-pointer.rkt"
         
         )
(define compil
  (compose
   
   expose-allocation-pointer
   impose-calling-conventions
   flatten-set!
   remove-complex-opera*
   verify-uil
   remove-let
   uncover-locals
   specify-representation
   normalize-context
   lift-letrec
   introduce-procedure-primitives
   optimize-known-call
   convert-closures
   uncover-free
   sanitize-binding-forms
   remove-anonymous-lambda
   optimize-direct-call
   convert-assignments
   purify-letrec
   uncover-assigned
   convert-complex-datum
   parse-scheme))
(define (test program)
  (pretty-display program)
  (newline)
  (pretty-display (compil program))
  (newline))
(define (test* program*)
  (for-each test program*))
(define (make-procedure label length)
  (define vec (make-vector (+ length 1)))
  (vector-set! vec 0 label)
  vec)
(define (procedure-set! closure index value)
  (vector-set! closure (+ index 1) value))
(define (procedure-code closure)
  (vector-ref closure 0))
(define (procedure-ref closure index)
  (vector-ref closure (+ index 1)))
(define (true) #t)
(define (false) #f)
(define (nop) (void))
(define *memory* (make-vector 1000))
(define allocated 0)
(define (alloc size)
  (if (and (>= size 0) (= (modulo size 8) 0))
      (let ((address allocated))
        (set! allocated (+ allocated (quotient size 8)))
        (* address 8))
      (error 'alloc "invalid size")))
(define (mset! index offset value)
  (unless (or (procedure? value) (int64? value))
    (error 'mset! "value must be a 64-bit fixnum"))
  (let ((x (+ index offset)))
    (if (and (>= x 0) (= (modulo x 8) 0))
        (let ((actual (quotient x 8)))
          (if (< actual allocated)
              (vector-set! *memory* actual value)
              (error 'mset! "try to access unallocated memory")))
        (error 'mset! "invalid arguments:\nindex: ~s\noffset: ~s" index offset))))
(define (mref index offset)
  (let ((x (+ index offset)))
    (if (and (>= x 0) (= (modulo x 8) 0))
        (let ((actual (quotient x 8)))
          (if (< actual allocated)
              (vector-ref *memory* actual)
              (error 'mref "try to access unallocated memory")))
        (error 'mref "invalid arguments"))))
(define (get-result encoding)
  (case (logand encoding #b111)
    ((#b000) (sra encoding 3))
    ((#b001) (cons (get-result (mref encoding (- offset:car tag:pair)))
                   (get-result (mref encoding (- offset:cdr tag:pair)))))
    ((#b010) '<procedure>)
    ((#b011) (box (get-result (mref encoding (- offset:box tag:box)))))
    ((#b110) (cond ((= encoding $false) #f)
                   ((= encoding $true) #t)
                   ((= encoding $nil) '())
                   ((= encoding $void) (void))))
    ((#b111)
     (let ((len (get-result
                 (mref encoding (- offset:vector-length tag:vector)))))
       (let iter ((i len) (result* '()))
         (if (= i 0)
             (apply vector result*)
             (iter (- i 1)
                   (cons (get-result
                          (mref encoding (- (* 8 i) tag:vector)))
                         result*))))))
    (else (error 'get-result "unknown encoding ~s" encoding))))
(define-syntax locals
  (syntax-rules ()
    ((_ (x ...) exp)
     (let ((x (void)) ...) exp))))
(define test-program*
  '((let ((x 0) (y 1) (z 2))
      (let ((counter (lambda ()
                       (set! x (+ x 1))
                       (set! z (+ z 2))
                       (cons x z))))
        (counter)
        (counter)
        (counter)))
    (let ((even? (void))
          (odd? (void)))
      (set! even? (lambda (n)
                    (if (= n 0)
                        #t
                        (odd? (- n 1)))))
      (set! odd? (lambda (n)
                   (if (= n 0)
                       #f
                       (even? (- n 1)))))
      (even? 88))
    (((lambda (h)
        ((lambda (f) (f f))
         (lambda (g)
           (h (lambda (x) ((g g) x))))))
      (lambda (fact)
        (lambda (n)
          (if (= n 0)
              1
              (* n (fact (- n 1)))))))
     10)
    (letrec ((enum (lambda (a b)
                     (if (> a b)
                         '()
                         (cons a (enum (+ a 1) b)))))
             (map (lambda (proc lst)
                    (if (null? lst)
                        '()
                        (cons (proc (car lst))
                              (map proc (cdr lst)))))))
      (map (lambda (x) (* x x)) (enum 1 20)))
    (let ((vector-map
           (lambda (proc vec)
             (let ((l (vector-length vec)))
               (let ((v (make-vector l)))
                 (letrec ((iter (lambda (i)
                                  (if (< i l)
                                      (begin
                                        (vector-set!
                                         v i (proc (vector-ref vec i)))
                                        (iter (+ i 1)))))))
                   (iter 0)
                   v))))))
      (vector-map (lambda (x) (* x x)) '#(1 2 3)))
    ))
;(test* test-program*)