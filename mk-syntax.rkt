#lang racket/unit

(require "microk-sig.rkt" "mk-sig.rkt")

(import (prefix μ: microk^))
(export mk^) 

(define disj   μ:disj)
(define conj   μ:conj)
(define relate μ:relate)
(define == μ:==)
(define =/= μ:=/=)
(define symbolo μ:symbolo)
(define stringo μ:stringo)
(define numbero μ:numbero)
(define not-symbolo μ:not-symbolo)
(define not-stringo μ:not-stringo)
(define not-numbero μ:not-numbero)
(define mplus μ:mplus)
(define bind μ:bind)
(define pause μ:pause)
(define mature μ:mature)
(define mature? μ:mature?)

;; Low-level goals
(define succeed (μ:== #t #t))
(define fail    (μ:== #f #t))

;; Queries
(define (stream-take n s)
  (if (eqv? 0 n) '()
    (let ((s (mature s)))
      (if (pair? s)
        (cons (car s) (stream-take (and n (- n 1)) (cdr s)))
        '()))))

(define-syntax run
  (syntax-rules ()
    ((_ n body ...) (map reify/initial-var (stream-take n (query body ...))))))
(define-syntax run*
  (syntax-rules () ((_ body ...) (run #f body ...))))
