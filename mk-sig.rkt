#lang racket/signature

(require "microk-sig.rkt" "common.rkt")

(open microk^)

(define-syntaxes (define-relation)
  (syntax-rules ()
    ((_ (name param ...) g ...)
     (define (name param ...)
       (relate (lambda () (fresh () g ...)) `(,name name ,param ...))))))

succeed
fail

(define-syntaxes (conj*)
  (syntax-rules ()
    ((_)                succeed)
    ((_ g)              g)
    ((_ gs ... g-final) (conj (conj* gs ...) g-final))))

(define-syntaxes (disj*)
  (syntax-rules ()
    ((_)           fail)
    ((_ g)         g)
    ((_ g0 gs ...) (disj g0 (disj* gs ...)))))

(define-syntaxes (fresh)
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((x (var/fresh 'x)) ...) (conj* g0 gs ...)))))

(define-syntaxes (conde)
  (syntax-rules ()
    ((_ (g gs ...) (h hs ...) ...)
     (disj* (conj* g gs ...) (conj* h hs ...) ...))))

;; Queries
(define-syntaxes (query)
  (syntax-rules ()
    ((_ (x ...) g0 gs ...)
     (let ((goal (fresh (x ...) (== (list x ...) initial-var) g0 gs ...)))
       (pause empty-state goal)))))

stream-take

(define-syntaxes (run)
  (syntax-rules ()
    ((_ n body ...) (map reify/initial-var (stream-take n (query body ...))))))

(define-syntaxes (run*)
  (syntax-rules () ((_ body ...) (run #f body ...))))
