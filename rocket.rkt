#lang racket/base
; Those are libraries, functions and macros I use on my own projects
(require racket/function
         racket/match
         racket/string
         racket/format
         racket/generator
         racket/set
         threading
         data/collection
         (only-in racket/list
                  remove-duplicates
                  empty))

(provide (all-from-out racket/base
                       racket/function
                       racket/match
                       racket/string
                       racket/format
                       racket/generator
                       racket/set
                       threading
                       data/collection)
         empty
         remove-duplicates
         (all-defined-out))

; == Language extensions

; procfy
; Make values acts as procs
(define/match ((λ= value) other)
  [((? hash?) _) (hash-ref value other other)]
  [((? sequence?) _) (index-of value other)]
  [(_ _) (equal? value other)])

(define (filter-map proc seq)
  (~>> seq (map proc) (filter identity)))

(define-syntax-rule (define~> (name args ...) body ...)
  (define (name args ... last-arg) (~> last-arg body ...)))

(define-syntax-rule (define~>> (name args ...) body ...)
  (define (name args ... last-arg) (~>> last-arg body ...)))

(define-syntax-rule (define-with data (lets procs) ...)
  (define-values (lets ...) (values (procs data) ...)))

(define (vector-allocate size elem)
  (vector->immutable-vector (make-vector size elem)))

(define (nth* seq . indexes)
  (map (curry nth seq) indexes))

(define (join separator seq)
  (define (join* seq rslt)
    (if (empty? seq) rslt
        (join* (rest seq) (string-append rslt separator (first seq)))))
  (if (empty? seq) ""
      (join* (rest seq) (first seq))))

; Inclusive range =)
(define (from-to n1 n2)
  (range n1 (add1 n2)))

(define (in-head-x-tail seq)
  (in-generator
   #:arity 3
   (let loop ([head #()]
              [x (first seq)]
              [tail (rest seq)])
     (yield head x tail)
     (unless (empty? tail)
       (loop (conj head x) (first tail) (rest tail))))))