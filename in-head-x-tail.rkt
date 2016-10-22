#lang racket/base

(require racket/generator
         data/collection)

(provide in-head-x-tail)

(define (in-head-x-tail seq)
  (in-generator
   #:arity 3
   (let loop ([head #()]
              [x (first seq)]
              [tail (rest seq)])
     (yield head x tail)
     (unless (empty? tail)
       (loop (conj head x) (first tail) (rest tail))))))