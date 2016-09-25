#lang s-exp "rocket.rkt"

(require "chairs.rkt"
         (only-in racket/list make-list))

(provide main)

; Parameters
(define max-iterations (make-parameter 10000))
(define population-size (make-parameter 25))
(define population-size-best (make-parameter 10))
(define clone-rate (make-parameter 10))

; Structs
(struct clones (number original) #:transparent)

; Algorithm
(define (clonal-selection)
  (let loop ([iteration (max-iterations)]
             [population (initialize-population)])
    (cond
      [(zero? iteration) population]     ; stop condition
      [(solution? population) population] ; stop condition
      [else (loop (sub1 iteration)
                  (select-and-mutate iteration population))])))

(define (select-and-mutate iteration population)
  (let* ([population-best (select-best (population-size-best) population)]
         [clones (clone population-best)]
         [mutants (mutate clones)]
         #;[best-of-all (select-best (population-size) mutants)]
         #;[population-new (complete-with-random best-of-all)])
    (printf "~a ~a\n"
            (~r iteration #:min-width 5)
            (~>> population-best (map state-energy) sequence->list))
    mutants))

; State

(struct state (energy place) #:transparent)

(define (make-state a-place)
  (state (energy a-place) a-place))

(define (make-state-random)
  (make-state (place-random)))

; Interface with "chairs"

(define (make-population-random size)
  (for/list ([_x (range size)])
    (make-state-random)))

(define (initialize-population)
  (make-population-random (population-size)))

(define (solution? population)
  (ormap (λ (p) (zero? (state-energy p))) population))

(define affinity state-energy)

(define (select-best size populations)
  (define pops (sequence->list populations))
  (take size (sort pops < #:key affinity)))

(define (clone population-sorted)
  (given [p-size (population-size)]
         [c-rate (clone-rate)])
  (for/list ([i (in-naturals 1)]
             [p (in population-sorted)])
    (define number-of-clones (ceiling (* p-size c-rate (/ 1 i))))
    (clones number-of-clones p)))

(define (mutate population-cloned-sorted)
  (define best (first population-cloned-sorted))
  (define original (list (clones-original best)))
  (define mutants
    (for/list ([i (in-naturals 1)]
               [c (in population-cloned-sorted)])
      (for/list ([_n (range (clones-number c))])
        (mutate-element (clones-original c) i))))
  (append* original mutants))

(define (mutate-element element mutations)
  (given [a-place (state-place element)]
         [mutant  (for/fold ([rslt a-place])
                            ([_i (range mutations)])
                    (place-random-change rslt))])
  (state (energy mutant) mutant))

(define (complete-with-random population)
  (given [current-size (length population)]
         [missing (- (population-size) current-size)])
  (append population (make-population-random missing)))

(define main clonal-selection)