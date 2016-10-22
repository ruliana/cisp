#lang s-exp "rocket.rkt"

(require "chairs.rkt"
         "draw-chairs.rkt"
         racket/class
         racket/gui/base
         racket/random
         (only-in racket/list make-list))

(provide ;main
 clonal-selection
 mutation-operators
 max-iterations
 population-size
 population-size-best
 clone-rate)

; Parameters
(define max-iterations (make-parameter 10000))
(define population-size (make-parameter 20))
(define population-size-best (make-parameter 20))
(define clone-rate (make-parameter 3))
(define mutation-operators (make-parameter (list place-random-change1
                                                 place-random-change2
                                                 place-random-change3
                                                 place-random-change4
                                                 place-random-change5)))

; Structs
(struct clones (number original) #:transparent)

; Algorithm
(define (clonal-selection [sample #f] #:updater [updater #f])
  (define rslt #f)
  (with-handlers ([exn:break? (λ (exn) rslt)])
    (let loop ([iteration (max-iterations)]
               [population (if sample
                               (conj (initialize-population) (make-state sample))
                               (initialize-population))])
      (set! rslt (first population))
      (when updater (updater rslt population iteration)) 
      (cond
        [(zero? iteration) rslt]      ; stop condition
        [(solution? population) rslt] ; stop condition
        [else (loop (sub1 iteration)
                    (select-and-mutate iteration population))]))))

(define (select-and-mutate iteration population)
  (let* ([population-best (select-best (population-size-best) population)]
         [clones (clone population-best)]
         [mutants (mutate clones)]
         [best-of-all (select-best (population-size) mutants population)]
         #;[population-new (complete-with-random best-of-all)])
    best-of-all))

; Interface with "chairs"

(define (make-population-random size)
  (for/list ([_x (range size)])
    (make-state-random)))

(define (initialize-population)
  (make-population-random (population-size)))

(define (solution? population)
  (ormap (λ (p) (zero? (state-energy p))) population))

(define affinity state-energy)

(define (select-best size . populations)
  (define pops (sequence->list (append* populations)))
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
         [mutagenes (mutation-operators)]
         [mutant  (for/fold ([rslt a-place])
                            ([_i (range mutations)])
                    (let ([mutagene (random-ref mutagenes)])
                      (mutagene rslt)))])
  (make-state mutant))

(define (complete-with-random population)
  (given [current-size (length population)]
         [missing (- (population-size) current-size)])
  (append population (make-population-random missing)))

#;(define (main)
    (define best-energy 99999)
    (define canvas (create-canvas (the-place)))
    (define (updater best-state population iteration)
      (define window (send canvas get-parent))
      (define best (state-place best-state))
      (send canvas refresh-now (λ (dc) (draw-heat-map-on-dc dc best) ) #:flush? #t)
      (send window set-label
            (format "Energy: ~a Iteration: ~a"
                    (~> best-state
                        state-display-energy
                        exact->inexact
                        (~r #:precision '(= 4) #:min-width 7))
                    iteration))
      (sleep/yield 0.05)
      (when (< (state-display-energy best-state) best-energy)
        (set! best-energy (state-display-energy best-state))
        (displayln "=")
        (display-place best)
        (displayln "="))
      (printf "~a ~a ~a\n"
              (~r iteration #:min-width 5)
              (~> population first state-display-energy exact->inexact (~r #:precision '(= 4) #:min-width 7))
              (~>> population
                   (map (λ~> state-display-energy exact->inexact (~r #:precision '(= 4) #:min-width 7)))
                   sequence->list)))
    (display-place (state-place (clonal-selection updater))))