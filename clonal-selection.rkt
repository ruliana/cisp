#lang s-exp "rocket.rkt"

(require racket/random
         (only-in racket/list make-list))

(provide clonal-selection
         ;parameters
         max-iterations
         population-size
         population-size-best
         clone-rate
         ;adapters
         make-state-proc
         make-state-randomized-proc
         state-energy-proc
         state-internal-proc
         mutation-operators)

; Parameters
(define max-iterations (make-parameter 10000))
(define population-size (make-parameter 20))
(define population-size-best (make-parameter 20))
(define clone-rate (make-parameter 3))

(define make-state-proc (make-parameter #f))
(define make-state-randomized-proc (make-parameter #f))
(define state-energy-proc (make-parameter #f))
(define state-internal-proc (make-parameter #f))
(define mutation-operators (make-parameter (list)))

(define (make-state individual)
  ((make-state-proc) individual))

(define (make-state-randomized)
  ((make-state-randomized-proc)))

(define (state-energy state)
  ((state-energy-proc) state))

(define (state-internal state)
  ((state-internal-proc) state))

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

(define (make-population-random size)
  (for/list ([_x (range size)])
    (make-state-randomized)))

(define (initialize-population)
  (make-population-random (population-size)))

(define (solution? population)
  (ormap (λ (p) (zero? (state-energy p))) population))

(define (select-best size . populations)
  (define pops (sequence->list (append* populations)))
  (take size (sort pops < #:key state-energy)))

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
  (given [data (state-internal element)]
         [mutagenes (mutation-operators)]
         [mutant (for/fold ([rslt data])
                           ([_i (range mutations)])
                   (let ([mutagene (random-ref mutagenes)])
                     (mutagene rslt)))])
  (make-state mutant))

(define (complete-with-random population)
  (given [current-size (length population)]
         [missing (- (population-size) current-size)])
  (append population (make-population-random missing)))