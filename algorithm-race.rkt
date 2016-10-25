#lang racket

(require "chairs.rkt"
         "simulated-annealing.rkt"
         "clonal-selection.rkt"
         "ant-colony-optimization.rkt"
         "given.rkt"
         threading
         data/collection
         gregor
         gregor/period)

(provide main
         algorithm-stop-after)

(define algorithm-stop-after (make-parameter (hours 8)))

(define (display-state best-state population iteration)
  (printf "~a ~a ~a ~a\n"
          (~t (now) "yyyy-MM-dd HH:mm:ss")
          (~r iteration #:min-width 5)
          (~> best-state
              state-display-energy
              exact->inexact
              (~r #:precision '(= 4) #:min-width 7))
          (~>> population
               (map (λ~> state-display-energy
                         exact->inexact
                         (~r #:precision '(= 4) #:min-width 7)))
               sequence->list)))

(define (make-updater #:at-step [at-step 1])
  (define start (now))
  (define best-energy +inf.0)
  
  (define (updater best-state population iteration)
    (when (zero? (remainder iteration at-step))
      (real-updater best-state population iteration)))
  
  (define (real-updater best-state population iteration)
    (define best (state-place best-state))
    (display-state best-state population iteration)
    (when (datetime>=? (now) (+period start (algorithm-stop-after)))
      (break-thread (current-thread))))
  updater)

(define (main)
  
  (parameterize ([annealing-cycle 8959]
                   [temperature-center 1336]
                   [temperature-height 1.9338]
                   [temperature-slope 0.7612])
      (define updater (make-updater #:at-step 100))
      (simulated-annealing (place-random) #:updater updater))
  
  #;(parameterize ([make-state-proc make-state]
                 [make-state-randomized-proc make-state-random]
                 [state-energy-proc state-energy]
                 [state-internal-proc state-place]
                 [mutation-operators (list place-random-change1
                                           place-random-change2
                                           place-random-change3
                                           place-random-change4
                                           place-random-change5)])
    (define updater (make-updater #:at-step 1))
    (clonal-selection (place-random) #:updater updater))
  
  #;(define updater (make-updater #:at-step 1))
  #;(ant-colony-optimization (place-random) #:updater updater))