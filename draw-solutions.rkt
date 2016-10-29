#lang s-exp "rocket.rkt"

(require "chairs.rkt"
         "simulated-annealing.rkt"
         "clonal-selection.rkt"
         "ant-colony-optimization.rkt"
         "draw-chairs.rkt"
         gregor
         gregor/period
         racket/class
         racket/gui/base)

(provide main
         algorithm-stop-after)

(define algorithm-stop-after (make-parameter (minutes 30)))

(define (make-graphic-updater #:at-step [at-step 1])
  (define start (now))
  (define best-energy +inf.0)
  (define canvas (create-canvas (the-place)))
  (define (updater best-state population iteration)
    (when (zero? (remainder iteration at-step)) (real-updater best-state population iteration)))
  (define (real-updater best-state population iteration)
    (define window (send canvas get-parent))
    (define best (state-place best-state))
    (send canvas refresh-now (λ (dc) (draw-heat-map-on-dc dc best) ) #:flush? #t)
    (send window set-label
          (format "Energy: ~a | Iteration: ~a | Step: ~a"
                  (~> best-state
                      state-display-energy
                      exact->inexact
                      (~r #:precision '(= 4) #:min-width 7))
                  (~r (/ iteration at-step) #:min-width 4)
                  (~r iteration #:min-width 6)))
    (sleep/yield 0.05)
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
                 sequence->list))
    (when (< (state-display-energy best-state) best-energy)
      (set! best-energy (state-display-energy best-state))
      (displayln "=")
      (display-place best)
      (displayln "="))
    (when (datetime>=? (now) (+period start (algorithm-stop-after))) (break-thread (current-thread))))
  updater)

(define (main)
  ;Cycle:  4484, Center: 1444, Height:  8.100, Slope:  0.181, Energy: 39.309
  ;Cycle:  4484, Center:  935, Height:  8.100, Slope:  0.181, Energy: 39.755
  ;Cycle:  4484, Center: 1444, Height:  8.100, Slope:  0.097, Energy: 39.772  
  (parameterize ([annealing-cycle 8959]
                 [temperature-center 1336]
                 [temperature-height 1.9338]
                 [temperature-slope 0.7612])
    (define updater (make-graphic-updater #:at-step 100))
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
      (define updater (make-graphic-updater #:at-step 1))
      (clonal-selection (place-random) #:updater updater))
  
  #;(define updater (make-graphic-updater #:at-step 1))
  #;(ant-colony-optimization (place-random) #:updater updater))

