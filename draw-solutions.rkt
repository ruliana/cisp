#lang s-exp "rocket.rkt"

(require "chairs.rkt"
         "simulated-annealing.rkt"
         "clonal-selection.rkt"
         "ant-colony-optimization.rkt"
         "draw-chairs.rkt"
         racket/class
         racket/gui/base)

(provide main)

(define (make-graphic-updater #:at-step [at-step 1])
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
    (printf "~a ~a ~a\n"
            (~r iteration #:min-width 5)
            (~> population
                first
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
      (displayln "=")))
  updater)

(define (main)
  
  ;(define updater (make-graphic-updater #:at-step 329))
  ;(display-place (state-place (simulated-annealing (place-random) #:updater updater)))
  
  ;(define updater (make-graphic-updater #:at-step 329))
  ;(display-place (state-place (clonal-selection (place-random) #:updater updater)))

  (define updater (make-graphic-updater #:at-step 1))
  (display-place (state-place (ant-colony-optimization (place-random) #:updater updater))))

