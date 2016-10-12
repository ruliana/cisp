#lang s-exp "rocket.rkt"

(require "chairs.rkt"
         "simulated-annealing.rkt"
         "draw-chairs.rkt"
         racket/class
         racket/gui/base)

(define (main)
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
  (display-place (state-place (simulated-annealing (place-random) #:updater updater))))

