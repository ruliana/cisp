#lang s-exp "rocket.rkt"

(require "chairs.rkt")

(provide simulated-annealing
         ;parameters
         annealing-cycle
         temperature-center
         temperature-height
         temperature-slope)

(define annealing-cycle (make-parameter 3000))
(define temperature-center (make-parameter 1000))
(define temperature-height (make-parameter 6))
(define temperature-slope (make-parameter 0.003))

; == Simulated Annealing

(define (simulated-annealing initial-state [max-steps 3000000] #:updater [updater #f])
  (define box-best (box (state-energy (make-state initial-state))))
  (define cycle-size (annealing-cycle))
  (with-handlers ([exn:break? (λ (_e) (unbox box-best))])
    (define (loop current best step)
      (set-box! box-best (state-energy best))
      (define new (~>> (state-place current) place-random-change make-state))
      (define e-new (state-energy new))
      (define e-best (state-energy best))
      (define e-current (state-energy current))
      (define best* (if (< e-new e-best) new best))
      (define cycle (remainder step (add1 cycle-size)))
      (define temperature (temperature-for cycle))
      (define prob (let ([p (exp (/ (- e-current e-new)
                                    (max 0.000001 temperature)))])
                     (if (> p 1) 1 p)))
      (when updater (updater best* (list current) step))
      (cond
        [(>= 0 e-current) (state-energy current)]
        [(>= step max-steps) (state-energy best*)]
        [(>= cycle cycle-size) (loop best* best* (add1 step))]
        [(should-change temperature e-current e-new) (loop new best* (add1 step))]
        [else (loop current best* (add1 step))]))
    (define first-state (make-state initial-state))
    (loop first-state first-state 0)))

(define (should-change temperature current-energy new-energy)
  (if (>= 0 temperature)
      (< new-energy current-energy)
      (let ([prob (exp (/ (- current-energy new-energy) temperature))])
        (< (random) prob))))

(define (temperature-for cycle)
  (define center (temperature-center))
  (define height (temperature-height))
  (define slope (temperature-slope))
  (/ height (+ 1 (exp (* slope (- cycle center))))))