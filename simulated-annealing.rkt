#lang s-exp "rocket.rkt"

(require "chairs.rkt")

; == Simulated Annealing

(struct state (energy data))

(define (new-state data)
  (state (energy data) data))

(define (simulated-annealing initial-state [max-steps 3000000])
  (define box-best (box initial-state))
  (define cycle-size 3000)
  (with-handlers ([exn:break? (λ (_e) (unbox box-best))])
    (define (loop current best step)
      (set-box! box-best best)
      ; If you want to optimize GC :P
      ;(when (= 0 (remainder step 10)) (collect-garbage 'minor))
      ;(when (= 0 (remainder step 1000)) (collect-garbage 'major))
      (define new (~>> (state-data current) place-random-change new-state))
      (define e-new (state-energy new))
      (define e-best (state-energy best))
      (define e-current (state-energy current))
      (define best* (if (< e-new e-best) new best))
      (define cycle (remainder step (add1 cycle-size)))
      (define temperature (temperature-for cycle))
      (define prob (let ([p (exp (/ (- e-current e-new)
                                    (max 0.000001 temperature)))])
                     (if (> p 1) 1 p)))
      (printf "t:~a s:~a c:~a e/b:~a e/c:~a e/n:~a prob:~a% \n"
              (~r (exact->inexact temperature) #:precision '(= 4))
              (~r step #:min-width 7)
              (~r cycle #:min-width 4)
              (~r (exact->inexact e-best) #:precision '(= 1))
              (~r (exact->inexact e-current) #:precision '(= 1))
              (~r (exact->inexact e-new) #:precision '(= 1))
              (~r (* 100 prob)
                  #:precision '(= 2)
                  #:min-width 6))
      (cond
        [(>= 0 e-current) current]
        [(>= step max-steps) best*]
        [(>= cycle cycle-size) (loop best* best* (add1 step))]
        [(should-change temperature e-current e-new) (loop new best* (add1 step))]
        [else (loop current best* (add1 step))]))
    (define first-state (new-state initial-state))
    (loop first-state first-state 0)))

(define (should-change temperature current-energy new-energy)
  (if (>= 0 temperature)
      (< new-energy current-energy)
      (let ([prob (exp (/ (- current-energy new-energy) temperature))])
        (< (random) prob))))

(define (temperature-for cycle)
  (define center 1000)
  (define height 6)
  (define slope 0.003)
  (/ height (+ 1 (exp (* slope (- cycle center))))))

; == Using (remove it later)

(define a-place (distribute-people (make-place 10 10) (file->list "data/chairs.csv")))
(define rslt (simulated-annealing a-place))
(displayln (state-energy rslt))
(display-place (state-data rslt))