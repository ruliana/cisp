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

(define algorithm-stop-after (make-parameter (minutes 2)))

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
  
  (define rslt 
    (parameterize ([population-size 5]
                   [population-size-best 2]
                   [clone-rate 2]
                   [make-state-proc ann-make-state]
                   [make-state-randomized-proc ann-make-state-randomized]
                   [state-energy-proc ann-state-energy]
                   [state-internal-proc ann-state-internal]
                   [mutation-operators (list ann-mut-operator1
                                             ann-mut-operator2
                                             ann-mut-operator3
                                             ann-mut-operator4)])
      (define updater (make-updater #:at-step 1))
      (clonal-selection)))
  
  #;(define updater (make-updater #:at-step 1))
  #;(clonal-selection (place-random) #:updater updater)
  
  #;(define updater (make-updater #:at-step 1))
  #;(ant-colony-optimization (place-random) #:updater updater)
  
  rslt)

;                                                                 ;  
;    ;;;  ;                   ;                                   ;  
;   ;     ;                   ;                                   ;  
;   ;                        ; ;                                  ;  
;   ;     ;   ;;;;;;;;       ; ;    ;;;;;   ;;;;;    ;;;    ;;;   ;  
;    ;    ;   ;   ;   ;      ; ;    ;    ;  ;    ;  ;   ;      ;  ;  
;     ;   ;   ;   ;   ;     ;   ;   ;    ;  ;    ;  ;   ;      ;  ;  
;      ;  ;   ;   ;   ;     ;   ;   ;    ;  ;    ;  ;;;;;   ;;;;  ;  
;      ;  ;   ;   ;   ;     ;;;;;   ;    ;  ;    ;  ;      ;   ;  ;  
;      ;  ;   ;   ;   ;     ;   ;   ;    ;  ;    ;  ;      ;   ;  ;  
;      ;  ;   ;   ;   ;    ;     ;  ;    ;  ;    ;  ;      ;   ;  ;  
;   ;;;   ;   ;   ;   ;    ;     ;  ;    ;  ;    ;   ;;;;   ;;;;  ;; 
;                                                                    

(struct ann-params (annealing-cycle
                    temperature-center
                    temperature-height
                    temperature-slope)
  #:transparent)

(struct ann-state (energy internal) #:transparent)

(define (ann-make-state params)
  (ann-state (ann-calculate-energy params) params))

(define (ann-make-state-randomized)
  (given annealing-cycle (random 1 10000)
         temperature-center (random 1 10000)
         temperature-height (/ (random 1 100) 10)
         temperature-slope (random))
  (ann-make-state (ann-params annealing-cycle
                              temperature-center
                              temperature-height
                              temperature-slope)))

(define (ann-calculate-energy params)
  (parameterize ([annealing-cycle (ann-params-annealing-cycle params)]
                 [temperature-center (ann-params-temperature-center params)]
                 [temperature-height (ann-params-temperature-height params)]
                 [temperature-slope (ann-params-temperature-slope params)])
    (printf "====> ~a ~a ~a ~a\n"
            (annealing-cycle)
            (temperature-center)
            (temperature-height)
            (temperature-slope))
    (define ann-updater (make-updater #:at-step 100))
    (simulated-annealing (place-random) #:updater ann-updater)))

(define (ann-mut-operator1 params)
  (given value (ann-params-annealing-cycle params)
         delta (+ 0.5 (random)))
  (struct-copy ann-params params [annealing-cycle (round (* value delta))]))

(define (ann-mut-operator2 params)
  (given value (ann-params-temperature-center params)
         delta (+ 0.5 (random)))
  (struct-copy ann-params params [temperature-center (* value delta)]))

(define (ann-mut-operator3 params)
  (given value (ann-params-temperature-height params)
         delta (+ 0.5 (random)))
  (struct-copy ann-params params [temperature-height (* value delta)]))

(define (ann-mut-operator4 params)
  (given value (ann-params-temperature-slope params)
         delta (+ 0.5 (random)))
  (struct-copy ann-params params [temperature-slope (* value delta)]))