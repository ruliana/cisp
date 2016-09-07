#lang s-exp "rocket.rkt"

(require "chairs.rkt")
(require (only-in racket/list
                  shuffle)
         racket/dict)
;(module+ test (require rackunit))

(define heuristic-coef (make-parameter 2.5))
(define greed-coef (make-parameter 0.9))
(define local-pheromone-coef (make-parameter 0.1))
(define ant-population (make-parameter 10))
(define decay (make-parameter 0.1))
(define init-pher (make-parameter 1/700))

(define-values (people a-place)
  (let* ([pp (file->list "data/chairs.csv")]
         [pc (make-place 10 10)]
         [pc* (distribute-people pc (take 10 pp))]
         [pp* (sequence->list (drop 10 pp))])
    (values pp* pc*)))

(struct choice (prob place remaining-people) #:transparent)

; Basic Scenario
(module+ test
  (define people (list (make-person "Ronie"   "LCoN" 0 0 "Cris")
                       (make-person "Cris"    "LCoN" 1 0 "Leandro")
                       (make-person "Leandro" "LCoN" 0 1 "Dávila" "Cris")
                       (make-person "Dávila"  "LCoN" 1 1 "Ronie")))
  (define empty-place (make-place 2 2))
  (define ronie-at-1 (place-set empty-place 0 0 (first people))))

; All options possible to fill (x, y) in place.
; (list person) int int place (hash (list x y person-name) . value) -> (list choice)
(define (choices remaining-people x y a-place [pheromones (hash)])
  (for/list ([(head p tail) (in-head-x-tail remaining-people)])
    (let* ([remaining (append head tail)]
           [new-place (place-set a-place x y p)]
           [e (energy-at new-place x y)]
           [pher (dict-ref pheromones (list x y (person-name p)) (init-pher))]
           [heur (expt (/ 1 e) (heuristic-coef))]
           [prob (* pher heur)])
      (choice prob new-place remaining))))

#;(module+ test
    (test-begin
     (define cs (choices people 0 0 empty-place))
     (check equal? (length cs) 4)
     (check equal? (choice-prob (first cs)) 12)
     (check equal? (length (choice-remaining-people (first cs))) 3))
    (test-begin
     (define cs (choices (rest people) 0 1 ronie-at-1))
     (check equal? (length cs) 3)
     (check equal? (choice-prob (first cs)) 11)
     (check equal? (length (choice-remaining-people (first cs))) 2)))

; Least probability between choices.
; (list choice) -> choice 
(define (best-choice choices)
  (first (sort choices < #:key choice-prob)))

; Random choice distributed by probability.
; (list choice) -> choice
(define (random-choice choices)
  (define sum (for/sum ([c (in choices)]) (choice-prob c)))
  (define rand (random))
  (let loop ([r rand]
             [c (first choices)]
             [cs (rest choices)])
    (if (or (negative? r) (empty? cs)) c
        (loop (- r (/ (choice-prob c) sum))
              (first cs)
              (rest cs)))))

#;(module+ test
    (test-begin
     (define cs (choices (rest people) 0 1 ronie-at-1))
     (check equal? (choice-prob (best-choice cs)) 11)))

(struct state (energy place))

(define (update-local pheromones a-state)
  (define a-place (state-place a-state))
  (for*/hash ([y (range (place-y a-place))]
              [x (range (place-x a-place))])
    (let ([key (list x y (~> a-place (place-ref x y) person-name))])
      (values key
              (+
               (* (init-pher)
                  (local-pheromone-coef))
               (* (dict-ref pheromones key (init-pher))
                  (- 1.0 (local-pheromone-coef))))))))

(define (update-global pheromones a-state)
  (define a-place (state-place a-state))
  (define e (state-energy a-state))
  (for*/hash ([y (range (place-y a-place))]
              [x (range (place-x a-place))])
    (let ([key (list x y (~> a-place (place-ref x y) person-name))])
      (values key
              (+
               (* (decay) (/ 1.0 e))
               (* (- 1 (decay))
                  (dict-ref pheromones key (init-pher))))))))

(define (solution people a-place)
  (define coords (~>> (for*/list ([x (range (place-x a-place))]
                                  [y (range (place-y a-place))])
                        (cons x y))
                      shuffle))
  (define-values (pc pp)
    (for*/fold ([pc a-place]
                [pp people])
               ([xy (in coords)]
                #:when (equal? 'empty-space (person-group (place-ref pc (car xy) (cdr xy))))
                #:break (empty? pp))
      (let* ([x (car xy)]
             [y (cdr xy)]
             [choicer (if (<= (random) (greed-coef)) best-choice random-choice)]
             [best (choicer (choices pp x y pc))])
        ;(printf "~a ~a ~a ~a\n" x y (person-name (place-ref (choice-place best) x y)) (choice-prob best))
        (values (choice-place best)
                (choice-remaining-people best)))))
  (state (energy pc) pc))

(define (search people a-place)
  (let loop ([steps 1000]
             [ants (ant-population)]
             [pheromones (hash)]
             [best (solution people a-place)])
    (let* ([current (solution people a-place)]
           [best* (if (< (state-energy current) (state-energy best)) current best)]
           [pheromones* (update-local pheromones current)])
      (printf "~a ~a\n" steps (state-energy best*))
      (cond [(> 1 steps) best*]
            [(> 1 ants) (loop (sub1 steps) (ant-population) (update-global pheromones* best*) best*)]
            [else (loop (sub1 steps) (sub1 ants) pheromones* best*)]))))

