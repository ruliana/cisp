#lang s-exp "rocket.rkt"

(require "chairs.rkt")
(require racket/dict
         match-plus
         (only-in racket/list shuffle)
         (prefix-in p: racket/place))

(provide main)

; === Problem ===
;(module+ test (require rackunit))

(define ant-population (make-parameter 21))
(define decay (make-parameter 0.9))
(define number-of-places (make-parameter 3))

(struct choice (prob x y person place remaining-people) #:transparent)

; Basic Scenario
#;(module+ test
    (define people (list (make-person "Ronie"   "LCoN" 0 0 "Cris")
                         (make-person "Cris"    "LCoN" 1 0 "Leandro")
                         (make-person "Leandro" "LCoN" 0 1 "Dávila" "Cris")
                         (make-person "Dávila"  "LCoN" 1 1 "Ronie")))
    (define empty-place (make-place 2 2))
    (define ronie-at-1 (place-set empty-place 0 0 (first people))))

; All options possible to fill (x, y) in place.
; (list person) int int place (hash (list x y person-name) . value) -> (list choice)
(define (choices remaining-people x y a-place pheromones)
  (define pher-ref (λ (p) ((make-pher-ref pheromones) x y p)))
  (define pher-sum
    (sum remaining-people #:key pher-ref))
  (for/list ([(head p tail) (in-head-x-tail remaining-people)])
    (let* ([remaining (append head tail)]
           [pher (pher-ref p)]
           [prob (if (= pher-sum 0) 1 (+ 0.001 (/ pher pher-sum)))])
      (choice prob x y p a-place remaining))))

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

; Random choice distributed by probability.
; (list choice) -> choice
(define (random-choice choices)
  (define choice-sum (sum choices #:key choice-prob))
  (define rand (random))
  (let loop ([r rand]
             [c (first choices)]
             [cs (rest choices)])
    (let ([prob (/ (choice-prob c) choice-sum)])
      (if (or (< r prob) (negative? r) (empty? cs)) c
          (loop (- r prob)
                (first cs)
                (rest cs))))))

; === Pheromones ===
(define (pher-key x y a-person)
  (list x y (person-name a-person)))

(define ((make-pher-ref pheromones) x y a-person)
  (dict-ref pheromones (pher-key x y a-person) 0))

#;(module+ test
    (test-begin
     (define cs (choices (rest people) 0 1 ronie-at-1))
     (check equal? (choice-prob (best-choice cs)) 11)))

(define (pheromomes-update pheromones currents best)
  (define e-best (state-energy best))
  (define local-bests (take 1 (sort currents < #:key state-energy)))
  (define local-best (state-energy (first local-bests)))
  (define goods (~>> local-bests
                     (map (λ (x) (pheromone-gain x local-best)))))
  (apply (dict-merger + 0)
         (conj goods pheromones)))

(define (pheromone-gain a-state best)
  (define a-place (state-place a-state))
  (define energy (state-energy a-state))
  ; How much pheromone to put here:
  (define pheromone-value (+ 10 (- best energy)))
  (define (key x y)
    (pher-key x y (place-ref a-place x y)))
  (define (not-empty-space? x y)
    (not (equal? (person-group (place-ref a-place x y))
                 'empty-space)))
  (for*/hash ([y (range (place-y a-place))]
              [x (range (place-x a-place))]
              #:when (not-empty-space? x y))
    ;(when (and (= 5 x) (= 5 y)) (let* ([k (key x y)]) (printf "~a: ~a\n" k v)))
    (values (key x y) pheromone-value)))

(define decay-matrix
  (matrix 3 3 #(0.00625 0.00625 0.00625
                        0.00625 0.9     0.00625
                        0.00625 0.00625 0.00625)))

(define/match* (pheromones->matrices pheromones (state _ (place x-size y-size _)))
  (define names (~>> pheromones dict-keys (map third) sequence->list list->set))
  (define rslt (for/hash ([name (in names)])
                 (values name (make-matrix y-size x-size))))
  (for ([(k v) (in-dict pheromones)])
    (match-define (list x y name) k)
    (~> (ref rslt name) (matrix-set! y x v)))
  rslt)

(define (matrices->pheromones matrices)
  (for*/fold ([rslt (hash)])
             ([(name hsh) (in-dict matrices)]
              [(y x v) (in-matrix hsh)])
    (dict-set rslt (list x y name) v)))

(define (matrices-convolution matrices)
  (for/hash ([(name mtrx) (in-dict matrices)])
    (values name (matrix-convolution mtrx decay-matrix))))

(define (pheromone-decay pheromones a-state)
  (~> pheromones
      (pheromones->matrices a-state)
      (matrices-convolution)
      (matrices->pheromones)))

; === Solution Builder ===

(struct state (energy place) #:prefab)

(define (solution people a-place pheromones)
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
             [choosen (random-choice (choices pp x y pc pheromones))])
        (values (place-set (choice-place choosen)
                           (choice-x choosen)
                           (choice-y choosen)
                           (choice-person choosen))
                (choice-remaining-people choosen)))))
  (state (energy pc) pc))

; === Distributed ===

(define (make-solution-place people a-place)
  (define p
    (p:place ch
             (match-define (list people a-place) (p:place-channel-get ch))
             (let loop ()
               (let* ([pheromones+best (p:place-channel-get ch)]
                      [pheromones (first pheromones+best)]
                      [best (second pheromones+best)]
                      [current (solution people a-place pheromones)]
                      [pheromones* (if (<= (state-energy current) (+ best 10))
                                       (pheromone-gain current best)
                                       (hash))])
                 (p:place-channel-put ch (list current pheromones*)))
               (loop))))
  (p:place-channel-put p (list people a-place))
  p)

(define (make-places number-of-places people a-place)
  (for/list ([i (in-range number-of-places)])
    (make-solution-place people a-place)))

(define (put-pheromones places pheromones best)
  (for ([p places])
    (p:place-channel-put p (list pheromones best))))

(define (get-pheromones places)
  (for/list ([p places]) (p:place-channel-get p)))

(define (search-distributed people a-place)
  (define threads (make-places (number-of-places) people a-place))
  (let loop ([steps 10000]
             [ants (ant-population)]
             [pheromones (hash)]
             [best (solution people a-place (hash))])
    (put-pheromones threads pheromones (state-energy best))
    (let* ([cphs (get-pheromones threads)]
           [states (map first cphs)]
           [best* (find-min (conj states best) #:key state-energy)]
           [pheromones* (apply (dict-merger + 0) (conj (map second cphs) pheromones))])
      (printf "~a ~a ~a ~a\n" steps ants
              (state-energy best*)
              (sequence->list (map state-energy states)))
      (when (> 1 ants) (display-pher pheromones*))
      (cond [(> 1 steps) best*]
            [(> 1 ants) (loop (- steps (number-of-places))
                              (ant-population)
                              (pheromone-decay pheromones* best*)
                              best*)]
            [else (loop (- steps (number-of-places))
                        (- ants (number-of-places))
                        pheromones*
                        best*)]))))

(define (search people a-place)
  (let loop ([steps 10000]
             [pheromones (hash)]
             [best (solution people a-place (hash))])
    (let* ([currents (for/list ([_i (in-range (ant-population))])
                       (solution people a-place pheromones))]
           [best* (find-min (conj currents best) #:key state-energy)]
           [pheromones* (pheromomes-update pheromones currents best)])
      (printf "~a ~a ~a\n"
              steps
              (state-energy best*)
              (sequence->list (map state-energy currents)))
      (display-pher pheromones* best*)
      (if (> 1 steps) best*
          (loop (sub1 steps)
                (pheromone-decay pheromones* best*)
                best*)))))

(define (display-pher pheromones a-state)
  (~> pheromones
      (pheromones->matrices a-state)
      (dict-ref "Bart")
      (matrix-display (λ (v) (~r v #:min-width 9 #:precision '(= 3))))))


#;(define (display-pher pheromones)
    (define x 5)
    (define y 5)
    (define ps
      (~>> (map person-name people)
           (map (λ (n) (cons n (dict-ref pheromones (list x y n) 0))))
           sequence->list
           (sort _ > #:key cdr)
           (take 5)))
    (for ([v ps]
          #:when (> (cdr v) 0.000001))
      (printf "~ax~a ~a: ~a\n" x y (car v) (cdr v))))

(define (main)
  (~> (search (people) (the-place)) state-place display-place))