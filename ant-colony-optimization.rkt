#lang s-exp "rocket.rkt"

(require "chairs.rkt")
(require racket/dict
         match-plus
         (only-in racket/list shuffle))

(provide ;main
 ant-colony-optimization)

; === Problem ===
;(module+ test (require rackunit))

(define ant-population (make-parameter 21))
(define decay (make-parameter 0.9))

(define (ant-colony-optimization initial-state #:updater [updater #f])
  (define rslt #f)
  (with-handlers ([exn:break? (λ (exn) rslt)])
    (let loop ([steps 10000]
               [pheromones (hash)]
               [best (make-state initial-state)])
      (let* ([currents (for/list ([_i (in-range (ant-population))])
                         (solution pheromones))]
             [best* (find-min (conj currents best) #:key state-energy)]
             [pheromones* (pheromomes-update pheromones currents best)])
        (set! rslt best*)
        (when updater (updater best* currents steps))
        (if (> 1 steps) best*
            (loop (sub1 steps)
                  (pheromone-decay pheromones* best*)
                  best*))))))

(struct choice (prob x y person place remaining-people) #:transparent)

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

(define/match* (pheromones->matrices pheromones (state _ _ (place x-size y-size _ _)))
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

(define (solution pheromones)
  (define some-people (people))
  (define a-place (the-place))
  (define coords (~>> (for*/list ([x (range (place-x a-place))]
                                  [y (range (place-y a-place))])
                        (cons x y))
                      shuffle))
  (define-values (pc pp)
    (for*/fold ([pc a-place]
                [pp some-people])
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
  (make-state pc))

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

#;(define (main)
    (~> (ant-colony-optimization (place-random)) state-place display-place))