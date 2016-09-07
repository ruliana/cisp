#lang s-exp "rocket.rkt"

(require "chairs.rkt")
(module+ test (require rackunit))

;;(struct status x y place #:transparent)

(define-values (people a-place)
  (let* ([pp (file->list "data/chairs.csv")]
         [pc (make-place 10 10)]
         [pc* (distribute-people pc (take 10 pp))]
         [pp* (sequence->list (drop 10 pp))])
    (values pp* pc*)))


(struct choice (energy place remaining-people) #:transparent)

; Basic Scenario
(module+ test
  (define people (list (make-person "Ronie"   "LCoN" 0 0 "Cris")
                       (make-person "Cris"    "LCoN" 1 0 "Leandro")
                       (make-person "Leandro" "LCoN" 0 1 "Dávila" "Cris")
                       (make-person "Dávila"  "LCoN" 1 1 "Ronie")))
  (define empty-place (make-place 2 2))
  (define ronie-at-1 (place-set empty-place 0 0 (first people))))

; All options possible to fill (x, y) in place.
; (list person) int int place -> (list choice)
(define (choices remaining-people x y a-place)
  (for/list ([(head p tail) (in-head-x-tail remaining-people)])
    (let ([new-place (place-set a-place x y p)])
      (choice (energy new-place) new-place (append head tail)))))

(module+ test
  (test-begin
   (define cs (choices people 0 0 empty-place))
   (check equal? (length cs) 4)
   (check equal? (choice-energy (first cs)) 12)
   (check equal? (length (choice-remaining-people (first cs))) 3))
  (test-begin
   (define cs (choices (rest people) 0 1 ronie-at-1))
   (check equal? (length cs) 3)
   (check equal? (choice-energy (first cs)) 11)
   (check equal? (length (choice-remaining-people (first cs))) 2)))

; Least energy between choices.
; (list choice) -> choice 
(define (best-choice choices)
  (first (sort choices < #:key choice-energy)))

; Random choice distributed by energy.
; (list choice) -> choice
(define (random-choice choices)
  (define sum (for/sum ([c (in choices)]) (choice-energy c)))
  (define rand (random sum))
  (let loop ([r rand]
             [c (first choices)]
             [cs (rest choices)])
    (if (or (negative? r) (empty? cs)) c
        (loop (- r (choice-energy c))
              (first cs)
              (rest cs)))))

(module+ test
  (test-begin
   (define cs (choices (rest people) 0 1 ronie-at-1))
   (check equal? (choice-energy (best-choice cs)) 11)))

(define (greedy-solution people a-place)
  (define-values (pc pp)
    (for*/fold ([pc a-place]
                [pp people])
               ([y (range (place-y a-place))]
                [x (range (place-x a-place))]
                #:when (equal? 'empty-space (person-group (place-ref pc x y)))
                #:break (empty? pp))
      (let ([best (~>> (choices pp x y pc) random-choice)])
        (printf "~a ~a ~a ~a\n" x y (person-name (place-ref (choice-place best) x y)) (choice-energy best))
        (values (choice-place best)
                (choice-remaining-people best)))))
  pc)
