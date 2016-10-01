#lang s-exp "rocket.rkt"

(require (only-in racket/list shuffle)
         racket/dict
         racket/class
         racket/draw
         (only-in racket/gui image-snip% frame% canvas%)
         "colors.rkt")

(provide (struct-out person)
         make-person
         wall
         screen
         empty-space
         file->list
         ; Positions (matrix)
         (struct-out place)
         make-place
         place-ref
         place-set
         place-random
         place-random-change
         place-around
         distribute-people
         display-place
         ; Cost function / Fitness function
         (rename-out [energy-at2 energy-at]
                     [energy2 energy])
         people
         the-place
         create-canvas
         draw-heat-map-on-dc
         draw-heat-map-on-bitmap)

(module+ test (require rackunit))

(define-syntax-rule (make-scenario max-x max-y (name x y friends ...) ...)
  (distribute-people (make-place max-x max-y)
                     (list (make-person name "Grupo Fixo" x y friends ...) ...)))

; == Struct protocol

(struct person (name group x y friends) #:prefab)

(define (wall x y)
  (person "Parede" 'wall x y empty))

(define (screen x y)
  (person "Monitor" 'wall x y empty))

(define (empty-space x y)
  (person "Vazio" 'empty-space x y empty))

(define/match (make-person name group x y . friends) 
  [("" _ _ _ _)       (empty-space x y)]
  [("Vazio" _ _ _ _)  (empty-space x y)]
  [("Parede" _ _ _ _) (wall x y)]
  [("Monitor" _ _ _ _) (screen x y)]
  [(_ _ _ _ _)        (person name
                              group
                              x y
                              (sequence->list (filter non-empty-string? friends)))])

(define (raw->person name group x y . friends)
  (apply make-person name group (string->number x) (string->number y) friends))

; == Load data protocol

(define (file->list file-name)
  (define raw (rest (call-with-input-file file-name read-lines)))
  (map (λ (x) (apply raw->person x)) raw))

(define (read-lines file)
  (for/list ([line (in-lines file)])
    (map string-trim (string-split line "," #:trim? #f))))

(module+ test
  (define (position a-person)
    (define-with a-person
      (x person-x)
      (y person-y))
    (list x y))
  
  (test-case
   "Load file"
   (test-begin
    (define data (file->list "data/chairs.csv"))
    (check equal? (position (first data)) '(0 0))
    (check equal? (position (last data)) '(9 9)))))

; == Place protocol
; Who goes where.

(struct place (x y positions index) #:prefab)

(define (make-place x y)
  (place x y (vector-allocate (* x y) (empty-space x y)) (hash)))

(define (xy->position a-place x y)
  (~> (place-x a-place) (* y) (+ x)))

(define (place-ref a-place x y)
  (define pos (xy->position a-place x y))
  (~> a-place place-positions (ref pos)))

(define (place-set a-place x y value)
  (given [pos (xy->position a-place x y)]
         [new-positions (~> a-place place-positions (set-nth pos value))]
         [new-index (if (member (person-group value) '(wall empty-space))
                        (place-index a-place) ; Don't add walls or empty-space to index
                        (~> a-place place-index (dict-set (person-name value) (cons x y))))])
  (struct-copy place a-place
               [positions new-positions]
               [index new-index]))

(define walls (sequence->list (from-to 0 9)))
(define (place-random-change a-place)
  (define positions (place-positions a-place))
  (define rnd (~> positions length randoms))
  (define (pos+per #:tabu [tabu empty])
    (define pos (~>> rnd
                     (filter (negate (λ= tabu)))
                     first))
    (define per (ref positions pos))
    (values pos per))
  (let*-values ([(p1 v1) (pos+per #:tabu walls)]
                [(p2 v2) (pos+per #:tabu (cons p1 walls))]
                [(new-positions) (~> positions (set-nth p1 v2) (set-nth p2 v1))])
    (struct-copy place a-place [positions new-positions])))

(define (positions-around a-place x y)
  (define row-below (max (sub1 y) 0))
  (define row-above (min (add1 y) (sub1 (place-y a-place))))
  (define col-left  (max (sub1 x) 0))
  (define col-right (min (add1 x) (sub1 (place-x a-place))))
  (for*/list ([row (remove-duplicates (list y row-below row-above))]
              [col (remove-duplicates (list x col-left col-right))]
              #:when (not (and (= x col) (= y row))))
    (xy->position a-place col row)))

(define (place-around a-place x y)
  (apply nth*
         (place-positions a-place)
         (positions-around a-place x y)))

(define (display-place a-place)
  (define row-size (place-x a-place))
  (define positions (map person-name (place-positions a-place)))
  (for* ([row (chunk row-size positions)])
    (displayln (join "\t" (reverse row)))))

(module+ test
  (define old-place (make-place 3 4))
  (define new-place (~> old-place
                        (place-set 1 2 (person "Ronie" "none" 1 2 '()))
                        (place-set 2 2 (person "Cris" "none" 2 2 '()))
                        (place-set 0 0 (wall 0 0))
                        (place-set 1 1 (empty-space 1 1))))
  (test-case
   "Set and recover data from place"
   (test-begin
    (check equal? (place-ref old-place 1 2) (empty-space 3 4))
    (check equal? (person-name (place-ref new-place 1 2)) "Ronie")
    (check equal? (person-name (place-ref new-place 2 2)) "Cris")
    (check equal? (person-group (place-ref new-place 0 0)) 'wall)))
  (test-case
   "Index for fast access to person x y"
   (test-begin
    (check equal? (~> (place-index new-place) (dict-ref "Ronie")) (cons 1 2))
    (check equal? (~> (place-index new-place) (dict-ref "Cris")) (cons 2 2))
    ; Those are NOT in index
    (check equal? (~> (place-index new-place) (dict-ref "Parede" #f)) #f)
    (check equal? (~> (place-index new-place) (dict-ref "Vazio" #f)) #f)))
  (test-case
   "Get positions around somewhere"
   (check equal? (positions-around old-place 1 1) '(3 5 1 0 2 7 6 8))
   (check equal? (positions-around old-place 0 0) '(1 3 4))
   (check equal? (positions-around old-place 2 3) '(10 8 7))
   (check equal? (positions-around old-place 2 1) '(4 2 1 8 7))))

; == Application

(define (distribute-people a-place people)
  (for/fold ([rslt a-place])
            ([a-person (in people)])
    (define-with a-person
      (x person-x)
      (y person-y))
    (place-set rslt x y a-person)))

(define (place-random)
  (define place (the-place))
  (define-values (_ps rslt)
    (for*/fold ([ps (shuffle (people))]
                [rslt place])
               ([x (in-range 0 (place-x place))]
                [y (in-range 1 (place-y place))])
      (define p (first ps))
      (values (rest ps) (place-set rslt x y p))))
  rslt)

(define (name-at a-place x y)
  (~> a-place (place-ref x y) person-name))

(module+ test
  (test-case
   "Put people in right places"
   (test-begin
    (define a-place
      (make-scenario 2 2
                     ("Ronie"  0 0)
                     ("Cris"   1 0)
                     ("Johnny" 0 1)
                     ("Pierri" 1 1)))
    
    (check equal? (name-at a-place 0 0) "Ronie")
    (check equal? (name-at a-place 1 0) "Cris")
    (check equal? (name-at a-place 0 1) "Johnny")
    (check equal? (name-at a-place 1 1) "Pierri"))))

; == Simulated Annealing protocol

; -- Energy simple model

(define (energy-at1 a-place x y)
  (define friends (person-friends (place-ref a-place x y)))
  (define neighbors (for/list ([p (place-around a-place x y)]) (person-name p)))
  (if (empty? friends) (length neighbors)
      (for/fold ([e (length neighbors)])
                ([f (in friends)])
        (- e (if (member f neighbors) 1 0)))))

(define (energy1 a-place)
  (define-with a-place
    (max-x place-x)
    (max-y place-y))
  (for*/sum ([x (in-range 0 max-x)]
             [y (in-range 0 max-y)])
    (energy-at1 a-place x y)))

(module+ test
  (test-case
   "Calculate energy simple model"
   (test-begin
    (define empty-place
      (make-scenario 3 3
                     ("Vazio" 0 0) ("Vazio" 1 0) ("Vazio" 2 0)
                     ("Vazio" 0 1) ("Vazio" 1 1) ("Vazio" 2 1)
                     ("Vazio" 0 2) ("Vazio" 1 2) ("Vazio" 2 2)))
    (check equal? (energy-at1 empty-place 1 1) 8)
    (check equal? (energy1 empty-place) 40))
   (test-begin
    (define max-energy
      (make-scenario 3 3
                     ("Ronie"  0 0) ("Cris"   1 0) ("Sato"   2 0)
                     ("Johnny" 0 1) ("Ceará"  1 1) ("Luis"   2 1)
                     ("Pierri" 0 2) ("Felipe" 1 2) ("Rubens" 2 2)))
    
    (check equal? (energy-at1 max-energy 1 1) 8)
    (check equal? (energy-at1 max-energy 0 0) 3)
    (check equal? (energy-at1 max-energy 1 0) 5)
    (check equal? (energy1 max-energy) 40))
   (test-begin
    (define no-energy
      (make-scenario
       3 3
       ("Ronie"  0 0 "Cris"  "Sato"  "Johnny" "Ceará" "Luis"  "Pierri" "Felipe" "Rubens")
       ("Cris"   1 0 "Ronie" "Sato"  "Johnny" "Ceará" "Luis"  "Pierri" "Felipe" "Rubens")
       ("Sato"   2 0 "Cris"  "Ronie" "Johnny" "Ceará" "Luis"  "Pierri" "Felipe" "Rubens")
       ("Johnny" 0 1 "Cris"  "Sato"  "Ronie"  "Ceará" "Luis"  "Pierri" "Felipe" "Rubens")
       ("Ceará"  1 1 "Cris"  "Sato"  "Johnny" "Ronie" "Luis"  "Pierri" "Felipe" "Rubens")
       ("Luis"   2 1 "Cris"  "Sato"  "Johnny" "Ceará" "Ronie" "Pierri" "Felipe" "Rubens")
       ("Pierri" 0 2 "Cris"  "Sato"  "Johnny" "Ceará" "Luis"  "Ronie"  "Felipe" "Rubens")
       ("Felipe" 1 2 "Cris"  "Sato"  "Johnny" "Ceará" "Luis"  "Pierri" "Ronie"  "Rubens")
       ("Rubens" 2 2 "Cris"  "Sato"  "Johnny" "Ceará" "Luis"  "Pierri" "Felipe" "Ronie")))
    (check equal? (energy-at1 no-energy  1 1) 0)
    (check equal? (energy1 no-energy) 0))))

(module+ test
  (test-case
   "Calculate energy on sample"
   (test-begin
    (define scenario-421 (read-scenario "data/experiment01.tsv"))
    (check equal? 421 (energy1 scenario-421)))))

; -- Energy Manhattan distance model

(define (energy-at2 a-place x y)
  (given [friends (person-friends (place-ref a-place x y))]
         [index (place-index a-place)])
  (for/sum ([f (in friends)])
    (match-define (cons x* y*) (dict-ref index f))
    (+ (abs (- x x*))
       (abs (- y y*)))))

(define (energy2 a-place)
  (define-with a-place
    (max-x place-x)
    (max-y place-y))
  (for*/sum ([x (in-range 0 max-x)]
             [y (in-range 0 max-y)])
    (energy-at2 a-place x y)))

(module+ test
  (test-case
   "Calculate energy Manhattan distance model"
   (test-begin
    (define empty-place
      (make-scenario 3 3
                     ("Vazio" 0 0) ("Vazio" 1 0) ("Vazio" 2 0)
                     ("Vazio" 0 1) ("Vazio" 1 1) ("Vazio" 2 1)
                     ("Vazio" 0 2) ("Vazio" 1 2) ("Vazio" 2 2)))
    (check equal? (energy-at2 empty-place 1 1) 0)
    (check equal? (energy2 empty-place) 0))
   (test-begin
    (define some-energy
      (make-scenario 3 3
                     ("Ronie"  0 0 "Rubens") ("Cris"   1 0 "Rubens" "Pierri") ("Sato"   2 0)
                     ("Johnny" 0 1)          ("Ceará"  1 1 "Cris" "Felipe")   ("Luis"   2 1)
                     ("Pierri" 0 2)          ("Felipe" 1 2)                   ("Rubens" 2 2)))
    
    (check equal? (energy-at2 some-energy 0 0) 4)
    (check equal? (energy-at2 some-energy 1 0) 6)
    (check equal? (energy-at2 some-energy 1 1) 2)
    (check equal? (energy2 some-energy) 12))))

; == Load Test Data Helpers

(define (read-scenario filename)
  (~>> (tabfile->list filename)
       (merge-people (people))
       (distribute-people (the-place))))

(define (tabfile->list filename)
  (call-with-input-file filename read-table-file))

(define (read-table-file file)
  (for/fold ([rslt (hash)])
            ([line (in-lines file)]
             [y (in-naturals)])
    (for/fold ([rslt rslt])
              ([col (in (reverse (string-split line "\t")))]
               [x (in-naturals)])
      (dict-set rslt col (list x y)))))

(define (merge-people people-list to-override)
  (for/list ([p people-list])
    (match-define (list x y) (dict-ref to-override (person-name p)))
    (struct-copy person p [x x] [y y])))

(define (people-by-energy a-place)
  (define people-energy
    (for*/list ([x (in-range (place-x a-place))]
                [y (in-range (place-y a-place))])
      (cons (name-at a-place x y) (energy-at1 a-place x y))))
  (sort people-energy < #:key cdr))

; == Visualization

(define cell-width (make-parameter 100))
(define cell-height (make-parameter 30))

(define color-step (/ 1/3 8))
(define brushes
  (reverse (for/list ([x (in-range 9)]
                      [color (in-range 0 1 color-step)])
             (define rgb (map (λ (c) (inexact->exact (ceiling (* c 255))))
                              (hsl->rgb (list color 1 0.5))))
             (new brush% [color (apply make-object color% rgb)]))))

(define (draw-heat-map-on-bitmap a-place)
  (given [width (cell-width)]
         [height (cell-height)]
         [target (make-bitmap (* 10 width) (* 10 height))]
         [dc (new bitmap-dc% [bitmap target])])
  (draw-heat-map-on-dc dc a-place)
  (make-object image-snip% target))

(define (draw-heat-map-on-dc dc a-place)
  (given [width (cell-width)]
         [height (cell-height)]
         [font-height 12]
         [send-text (λ (dc x y text)
                      (send dc set-clipping-rect x y (- width 3) (sub1 height))
                      (send dc draw-text text
                            (+ x 3)
                            (+ y (ceiling (/ font-height 2))))
                      (send dc set-clipping-region #f))])
  
  (send dc set-font (make-font #:size font-height))
  (for* ([y* (in-range (place-y a-place))]
         [x* (in-range (place-x a-place))])
    (given [x (- (* 9 width) (* width x*))] ; revert x
           [y (* height y*)]
           [e (energy-at1 a-place x* y*)]
           [name (name-at a-place x* y*)])
    (send dc set-brush (ref brushes e))
    (send dc draw-rectangle x y (sub1 width) (sub1 height))
    (send-text dc x y name)))


(define (do-it-bitmap)
  ;(define a-place (read-scenario "data/experiment01.tsv"))
  (draw-heat-map-on-bitmap (distribute-people (the-place) (people))))

(define (do-it-frame)
  (given [a-place (read-scenario "data/experiment01.tsv")]
         [canvas (create-canvas a-place)]
         [dc (send canvas get-dc)])
  (draw-heat-map-on-dc dc (distribute-people (the-place) (people))))

(define (create-canvas a-place)
  (define frame (new frame%
                     [label "Example"]
                     [width 1000]
                     [height 300]))
  (define canvas (new canvas% [parent frame]
                      [paint-callback
                       (lambda (canvas dc)
                         (draw-heat-map-on-dc dc a-place))]))
  (send frame show #t)
  canvas)

; == Initial parameters

(define-values (people the-place)
  (let* ([pp (file->list "data/chairs.csv")]
         [pc (make-place 10 10)]
         [pc* (distribute-people pc (take 10 pp))]
         [pp* (sequence->list (drop 10 pp))])
    (values (make-parameter pp*)
            (make-parameter pc*))))