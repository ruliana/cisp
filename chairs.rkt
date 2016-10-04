#lang s-exp "rocket.rkt"

(require math/distributions
         (only-in racket/list shuffle)
         racket/random
         racket/dict)

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
         place-around
         distribute-people
         display-place
         name-at
         friends-at
         ; Cost function / Fitness function
         (rename-out [energy-at2 energy-at]
                     [energy2 energy]
                     [place-random-change2 place-random-change])
         energy-at1
         energy1
         energy-at2
         energy2
         place-random-change1
         place-random-change2
         place-random-change3
         people
         the-place
         read-scenario)

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

(define (place-xy-for a-place name)
  (~> a-place place-index (dict-ref name #f)))

(define (place-replace a-place name-a name-b)
  (match-let* ([(cons x1 y1) (place-xy-for a-place name-a)]
               [(cons x2 y2) (place-xy-for a-place name-b)]
               [a (place-ref a-place x1 y1)]
               [b (place-ref a-place x2 y2)])
    (~> a-place (place-set x1 y1 b) (place-set x2 y2 a))))

(define (place-move a-place x1 y1 x2 y2)
  (given [a (place-ref a-place x1 y1)]
         [b (place-ref a-place x2 y2)])
  (~> a-place (place-set x1 y1 b) (place-set x2 y2 a)))


; Change 2 random people
(define (place-random-change1 a-place)
  (given [max-x (place-x a-place)]
         [max-y (place-y a-place)]
         [x1 (random-ref (range 0 max-x))]
         [y1 (random-ref (range 1 max-y))]
         [x2 (random-ref (range 0 max-x))]
         [y2 (random-ref (range 1 max-y))])
  (place-move a-place x1 y1 x2 y2))

; Move a random person horizontally or vertically
(define (place-random-change2 a-place)
  (given [max-x (sub1 (place-x a-place))]
         [max-y (sub1 (place-y a-place))]
         [x1 (random-ref (range 0 (add1 max-x)))]
         [y1 (random-ref (range 1 (add1 max-y)))]
         [moves (list (λ (x y) (values (if (= x 0)       max-x (sub1 x)) y))  ; left
                      (λ (x y) (values (if (= x max-x)   0     (add1 x)) y))  ; right
                      (λ (x y) (values x (if (= y 1)     max-y (sub1 y))))    ; up
                      (λ (x y) (values x (if (= y max-y) 1     (add1 y)))))]  ; down
         [move (random-ref moves)]
         [(x2 y2) (move x1 y1)])
  (place-move a-place x1 y1 x2 y2))

; Move a random block horizontally or vertically
(define (place-random-change3 a-place)
  (given [max-x (sub1 (place-x a-place))]
         [max-y (sub1 (place-y a-place))]
         [x1 (random-ref (range 0 max-x))]
         [y1 (random-ref (range 1 max-y))]
         [x2 (random-ref (range (add1 x1) (add1 max-x)))]
         [y2 (random-ref (range (add1 y1) (add1 max-y)))]
         [moves (list (list (list x1 x2) (list y1 y2)
                            (λ (x y) (values (if (= x 0) max-x (sub1 x)) y)))  ; left
                      (list (list x2 x1) (list y1 y2)
                            (λ (x y) (values (if (= x max-x) 0 (add1 x)) y)))  ; right
                      (list (list x1 x2) (list y1 y2)
                            (λ (x y) (values x (if (= y 1) max-y (sub1 y)))))    ; up
                      (list (list x1 x2) (list y2 y1)
                            (λ (x y) (values x (if (= y max-y) 1 (add1 y))))))]  ; down
         [(list x-range y-range move) (random-ref moves)])
  ;(printf "block: ~a ~a ~a ~a\n" x1 x2 y1 y2)
  (for*/fold ([rslt a-place])
             ([x (apply from-to x-range)]
              [y (apply from-to y-range)])
    (given [(x* y*) (move x y)])
    ;(printf "~a ~a -> ~a ~a : ~a\n" x y x* y* (name-at a-place x y))
    (place-move rslt x y x* y*)))

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

(define (name-at a-place x y)
  (~> a-place (place-ref x y) person-name))

(define (friends-at a-place x y)
  (given [friends (~> a-place (place-ref x y) person-friends)]
         [valid (~> a-place place-index dict-keys)])
  (sequence->list (filter (λ (f) (member f valid)) friends)))

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

; -- Energy simple model

(define (energy-at1 a-place x y)
  (given [friends (friends-at a-place x y)])
  (for/sum ([f (in friends)])
    (define pos (dict-ref (place-index a-place) f #f))
    (match pos
      [(cons x* y*)
       (if (and (>= 1 (abs (- x x*)))
                (>= 1 (abs (- y y*))))
           0 1)]
      [_ 0])))

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
    (check equal? (energy-at1 empty-place 1 1) 0)
    (check equal? (energy1 empty-place) 0))
   (test-begin
    (define max-energy
      (make-scenario 3 3
                     ("Ronie"  0 0) ("Cris"   1 0) ("Sato"   2 0)
                     ("Johnny" 0 1) ("Ceará"  1 1) ("Luis"   2 1)
                     ("Pierri" 0 2) ("Felipe" 1 2) ("Rubens" 2 2)))
    
    (check equal? (energy-at1 max-energy 1 1) 0)
    (check equal? (energy-at1 max-energy 0 0) 0)
    (check equal? (energy-at1 max-energy 1 0) 0)
    (check equal? (energy1 max-energy) 0))
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
    (check equal? (energy1 no-energy) 4))))

(module+ test
  (test-case
   "Calculate energy on sample"
   (test-begin
    (define scenario-422 (read-scenario "data/experiment01.tsv"))
    (check equal? 19693867/556920 (energy1 scenario-422)))))

; -- Energy Manhattan distance model

(define (manhattan a-place x y friend)
  (define pos (dict-ref (place-index a-place) friend #f))
  (match pos
    [(cons x* y*)
     (+ (abs (- x x*))
        (abs (- y y*)))]
    [_ 0]))

(define (energy-at2 a-place x y)
  (define (energy-for friends)
    (for/sum ([f (in friends)])
      (manhattan a-place x y f)))
  (given [friends (friends-at a-place x y)]
         [e (energy-for friends)])
  (if (zero? e) 0
      (- 1 (/ (length friends) e))))

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
                     ("Pierri" 0 2)          ("Felipe" 1 2 "Pierri" "Rubens") ("Rubens" 2 2)))
    
    (check equal? (energy-at2 some-energy 0 0) 3/4)
    (check equal? (energy-at2 some-energy 1 0) 2/3)
    (check equal? (energy-at2 some-energy 1 1) 0)
    (check equal? (energy-at2 some-energy 1 2) 0)
    (check equal? (energy2 some-energy) 17/12))))


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

; == Initial parameters

(define-values (people the-place)
  (let* ([pp (file->list "data/chairs.csv")]
         [pc (make-place 10 10)]
         [pc* (distribute-people pc (take 10 pp))]
         [pp* (sequence->list (drop 10 pp))])
    (values (make-parameter pp*)
            (make-parameter pc*))))