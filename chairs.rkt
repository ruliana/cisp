#lang racket/base
(require racket/function
         racket/match
         racket/string
         racket/generator
         threading
         data/collection
         racket/set
         (only-in racket/list
                  empty))

(module+ test (require rackunit))

; == Language extensions

; procfy
; Make values acts as procs
(define/match ((λ= value) other)
  [((? hash?) _) (hash-ref value other other)]
  [((? sequence?) _) (index-of value other)]
  [(_ _) (equal? value other)])

(define (filter-map proc seq)
  (~>> seq (map proc) (filter identity)))

(define-syntax-rule (make-scenario max-x max-y (name x y friends ...) ...)
  (distribute-people (make-place max-x max-y)
                     (hash->graph person-friends
                                  (make-immutable-hash (list (cons name (make-person name "Grupo Fixo" x y friends ...)) ...)))))

(define-syntax-rule (define~> (name args ...) body ...)
  (define (name args ... last-arg) (~> last-arg body ...)))

(define-syntax-rule (define~>> (name args ...) body ...)
  (define (name args ... last-arg) (~>> last-arg body ...)))

(define-syntax-rule (define/maker (name args ...) body ...)
  (define name (generator (args ...) body ...)))

(define-syntax-rule (define-with data (lets procs) ...)
  (define-values (lets ...) (values (procs data) ...)))

(define (vector-allocate size elem)
  (vector->immutable-vector (make-vector size elem)))

(define (nth* seq . indexes)
  (map (curry nth seq) indexes))

(define (join separator seq)
  (define (join* seq rslt)
    (if (empty? seq) rslt
        (join* (rest seq) (string-append rslt separator (first seq)))))
  (if (empty? seq) ""
      (join* (rest seq) (first seq))))

; Inclusive range =)
(define (from-to n1 n2)
  (range n1 (add1 n2)))

; == Struct protocol

(struct person (name group x y friends) #:prefab)

(define/maker (wall x y)
  (let loop ([col x] [row y] [i 0])
    (define-values (c r) (yield (new-wall col row i)))
    (loop c r (add1 i))))

(define (new-wall x y i)
  (person (format "wall ~a" i) 'wall x y (make-placeholder empty)))

(define/maker (empty-space x y)
  (let loop ([col x] [row y] [i 0])
    (define-values (c r) (yield (new-empty-space col row i)))
    (loop c r (add1 i))))

(define (new-empty-space x y i)
  (person (format "empty ~a" i) 'empty-space x y (make-placeholder empty)))

(define/match (make-person name group x y . friends) 
  [("" _ _ _ _)       (empty-space x y)]
  [("Vazio" _ _ _ _)  (empty-space x y)]
  [("Parede" _ _ _ _) (wall x y)]
  [(_ _ _ _ _)        (person name
                              group
                              x y
                              (make-placeholder (filter non-empty-string? friends)))])

(define (raw->person done name group x y . friends)
  (apply make-person name group (string->number x) (string->number y) friends))

; == Load data protocol

(define (file->list file-name)
  (rest (call-with-input-file file-name read-lines)))

(define (read-lines file)
  (for/list ([line (in-lines file)])
    (map string-trim (string-split line "," #:trim? #f))))

(module+ test
  (define (position line) (~> line (subsequence 3 5) sequence->list))
  
  (test-case
   "Load file"
   (test-begin
    (define data (file->list "chairs.csv"))
    (check equal? (position (first data)) '("0" "0"))
    (check equal? (position (last data)) '("9" "9")))))

; == Data -> Struct protocol

(define~>> (make-graph)
  list->hash
  (hash->graph person-friends))

(define (list->hash lst)
  (for/hash ([line (in lst)])
    (let ([a-person (apply raw->person line)])
      (values (person-name a-person) a-person))))

(define (hash->graph field-get hsh)
  (define (get key) (ref hsh key #f)) 
  (for ([(k v) (in-hash hsh)])
    (let* ([place (field-get v)]
           [keys (placeholder-get place)]
           [replacements (~>> keys (filter-map get) sequence->list)])
      (placeholder-set! place replacements)))
  (make-reader-graph hsh))

(module+ test
  (define~>> (friends-of name)
    (ref _ name)
    person-friends
    (map person-name)
    sequence->list)
  
  (define (first-friend a-person)
    (~> a-person person-friends first))
  
  (define data '(("" "Ronie"  "Empresas" "0" "0" "Johnny" "Pierri" )
                 ("" "Johnny" "Empresas" "1" "0" "Pierri")
                 ("" "Pierri" "Empresas" "1" "1")))
  
  (define graph (make-graph data))
  
  (check = (length graph) 3)
  (check equal? (person-x (ref graph "Ronie")) 0)
  (check equal? (person-y (ref graph "Pierri")) 1)
  
  (test-case
   "Basic relations on graph"
   (check equal? (friends-of "Ronie" graph) '("Johnny" "Pierri"))
   (check equal? (friends-of "Johnny" graph) '("Pierri"))
   (check equal? (friends-of "Pierri" graph) '()))
  
  (test-case
   "Friend of friend"
   (check equal? (~>> (ref graph "Ronie") first-friend first-friend person-name) "Pierri")))

; == Place protocol
; Who goes where.

(struct place (x y positions) #:transparent)

(define (make-place x y)
  (place x y (vector-allocate (* x y) (empty-space x y))))

(define (xy->position a-place x y)
  (~> (place-x a-place) (* y) (+ x)))

(define (place-ref a-place x y)
  (define pos (xy->position a-place x y))
  (~> a-place place-positions (ref pos)))

(define (place-set a-place x y value)
  (define pos (xy->position a-place x y))
  (define new-positions (~> a-place place-positions (set-nth pos value)))
  (struct-copy place a-place [positions new-positions]))

(define (place-random-change a-place)
  (define pos (place-positions a-place))
  (define rnd (~> pos length randoms))
  (let* ([p1 (first rnd)]
         [p2 (first (filter (negate (λ= p1)) rnd))]
         [v1 (ref pos p1)]
         [v2 (ref pos p2)]
         [new-positions (~> pos (set-nth p1 v2) (set-nth p2 v1))])
    (struct-copy place a-place [positions new-positions])))

(define (positions-around a-place x y)
  (define row-below (max (sub1 y) 0))
  (define row-above (min (add1 y) (sub1 (place-y a-place))))
  (define col-left  (max (sub1 x) 0))
  (define col-right (min (add1 x) (sub1 (place-x a-place))))
  (for*/list ([row (from-to row-below row-above)]
              [col (from-to col-left col-right)]
              #:when (not (and (= x col) (= y row))))
    (xy->position a-place col row)))

(define (place-around a-place x y)
  (apply nth*
         (place-positions a-place)
         (positions-around a-place x y)))

(module+ test
  (define old-place (make-place 3 4))
  (test-case
   "Set and recover data from place"
   (test-begin
    (define new-place (place-set old-place 1 2 'testing))
    (check equal? (place-ref old-place 1 2) 'empty)
    (check equal? (place-ref new-place 1 2) 'testing)))
  (test-case
   "Get positions around somewhere"
   (check equal? (positions-around old-place 1 1) '(0 1 2 3 5 6 7 8))
   (check equal? (positions-around old-place 0 0) '(1 3 4))
   (check equal? (positions-around old-place 2 3) '(7 8 10))
   (check equal? (positions-around old-place 2 1) '(1 2 4 7 8))))

; == Application

(define (distribute-people a-place people)
  (for/fold ([rslt a-place])
            ([(name a-person) (in-hash people)])
    (define-with a-person
      (x person-x)
      (y person-y))
    (place-set rslt x y a-person)))

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

(define (energy-at a-place x y)
  (define friends (~>> (place-ref a-place x y) person-friends sequence->list))
  (define neighbors (~>> (place-around a-place x y) sequence->list))
  (define matching (set-intersect friends neighbors))
  (- (length neighbors) (length matching)))

(define (energy a-place)
  (define-with a-place
    (max-x place-x)
    (max-y place-y))
  (for*/sum ([x (range 0 max-x)]
             [y (range 0 max-y)])
    (energy-at a-place x y)))

(define (status-at a-place x y)
  (define (display-names msg people)
    (displayln (format "~a: ~a" msg (~>> people (map person-name) (join ", ")))))
  (define friends (~>> (place-ref a-place x y) person-friends sequence->list))
  (define neighbors (~>> (place-around a-place x y) sequence->list))
  (define matching (set-intersect friends neighbors))
  (displayln (format "name: ~a" (~> a-place (place-ref x y) person-name)))
  (display-names "friends" friends)
  (display-names "neighbors" neighbors)
  (display-names "matching" matching)
  (display-names "missing" (set-subtract friends neighbors)))

(module+ test
  (test-case
   "Calculate energy"
   (test-begin
    (define max-energy
      (make-scenario 3 3
                     ("Ronie"  0 0) ("Cris"   1 0) ("Sato"   2 0)
                     ("Johnny" 0 1) ("Ceará"  1 1) ("Luis"   2 1)
                     ("Pierri" 0 2) ("Felipe" 1 2) ("Rubens" 2 2)))
    
    (check equal? (energy-at max-energy 1 1) 8)
    (check equal? (energy-at max-energy 0 0) 3)
    (check equal? (energy-at max-energy 1 0) 5)
    (check equal? (energy max-energy) 40))
   (test-begin
    (define no-energy
      (make-scenario 3 3
                     ("Ronie"  0 0 "Cris"  "Sato"  "Johnny" "Ceará" "Luis"  "Pierri" "Felipe" "Rubens")
                     ("Cris"   1 0 "Ronie" "Sato"  "Johnny" "Ceará" "Luis"  "Pierri" "Felipe" "Rubens")
                     ("Sato"   2 0 "Cris"  "Ronie" "Johnny" "Ceará" "Luis"  "Pierri" "Felipe" "Rubens")
                     ("Johnny" 0 1 "Cris"  "Sato"  "Ronie"  "Ceará" "Luis"  "Pierri" "Felipe" "Rubens")
                     ("Ceará"  1 1 "Cris"  "Sato"  "Johnny" "Ronie" "Luis"  "Pierri" "Felipe" "Rubens")
                     ("Luis"   2 1 "Cris"  "Sato"  "Johnny" "Ceará" "Ronie" "Pierri" "Felipe" "Rubens")
                     ("Pierri" 0 2 "Cris"  "Sato"  "Johnny" "Ceará" "Luis"  "Ronie"  "Felipe" "Rubens")
                     ("Felipe" 1 2 "Cris"  "Sato"  "Johnny" "Ceará" "Luis"  "Pierri" "Ronie"  "Rubens")
                     ("Rubens" 2 2 "Cris"  "Sato"  "Johnny" "Ceará" "Luis"  "Pierri" "Felipe" "Ronie")))
    (check equal? (energy-at no-energy  1 1) 0)
    (check equal? (energy no-energy) 0))))

; == Simulated Annealing

(define (simulated-annealing current [best current] [temperature 100] [retries 40])
  (define new (place-random-change current))
  (define best* (if (< (energy new) (energy best)) new best))
  (cond
    [(>= 0 (energy current)) current]
    [(>= 0 retries)          best*]
    [(>= 0 temperature)      (simulated-annealing best* best* 100 (sub1 retries))]
    [(should-change temperature (energy current) (energy new))
     (simulated-annealing new best* (sub1 temperature) retries)]
    [else
     (simulated-annealing current best* (sub1 temperature) retries)]))

(define (should-change temperature current-energy new-energy)
  (define prob (exp (/ (- current-energy new-energy) temperature)))
  (< (random) prob))

; == Graphviz

(define (graph->dotfile a-graph)
  (define middle (for*/list ([(k v) (in-hash a-graph)]
                             [friend (person-friends v)])
                   (format "  \"~a\" -> \"~a\";"
                           k (person-name friend))))
  (header->footer middle))

(define (header->footer middle)
  (~>> (append '("digraph G {"
                 "  overlap=false;"
                 "  splines=true;"
                 "  concentrate=true;"
                 "  edge[dir=none];")
               middle
               '("}"))
       (join "\n")))

; == Using (remove it later)

(define a-graph (~> (file->list "chairs.csv") (make-graph)))
(define a-place (distribute-people (make-place 10 10) a-graph))
(define rslt (simulated-annealing a-place))