#lang racket/base
(require racket/function)
(require racket/match)
(require racket/string)
(require threading)
(require data/collection)
(module+ test (require rackunit))

; == Language extensions

(define-syntax-rule (define~> (name args ...) body ...)
  (define (name args ... last-arg) (~> last-arg body ...)))

(define-syntax-rule (define~>> (name args ...) body ...)
  (define (name args ... last-arg) (~>> last-arg body ...)))

(define (vector-allocate size elem)
  (vector->immutable-vector (make-vector size elem)))

(define (nth* seq . indexes)
  (map (curry nth seq) indexes))

; Inclusive range =/
(define (from-to n1 n2)
  (range n1 (add1 n2)))

; == Struct protocol

(struct person (name group x y friends) #:prefab)

(define/match (make-person done name group x y . friends)
  [(_ "Vazio" _ _ _ _) 'empty]
  [(_ "Parede" _ _ _ _) 'wall]
  [(_ _ _ _ _ _)  (person name
                          group
                          (string->number x)
                          (string->number y)
                          (make-placeholder (filter non-empty-string? friends)))])

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
    (values (second line) (apply make-person line))))

(define (hash->graph field-get hsh)
  (define (get key) (ref hsh key #f)) 
  (for ([(k v) (in-hash hsh)])
    (let* ([place (field-get v)]
           [keys (placeholder-get place)]
           [replacements (~>> keys (map get) (filter identity) sequence->list)])
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
  (place x y (vector-allocate (* x y) 'empty)))

(define (xy->position a-place x y)
  (~> (place-x a-place) (* y) (+ x)))

(define (place-ref a-place x y)
  (define pos (xy->position a-place x y))
  (~> a-place place-positions (ref pos)))

(define (place-set a-place x y value)
  (define pos (xy->position a-place x y))
  (define new-positions (~> a-place place-positions (set-nth pos value)))
  (struct-copy place a-place [positions new-positions]))

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
