#lang racket/base
; Those are libraries, functions and macros I use on my own projects
(require racket/function
         racket/match
         racket/string
         racket/dict
         racket/format
         racket/generator
         racket/set
         threading
         data/collection
         match-plus
         (only-in racket/list
                  remove-duplicates
                  empty))

(provide (all-from-out racket/base
                       racket/function
                       racket/match
                       racket/string
                       racket/format
                       racket/generator
                       racket/set
                       threading
                       data/collection)
         empty
         remove-duplicates
         (all-defined-out))

; == Language extensions

; "define" replacement
(define-syntax-rule (given (bind expr) ...)
  (begin (define bind expr) ...))

; procfy
; Make values acts as procs
(define/match ((λ= value) other)
  [((? hash?) _) (hash-ref value other other)]
  [((? sequence?) _) (index-of value other)]
  [(_ _) (equal? value other)])

(define (filter-map proc seq)
  (~>> seq (map proc) (filter identity)))

(define-syntax-rule (define~> (name args ...) body ...)
  (define (name args ... last-arg) (~> last-arg body ...)))

(define-syntax-rule (define~>> (name args ...) body ...)
  (define (name args ... last-arg) (~>> last-arg body ...)))

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

; Collections

(define (in-head-x-tail seq)
  (in-generator
   #:arity 3
   (let loop ([head #()]
              [x (first seq)]
              [tail (rest seq)])
     (yield head x tail)
     (unless (empty? tail)
       (loop (conj head x) (first tail) (rest tail))))))


(define (in-dicts dicts [on-failure #f])
  (define keys (list->set (apply append (map dict-keys dicts))))
  (in-generator #:arity 2
                (for ([k keys])
                  (let ([vs (for/list ([d dicts]) (dict-ref d k on-failure))])
                    (yield k vs)))))

(define ((dict-merger updater [on-failure #f]) . dicts)
  (for/hash ([(k vs) (in-dicts dicts on-failure)])
    (values k (apply updater vs))))

; Matrix

(struct matrix (row col data) #:transparent)

(define (make-matrix row col)
  (matrix row col (make-vector (* col row) 0.0)))

(define (matrix-ref a-matrix row col)
  (define-values (data index)
    (matrix-data/index a-matrix row col))
  (vector-ref data index))

(define (matrix-set! a-matrix row col value)
  (define-values (data index)
    (matrix-data/index a-matrix row col))
  (vector-set! data index value))

(define/match* (matrix-data/index (matrix _ col-size data) row col)
  (values data (+ row (* col col-size))))

(define/match (in-matrix . args)
  [((list (and a-matrix (matrix row col data)))) (in-matrix a-matrix 0 0)]
  [((list (matrix row col data) r-origin c-origin))
   (in-generator
    #:arity 3
    (for ([v (in-vector data)]
          [i (in-naturals)])
      (define-values (c r) (quotient/remainder i col))
      (yield (- r r-origin) (- c c-origin) v)))])

(define/match* (matrix-convolution matrix-a matrix-b)
  (given (row-a (matrix-row matrix-a))
         (col-a (matrix-col matrix-a))
         (rslt (make-matrix row-a col-a)))
  (for* ([ra (in-range row-a)]
         [ca (in-range col-a)])
    (define new-value
      (for/sum ([(rb cb vb) (in-matrix matrix-b 1 1)]
                #:when (and (< -1 (+ ra rb) row-a)
                            (< -1 (+ ca cb) col-a)))
        (* vb (matrix-ref matrix-a (+ ra rb) (+ ca cb)))))
    (matrix-set! rslt ra ca new-value))
  rslt)

(define/match* (matrix-display (matrix row col data) format-proc)
  (for ([r (in-range row)])
    (define line
      (for/list ([c (in-range col)])
        (format-proc (vector-ref data (+ r (* c col))))))
    (displayln (join " | " line))))
  