#lang racket/base

(require racket/function
         racket/match
         racket/vector
         racket/generic
         rackunit)

(module+ test
  (define simple (matrix #(a b c d e f) 2 3))
  (define partial (slice simple 1 2 2 3)))

(define (from-to start finish)
  (in-range start (add1 finish)))

; Vector index at position w h from matrix m
(define (index m w h)
  (let ([vec (matrix-data m)])
    (sub1 (+ (* (sub1 w) (matrix-h m)) h))))

(module+ test
  (check equal? (index simple 1 1) 0)
  (check equal? (index simple 2 3) 5)
  (check equal? (index simple 99 99) 392)) ; No boundary check

; Execute a proc with value at position w h
(define (at-index m w h proc)
  (let ([vec (matrix-data m)]
        [i (index m w h)])
    (proc vec i)))

(module+ test
  (check equal? (at-index simple 1 1 vector-ref) 'a)
  (check equal? (at-index simple 2 3 vector-ref) 'f)
  (check equal? (at-index simple 1 3 vector-ref) 'c)
  (check equal? (at-index simple 2 1 vector-ref) 'd))

(struct matrix (data w h) #:transparent)
(struct matrix-slice (data w1 h1 w2 h2) #:transparent)
(struct matrix-mutated (data mutations) #:transparent)

(define (width m)
  (match m
    [(matrix _ w _) w]
    [(matrix-slice _ w1 _ w2 _) (add1 (- w2 w1))]))

(module+ test
  (check equal? (width simple) 2)
  (check equal? (width partial) 2))

(define (height m)
  (match m
    [(matrix _ _ h) h]
    [(matrix-slice _ _ h1 _ h2) (add1 (- h2 h1))]))

(module+ test
  (check equal? (height simple) 3)
  (check equal? (height partial) 2))

(define (ref m w h)
  (match m
    [(matrix _ _ _) (at-index m w h vector-ref)]
    [(matrix-slice data w1 h1 _ _) (ref data (+ w1 w -1) (+ h1 h -1))]))

(module+ test
  (check equal? (ref simple 1 1) 'a)
  (check equal? (ref simple 2 3) 'f)
  (check equal? (ref partial 1 1) 'b)
  (check equal? (ref partial 2 2) 'f))

(define (set! m w h v)
  (match m
    [(matrix _ _ _) (at-index m w h (λ (vec i) (vector-set! vec i v)))]
    [(matrix-slice data w1 h1 _ _) (set! data (+ w1 w -1) (+ h1 h -1) v)]))

(module+ test
  (define m1 (matrix (vector 'a 'b 'c 'd) 2 2))
  (define p1 (matrix-slice m1 1 2 2 2))
  (set! m1 1 2 'z)
  (check equal? (ref m1 1 2) 'z)
  (check equal? (ref p1 1 1) 'z)
  (set! p1 2 1 'x)
  (check equal? (ref p1 2 1) 'x)
  (check equal? (ref m1 2 2) 'x))

(define (slice m w1 h1 w2 h2)
  (match m
    [(matrix _ _ _) (matrix-slice m w1 h1 w2 h2)]
    [(matrix-slice data w1* h1* w2* h2*)
     (slice data
            (+ w1* w1 -1)
            (+ h1* h1 -1)
            (+ w1* w2 -1)
            (+ h1* h2 -1))]))

(module+ test
  (define partial2 (slice partial 1 1 1 2))
  (check equal? (width partial2) 1)
  (check equal? (height partial2) 2)
  (check equal? (ref partial2 1 1) 'b)
  (check equal? (ref partial2 1 2) 'c))

(define (slice-set m w1 h1 other)
  (define vec (matrix-data m))
  (define mutations (vector-copy m))
  (define wid-length (matrix-w other))
  (define col-length (matrix-h other))
  (for ([w (in-naturals w1)]
        [wo (in-naturals 1)]
        [_i (in-range wid-length)])
    (vector-copy! mutations
                  (index m w h1)
                  other
                  (index other wo 1)
                  col-length))
  (matrix-mutated m mutations))

(define (i->wh i) #t)

(define (build-matrix w h proc)
  (define size (* w h))
  (for*/vector #:length size
    ([x (from-to 1 w)]
     [y (from-to 1 h)])
    (proc x y)))

(define (test-data w h)
  (format "~a,~a" w h))