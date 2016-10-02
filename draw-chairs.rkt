#lang s-exp "rocket.rkt"

(require "chairs.rkt"
         "colors.rkt"
         racket/class
         racket/draw
         (only-in racket/gui image-snip% frame% canvas%))

(provide create-canvas
         draw-heat-map-on-dc
         draw-heat-map-on-bitmap)

; == Visualization

(define cell-width (make-parameter 100))
(define cell-height (make-parameter 30))

(define (make-brush current)
  (let* ([color (* (- 1 current) 1/3)]
         [rgb (map (λ (x) (inexact->exact (ceiling (* x 255)))) (hsl->rgb (list color 1 0.5)))])
    (new brush% [color (apply make-object color% rgb)])))

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
         [max-x (place-x a-place)]
         [max-y (place-y a-place)]
         [energies (build-vector max-x (λ (i) (make-vector max-y 0)))]
         [send-text (λ (dc x y text)
                      (send dc set-clipping-rect x y (- width 3) (sub1 height))
                      (send dc draw-text text
                            (+ x 3)
                            (+ y (ceiling (/ font-height 2))))
                      (send dc set-clipping-region #f))])
  
  (send dc set-font (make-font #:size font-height))
  (for* ([y* (in-range max-y)]
         [x* (in-range max-x)])
    (define e (energy-at a-place x* y*))
    (~> energies (vector-ref x*) (vector-set! y* e)))
  (for* ([y* (in-range max-y)]
         [x* (in-range max-x)])
    (given [x (- (* 9 width) (* width x*))] ; revert x
           [y (* height y*)]
           [e (~> energies (vector-ref x*) (vector-ref y*))]
           [name (name-at a-place x* y*)])
    (send dc set-brush (make-brush e))
    (send dc draw-rectangle x y (sub1 width) (sub1 height))
    (send-text dc x y name)))

(define (do-it-bitmap)
  ;(define a-place (read-scenario "data/experiment01.tsv"))
  (draw-heat-map-on-bitmap (distribute-people (the-place) (people))))

(define (do-it-frame)
  (given [a-place (read-scenario "data/experiment01.tsv")]
         [canvas (create-canvas a-place)]
         [dc (send canvas get-dc)])
  (draw-heat-map-on-dc dc a-place))

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