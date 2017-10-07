#lang racket

(require data/collection)

(provide join)

(define (join seq [separator ""])
  (define (join* seq rslt)
    (if (empty? seq) rslt
        (join* (rest seq) (string-append rslt separator (first seq)))))
  (if (empty? seq) ""
      (join* (rest seq) (first seq))))