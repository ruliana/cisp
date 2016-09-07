#lang s-exp "rocket.rkt"

(require "chairs.rkt")

(define (graph->dotfile lst)
  (define middle (for*/list ([v (in lst)]
                             [friend (person-friends v)])
                   (format "  \"~a\" -> \"~a\";"
                           (person-name v) friend)))
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

(graph->dotfile (file->list "data/chairs.csv"))