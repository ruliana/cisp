#lang s-exp "rocket.rkt"
(require plot)
(provide main)

(define (call-with-input-output-file input-path output-path proc)
  (define (open-output input)
    (call-with-output-file* output-path
                            (λ (output) (proc input output))
                            #:mode 'text
                            #:exists 'truncate))
  (call-with-input-file* input-path
    open-output 
    #:mode 'text))

(define (average numbers)
  (define ns (map string->number (string-split numbers " ")))
  (exact->inexact (/ (apply + ns) (length ns))))

(define (process input)
  (for/list ([line (in-lines input)]
             #:when (regexp-match? #px"^\\d+\\s+\\d+\\s+\\(" line))
    (match-define (list _ best evals)
      (regexp-match #px"^\\d+\\s+(\\d+)\\s+\\((\\d+(?:\\s+\\d+)*)\\)" line))
    (list (string->number best) (average evals))))

(define (read-and-plot title filename)
  (define (indexed proc data)
    (for/list ([x (in-naturals)]
               [d data])
      (list x (proc d))))
  (given (data (call-with-input-file* filename process #:mode 'text))
         (best-line (indexed first data))
         (avg-line (indexed second data)))
  (plot-data title best-line avg-line))

(define (plot-data title best average)
  (plot (list (lines best #:color 'black #:label "Melhor" #:style 'short-dash)
              (lines average #:color 'black #:label "Média"))
        #:title title
        #:x-label "Iterações"
        #:y-label "Fitness"
        #:legend-anchor 'bottom-right))

(define (logfile? path)
  (regexp-match? #px"\\.log$" path))

(define (main)
  (for ([filename (in-directory ".")]
        #:when (logfile? filename))
    (read-and-plot filename)))

