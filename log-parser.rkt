#lang s-exp "rocket.rkt"
(require plot
         plot/no-gui)

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
             #:when (regexp-match? #px"^\\s*\\d+\\s+\\d+(?:\\.\\d+)?\\s+\\(" line))
    (match-define (list _ best evals)
      (regexp-match #px"^\\s*\\d+\\s+(\\d+(?:\\.\\d+)?)\\s+\\((\\d+(?:\\.\\d+)?(?:\\s+\\d+(?:\\.\\d+)?)*)\\)" line))
    (list (string->number best) (average evals))))

(define (read-data input-filename)
  (define (indexed proc data)
    (for/list ([x (in-naturals)]
               [d data])
      (list x (proc d))))
  (given (data (call-with-input-file* input-filename process #:mode 'text))
         (best-line (indexed first data))
         (avg-line (indexed second data)))
  best-line)

(define (plot-data lsts [output-filename #f])
  (define data (for/list ([color (in '(red blue red black black black black black black black))]
                          [label (in (list "Manhattan" "Vizinhança"))]
                          #;[color (in (repeat 'black))]
                          [d (in lsts)])
                 (lines d #:color color #:label label)))
  (if output-filename
      (plot-file data
                 output-filename
                 #:x-label "Iterações"
                 #:y-label "Custo"
                 #:legend-ancho 'top-right)
      (plot data
            #:x-label "Iterações"
            #:y-label "Custo")))

(define (logfile path)
  (define rslt (regexp-match #px"\\/(mut[^.\\/]+)\\.log$" path))
  (and rslt (ref rslt 1)))

(define (main)
  (given [out-file "/tmp/vizinho-vs-manhattan.png"]
         [ds (list (read-data "rslts/mut12345-manhattan.log")
                   (read-data "rslts/mut12345-block.log"))])
  (plot-data ds #f))

(main)
