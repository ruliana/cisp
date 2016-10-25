#lang s-exp "rocket.rkt"
(require gregor
         plot
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
             #:when (regexp-match? #px"^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\s+\\d+" line))
    (match-define (list _ time best evals)
      (regexp-match #px"^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\s+\\d+\\s+(\\d+(?:\\.\\d+)?)\\s+\\((\\d+(?:\\.\\d+)?(?:\\s+\\d+(?:\\.\\d+)?)*)\\)" line))
    (list (->posix (parse-datetime time "yyyy-MM-dd HH:mm:ss"))
          (string->number best)
          (average evals))))

(define (read-data input-filename)
  (given (raw-data (call-with-input-file* input-filename process #:mode 'text))
         (data (map (curry take 2) raw-data))
         (point-zero (first (first data)))
         (->minutes (λ (secs) (/ (- secs point-zero) 60)))
         (rslt (map (λ (lst) (list (->minutes (first lst)) (second lst))) data)))
  rslt)

(define (plot-data lsts [output-filename #f])
  (given (labels (append (list "Simulated Annealing"
                               "Clonal Selection"
                               #;"Ant Colony Optimization")
                         (repeat #f)))
         (data (for/list ([color (in-cycle '(red blue #;gray))]
                          [label (in labels)]
                          [d (in lsts)])
                 (lines d #:color color #:label label))))
  (if output-filename
      (plot-file data
                 output-filename
                 #:x-label "Minutos"
                 #:y-label "Custo"
                 #:legend-anchor 'top-right)
      (plot data
            #:x-label "Minutos"
            #:y-label "Custo"
            #:legend-anchor 'top-right)))

(define (logfile path)
  (define rslt (regexp-match #px"\\/(mut[^.\\/]+)\\.log$" path))
  (and rslt (ref rslt 1)))

(define (main)
  (given [out-file "/tmp/rslts-race.png"]
         [ds (list (read-data "rslts-race/simulated-annealing1.log")
                   (read-data "rslts-race/clonal-selection1.log")
                   #;(read-data "rslts-race/ant-colony-optimization1.log")
                   (read-data "rslts-race/simulated-annealing2.log")
                   (read-data "rslts-race/clonal-selection2.log")
                   #;(read-data "rslts-race/ant-colony-optimization2.log")
                   (read-data "rslts-race/simulated-annealing3.log")
                   (read-data "rslts-race/clonal-selection3.log")
                   #;(read-data "rslts-race/ant-colony-optimization3.log")
                   (read-data "rslts-race/simulated-annealing4.log")
                   (read-data "rslts-race/clonal-selection4.log")
                   #;(read-data "rslts-race/ant-colony-optimization4.log")
                   (read-data "rslts-race/simulated-annealing5.log")
                   (read-data "rslts-race/clonal-selection5.log")
                   #;(read-data "rslts-race/ant-colony-optimization5.log")
                   (read-data "rslts-race/simulated-annealing6.log")
                   (read-data "rslts-race/clonal-selection6.log")
                   #;(read-data "rslts-race/ant-colony-optimization6.log"))])
  (plot-data ds #f))

(main)
