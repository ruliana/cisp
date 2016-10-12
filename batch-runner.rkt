#lang s-exp "rocket.rkt"

(require "chairs.rkt"
         "clonal-selection.rkt"
         (only-in racket/list in-combinations)
         (only-in racket/place
                  [place-channel-get receive-from]
                  [place-channel-put send-to]
                  [place create-process]
                  [place-wait wait-for]))
(provide main)

(struct task (mutator-indexes energy-index filename) #:prefab)

(define (make-tracker initial)
  (define current initial)
  (λ (it) (if (< it current)
              (begin (set! current it) #t)
              #f)))

(define (write-state output best-state population iteration tracker)
  (define best (state-place best-state))
  (when (tracker (state-display-energy best-state))
    (displayln "=" output)
    (display-place best output)
    (displayln "=" output))
  (fprintf output
           "~a ~a ~a\n"
           (~r iteration #:min-width 5)
           (~> population first state-display-energy exact->inexact (~r #:precision '(= 4) #:min-width 7))
           (~>> population
                (map (λ~> state-display-energy exact->inexact (~r #:precision '(= 4) #:min-width 7)))
                sequence->list)))

(define (run-task a-task)
  (define mutators (hash 1 place-random-change1
                         2  place-random-change2
                         3  place-random-change3
                         4  place-random-change4
                         5  place-random-change5))
  (define energies (hash 'block energy1
                         'manhattan energy2))
  (parameterize ([mutation-operators (sequence->list (map (λ (x) (ref mutators x)) (task-mutator-indexes a-task)))]
                 [fitness-energy (ref energies (task-energy-index a-task))]
                 [max-iterations 2500])
    (define tracker (make-tracker 99999))
    (define (updater best-state population iteration)
      (call-with-output-file (task-filename a-task)
        (λ (output) (write-state output best-state population iteration tracker))
        #:exists 'append
        #:mode 'text))
    (state-place (clonal-selection updater))))


(define best-tasks (list (task '(4) 'manhattan "mut4c-manhattan.log")
                         (task '(4 5) 'manhattan "mut45c-manhattan.log")
                         (task '(1 4 5) 'manhattan "mut145c-manhattan.log")
                         (task '(1 3) 'manhattan "mut13c-manhattan.log")
                         (task '(1 3 4) 'manhattan "mut134c-manhattan.log")
                         (task '(1 3 4 5) 'manhattan "mut1345c-manhattan.log")
                         (task '(1 2 3) 'manhattan "mut123c-manhattan.log")
                         (task '(1 2 4) 'manhattan "mut124c-manhattan.log")
                         (task '(1 2 3 5) 'manhattan "mut1235c-manhattan.log")
                         (task '(1 2 3 4) 'manhattan "mut1234c-manhattan.log")
                         (task '(1 2 3 4 5) 'manhattan "mut12345c-manhattan.log")))

(define missing-tasks (list (task '(1 3) 'manhattan "mut13d-manhattan.log")
                            (task '(1 3) 'manhattan "mut13e-manhattan.log")
                            (task '(1 3) 'manhattan "mut13f-manhattan.log")
                            (task '(1 3) 'manhattan "mut13g-manhattan.log")
                            (task '(1 3) 'manhattan "mut13h-manhattan.log")
                            (task '(1 3) 'manhattan "mut13i-manhattan.log")))
                            
                         

(define (main)
  (given #;[tasks (for*/list ([muts (in-combinations '(1 2 3 4 5))]
                            [type (in '(manhattan block))]
                            #:unless (empty? muts))
                  (task muts type (format "mut~a-~a.log" (join "" (map number->string muts)) type)))]
         [tasks missing-tasks]
         [processes (for/list ([_n (in-range 3)])
                      (create-process channel
                                      (let loop ()
                                        (let ([a-task (receive-from channel)])
                                          (when (task? a-task)
                                            (run-task a-task)
                                            (loop))))))])
  (for ([a-task (in tasks)]
        [p (in (cycle processes))]
        [i (in-naturals 1)])
    (printf "~a task #~a\n" i a-task)
    (send-to p a-task))
  (for ([p (in processes)]) (send-to p 'done))
  (for ([p (in processes)]) (wait-for p))
  (custodian-shutdown-all (current-custodian)))