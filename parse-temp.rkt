#lang racket

(require data/collection)

(define (parse-best line)
  (define match (regexp-match #px"(\\d+\\.\\d+)\\s\\([^)]+\\)$" line))
  (if match
      (string->number (second match))
      +inf.0))

(define-values (parsed _head _last)
  (for/fold ([rslt empty]
             [head ""]
             [last 0])
            ([line (in-lines)])
    (if (regexp-match? #px"^====>" line)
        (values (conj rslt (list last head)) line 0)
        (values rslt head (parse-best line)))))

(for ([line (take 10 (sort parsed < #:key first))])
  (apply printf "~a ~a\n" line))