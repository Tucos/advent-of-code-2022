(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1) ; list

(define (range-contains? a b)
  (and (<= (car a)
           (car b))
       (>= (cdr a)
           (cdr b))))

(define (fully-contained? a b)
  (or (range-contains? a b)
      (range-contains? b a)))

; a-b,c-d
(define (parse-line l)
  (map
    (lambda (r)
      (apply cons
             (map string->number
                  (string-split r "-"))))
    (string-split l ",")))

(define (main . args)
  (print (foldl (lambda (acc l) (+ 1 acc))
         0
         (filter
           (lambda (l) (apply fully-contained? l))
           (map
             parse-line
             (read-lines (open-input-file (caar args))))))))

