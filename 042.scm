(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1) ; list

; 12345678
; |--|
;    |--|

;    |--|
; |--|

; |--|
;     |--|

(define (range-contains? a b)
  (if (<= (car a) (car b))
    (and (<= (car b)
             (cdr a)))
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
           (lambda (l) (apply range-contains? l))
           (map
             parse-line
             (read-lines (open-input-file (caar args))))))))

