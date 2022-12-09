(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1)

(define (marker? a b c d)
  (not (or (eq? a b)
          (eq? a c)
          (eq? a d)
          (eq? b c)
          (eq? b d)
          (eq? c d))))

(define (find-marker s)
  (let loop ((s (cdddr s))
             (i 3)
             (p1 (car s))
             (p2 (cadr s))
             (p3 (caddr s)))
    (if (marker? p1 p2 p3 (car s))
      (+ i 1)
      (loop (cdr s)
            (+ 1 i)
            p2
            p3
            (car s)))))

(define (main args)
  (print (find-marker (string->list (car args)))))
