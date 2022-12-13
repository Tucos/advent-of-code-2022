(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import srfi-1)
(import srfi-113) ; sets and bags
(import srfi-128) ; comparators

(define (compare-list a b)
  (let loop ((a a)
             (b b))
    (cond
      ((and (null? a) (null? b))
       0)
      ((null? a) -1)
      ((null? b) 1)
      (else (let ((c (compare (car a) (car b))))
              (if (= 0 c)
                (loop (cdr a) (cdr b))
                c
                ))))))


(define (compare a b)
  ;(print "(compare a='" a "', b='" b "')")
  (cond
    ((and (number? a)
          (number? b))
     (- a b))
    (else
     (compare-list
       (if (list? a) a (cons a '()))
       (if (list? b) b (cons b '()))))))


(define (line->list l)
  (with-input-from-string (string-translate l "[,]" "( )") read))

(define (pairs-from-file file)
  (let ((port (open-input-file file)))
    (let loop ((line (read-line port))
               (prev-line #f)
               (acc '()))
      (cond
        ((eq? #!eof line)
         (reverse acc))
        (prev-line
          (read-line port)
          (loop (read-line port)
                #f
                (cons (cons (line->list prev-line)
                            (line->list line))
                      acc)))
        (else (loop (read-line port)
                    line
                    acc))))))


(define (main args)
  (let loop ((i 1)
             (sum 0)
             (pairs (pairs-from-file (car args))))
    (if (null? pairs)
      (print "sum: " sum)
      (loop (+ i 1)
            (if (negative? (compare (caar pairs) (cdar pairs)))
              (begin (print "yay " i) (+ sum i))
              (begin (print "nay " i) sum))
            (cdr pairs)))
    ))

(define test-input "input_13_test.txt")
(define test-pairs (pairs-from-file test-input))
