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

(define (lt? a b)
  (< (compare a b) 0))

(define (line->list l)
  (with-input-from-string (string-translate l "[,]" "( )") read))

(define (pairs-from-file file)
  (let ((port (open-input-file file)))
    (let loop ((line (read-line port))
               (acc '()))
      (if (eq? #!eof line)
        (reverse acc)
        (loop (read-line port)
              (if (= 0 (string-length line))
                acc
                (cons (line->list line) acc)))))))


(define (main args)
  (let* ((packets (sort (cons '((2)) (cons '((6)) (pairs-from-file (car args)))) lt?))
         (l (length packets))
         (t1 (member '((2)) packets))
         (t2 (member '((6)) packets)))
    (print (* (- l (length t1) -1)
              (- l (length t2) -1)))))

(define test-input "input_13_test.txt")
(define test-pairs (pairs-from-file test-input))
