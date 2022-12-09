(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1)
(import srfi-113) ; sets and bags
(import srfi-128) ; comparators

(define (coord x y tree)
  (cons (cons x y) tree))

(define (coord? c)
  (and (pair? c)
       (pair? (car c))
       (not (pair? (cdr c)))))

(define coord-x caar)
(define coord-y cdar)
(define coord-tree cdr)

(define (coord= a b)
  (and (= (coord-x a) (coord-x b))
       (= (coord-y a) (coord-y b))))

(define (coord< a b)
  (or (< (coord-x a) (coord-x b))
      (and (= (coord-x a) (coord-x b))
           (< (coord-y a) (coord-y b)))))

(define coord-comparator
  (make-comparator coord?
                   coord=
                   coord<
                   (lambda (c) (* (coord-x c) (coord-y c)))))


(define (blocked? trees tree x y)
  (let* ((h (coord-tree tree))
         (x (if x x (coord-x tree)))
         (y (if y y (coord-y tree)))
         (t (coord-tree (set-member trees (coord x y #f) #f))))
    (>= t h)))

(define (check-left trees tree)
  (let loop ((x (- (coord-x tree) 1))
             (count 0))
    (cond
      ((< x 0) count)
      ((blocked? trees tree x #f) (+ count 1))
      (else (loop (- x 1)
                  (+ count 1))))))

(define (check-right trees width tree)
  (let loop ((x (+ (coord-x tree) 1))
             (count 0))
    (cond
      ((>= x width) count)
      ((blocked? trees tree x #f) (+ count 1))
      (else (loop (+ x 1)
                  (+ count 1))))))

(define (check-up trees tree)
  (let loop ((y (- (coord-y tree) 1))
             (count 0))
    (cond
      ((< y 0) count)
      ((blocked? trees tree #f y) (+ count 1))
      (else (loop (- y 1)
                  (+ count 1))))))

(define (check-down trees height tree)
  (let loop ((y (+ (coord-y tree) 1))
             (count 0))
    (cond
      ((>= y height) count)
      ((blocked? trees tree #f y) (+ count 1))
      (else (loop (+ y 1)
                  (+ count 1))))))

(define (scenic-score trees width height x y)
  (let* ((tree (set-member trees (coord x y #f) #f))
         (l (check-left trees tree))
         (r (check-right trees width tree))
         (u (check-up trees tree))
         (d (check-down trees height tree)))
    (* l r u d)))


(define (char->digit c)
  (- (char->integer c) (char->integer #\0)))

(define (string->numberlist s)
  (map char->digit (string->list s)))

(define (grid-from-file filename)
  (map string->numberlist (read-lines (open-input-file filename))))


(define (set-from-grid grid)
  (let loop-y ((grid grid)
               (y 0)
               (s (set coord-comparator)))
    (if (null? grid)
      s
      (let loop-x ((line (car grid))
                   (x 0)
                   (s s))
        (if (null? line)
          (loop-y (cdr grid)
                  (+ y 1)
                  s)
          (loop-x (cdr line)
                  (+ x 1)
                  (set-adjoin s (coord x y (car line)))))))))


(define (main args)
  (let* ((lines (grid-from-file (car args)))
         (height (length lines))
         (width (length (car lines)))
         (trees (set-from-grid lines)))
    (let loop-y ((y (- height 1))
                 (acc-y '()))
      (if (< y 0)
        (print (apply max acc-y))
        (let loop-x ((x (- width 1))
                     (acc-x '()))
          (if (< x 0)
            (loop-y (- y 1)
                    (append acc-y acc-x))
            (loop-x (- x 1)
                    (cons (scenic-score trees width height x y) acc-x))
            ))))))

(define test-input "input_08_test.txt")
(define test-grid (grid-from-file test-input))
(define test-set (set-from-grid test-grid))
