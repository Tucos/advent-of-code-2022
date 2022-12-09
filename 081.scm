(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1)
(import srfi-113) ; sets and bags
(import srfi-128) ; comparators


(define (coord? c)
  (and (pair? c)
       (not (pair? (cdr c)))))

(define coord-x car)
(define coord-y cdr)

(define (coord= a b)
  (and (= (car a) (car b))
       (= (cdr a) (cdr b))))

(define (coord< a b)
  (or (< (car a) (car b))
      (and (= (car a) (car b))
           (< (cdr a) (cdr b)))))

(define coord-comparator
  (make-comparator coord?
                   coord=
                   coord<
                   (lambda (c) (* (car c) (cdr c)))))

(define (add-if-visible visibles h vh x y coord-mapper)
  (if (> h vh)
    (let ((coord (coord-mapper x y)))
      (set-adjoin visibles
                  coord))
    visibles))


(define (scroll lines coord-mapper width)
  (let loop-lines ((visible-heights (make-list width -1))
                   (y 0)
                   (visibles (set coord-comparator))
                   (lines lines)
                   (acc-lines '()))
    (if (null? lines)
      (cons acc-lines visibles)
      (let loop-line ((line (car lines))
                      (acc-line '())
                      (visible-heights visible-heights)
                      (acc-visible-heights '())
                      (visibles visibles)
                      (x 0))
      (if (null? line)
        (loop-lines (reverse acc-visible-heights)
                    (+ y 1)
                    visibles
                    (cdr lines)
                    (cons (reverse acc-line) acc-lines))
        (let ((tree (car line))
              (vh (car visible-heights)))
          (loop-line (cdr line)
                     (cons tree acc-line)
                     (cdr visible-heights)
                     (cons (max vh tree)
                           acc-visible-heights)
                     (add-if-visible visibles
                                     tree
                                     vh
                                     x
                                     y
                                     coord-mapper)
                     (+ x 1))))))))


(define (scroll-twice grid height width coord-mapper)
  (let* ((r1 (scroll grid
                     coord-mapper
                     width))
         (r2 (scroll (car r1)
                     (lambda (x y) (coord-mapper x
                                                 (- height 1 y)))
                     width)))
    (set-union (cdr r1) (cdr r2))))


(define (do-main grid width height)
  (let ((r1 (scroll-twice grid
                          height
                          width
                          cons))
        (r2 (scroll-twice (rotate grid)
                          width
                          height
                          (lambda (x y) (cons y
                                              (- height 1 x))))))
    (set-union r1 r2)))


(define (rotate grid)
  ; 1 2 3   7 4 1
  ; 4 5 6   8 5 2
  ; 7 8 9   9 6 3
  (let loop-rows ((grid grid)
                  (acc '()))
    (if (null? (car grid))
      (reverse acc)
      (loop-rows (map cdr grid)
                 (cons (fold (lambda (r acc) (cons (car r) acc))
                             '()
                             grid)
                       acc)))))


(define (char->digit c)
  (- (char->integer c) (char->integer #\0)))

(define (string->numberlist s)
  (map char->digit (string->list s)))

(define (main args)
  (let* ((lines (map string->numberlist (read-lines (open-input-file (car args)))))
         (height (length lines))
         (width (length (car lines))))
    (print (set-size (do-main lines width height)))))


(define test-input (list "input_08_test.txt"))
