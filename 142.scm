(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import srfi-1)
(import srfi-113) ; sets and bags
(import srfi-128) ; comparators

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define coord cons)
(define coord-x car)
(define coord-y cdr)

(define (coord? c)
  (and (pair? c)
       (not (pair? (cdr c)))))

(define ((coord-map-x fn) c)
  (coord (fn (coord-x c))
         (coord-y c)))

(define ((coord-map-y fn) c)
  (coord (coord-x c)
         (fn (coord-y c))))

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

(define left (coord-map-x dec))
(define right (coord-map-x inc))
(define up (coord-map-y dec))
(define down (coord-map-y inc))

(define (string->coord s)
  (let* ((parts (string-split s ","))
         (x (string->number (car parts)))
         (y (string->number (cadr parts))))
    (coord x y)))

(define (line-direction start end)
  (if (= (coord-x start) (coord-x end))
    (if (> (coord-y start) (coord-y end))
      up
      down)
    (if (> (coord-x start) (coord-x end))
      left
      right)))

(define (coords-from-line line)
  (let* ((parts (string-split line)))
    (let loop ((parts (cddr parts))
               (start (string->coord (car parts)))
               (acc '()))
      (let* ((end (string->coord (car parts)))
             (dir (line-direction start end))
             (end-inclusive (dir end)))
        (let loop-segment ((current-coord start)
                           (acc-segment '()))
          (if (coord= current-coord end-inclusive)
            (if (null? (cdr parts))
              (append acc acc-segment)
              (loop (cddr parts)
                    end
                    (append acc acc-segment)))
            (loop-segment (dir current-coord)
                          (cons current-coord acc-segment))))))))

(define (coords-from-file filename)
  (let ((port (open-input-file filename)))
    (let loop ((line (read-line port))
               (acc (set coord-comparator)))
      (if (eq? line #!eof)
        acc
        (loop (read-line port)
              (apply set-adjoin (cons acc (coords-from-line line))))))))


(define (blocked? blocked-coords bmc sand-coord)
  (or (= (coord-y sand-coord) (+ 2 bmc))
      (set-contains? blocked-coords sand-coord)))

; down, down and left, down and right
(define (move-sand sc blocked-coords bmc)
  (cond
    ((not (blocked? blocked-coords bmc (down sc)))
     (down sc))
    ((not (blocked? blocked-coords bmc (down (left sc))))
     (down (left sc)))
    ((not (blocked? blocked-coords bmc (down (right sc))))
     (down (right sc)))
    (else #f)))

; sand starts at 500,0
(define (add-sand bmc blocked-coords)
  (let loop ((sand-coord (coord 500 0)))
    (let ((next-coord (move-sand sand-coord blocked-coords bmc)))
      (cond
        (next-coord (loop next-coord))
        ((coord= sand-coord (coord 500 0)) #f)
        (else sand-coord)))))


(define (find-bmc coords)
  (set-fold (lambda (c acc) (max acc (coord-y c))) 0 coords))

(define (main args)
  (let* ((blocked-coords (coords-from-file (car args)))
         (bottom-most-coord (find-bmc blocked-coords)))
    (let loop ((blocked-coords blocked-coords)
               (counter 0))
      (let ((new-blocked (add-sand bottom-most-coord blocked-coords)))
        (if new-blocked
          (loop (set-adjoin blocked-coords new-blocked)
                (+ counter 1))
          (print (+ 1 counter)))))))


(define test-input "input_14_test.txt")
;(define test-pairs (pairs-from-file test-input))
