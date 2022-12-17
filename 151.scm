(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import srfi-1)
(import srfi-113) ; sets and bags
(import srfi-128) ; comparators


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

(define (distance a b)
  (+ (abs (- (coord-x a) (coord-x b)))
     (abs (- (coord-y a) (coord-y b)))))



(define (sensor-from-line line)
  ; Sensor at x=.., y=..: closest beacon is at x=.., y=..
  (let* ((parts (string-split line "=,:"))
         (sensor-x (string->number (second parts)))
         (sensor-y (string->number (fourth parts)))
         (beacon-x (string->number (sixth parts)))
         (beacon-y (string->number (eighth parts)))
         (sensor-coord (coord sensor-x sensor-y))
         (beacon-coord (coord beacon-x beacon-y)))
    (cons (cons sensor-coord
                (distance sensor-coord beacon-coord))
          beacon-coord)
          ))

(define (sensors-from-file filename)
  (let ((port (open-input-file filename)))
    (let loop ((line (read-line port))
               (sensor-acc '())
               (beacon-acc '()))
      (if (eq? #!eof line)
        (cons sensor-acc (list->set coord-comparator beacon-acc))
        (let* ((data (sensor-from-line line))
               (sensor (car data))
               (beacon (cdr data)))
          (loop (read-line port)
                (cons sensor sensor-acc)
                (cons beacon beacon-acc))
        )))))


(define (interval-at-y y sensor)
  ; for each y, add 1 on left and right
  (let* ((dy (abs (- y (coord-y (car sensor)))))
         (dr (- (cdr sensor) dy)))
    (if (< dr 0)
      #f
      (cons (- (coord-x (car sensor)) dr)
            (+ (coord-x (car sensor)) dr)))))

(define (interval<? a b)
  (or (< (car a) (car b))
      (and (= (car a) (car b))
           (< (cdr a) (cdr b)))))

(define (interval-length v)
  (- (cdr v) (car v) -1))


(define (sum-intervals lst)
  (car (fold (lambda (v acc)
               (let* ((l-real (interval-length v))
                      (overlap (max 0 (- (cdr acc) (car v) -1)))
                      (l-over (- l-real overlap)))
                 (cons (+ (car acc)
                          (max 0
                               l-over))
                       (max (cdr acc) (cdr v)))))
             (cons (interval-length (car lst))
                   (cdar lst))
             (cdr lst))))



(define (main args)
  (let* ((data (sensors-from-file (car args)))
         (sensors (car data))
         (beacons (cdr data))
         (wanted-y (string->number (cadr args)))
         (intervals (filter-map (lambda (s) (interval-at-y wanted-y s)) sensors))
         (sorted (sort intervals interval<?)))
    (print "sum: " (- (sum-intervals sorted)
                      (set-size (set-filter (lambda (b) (= wanted-y (coord-y b))) beacons))))))


(define test-input "input_15_real.txt")
;(define test-sensors (sensors-from-file test-input))
;(define test-intervals-on-y (filter-map (lambda (s) (interval-at-y 2000000 s)) test-sensors))
;(define test-sorted-intervals (sort test-intervals-on-y interval<?))

