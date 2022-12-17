(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import (chicken format))
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


(define (intervals-at-y wanted-y sensors)
  (sort (filter-map (lambda (s) (interval-at-y wanted-y s)) sensors)
        interval<?))

(define (gaps-between-intervals intervals)
  (let loop ((px (cdar intervals))
             (intervals (cdr intervals))
             (gaps '()))
    ;(if (not (null? intervals)) (print "px=" px ", ival=" (car intervals)))
    (if (null? intervals)
      (if (null? gaps) #f gaps)
      (loop (max px (cdar intervals))
            (cdr intervals)
            (if (> (caar intervals) (+ px 1))
              (cons (cons (+ px 1) (- (caar intervals) 1))
                    gaps)
              gaps)))))

(define (print-progress y ym)
  (let* ((l (floor (* 80 (/ y ym))))
         (r (- 80 l)))
    (begin
      (display #\return)
      (display (make-string l #\.))
      (display (make-string r #\-))
      (printf "~!")
      )))

(define (main args)
  (let* ((data (sensors-from-file (car args)))
         (yx-max (string->number (cadr args)))
         (sensors (car data))
         (beacons (cdr data)))
    (let loop ((y 0)
               (gaps '()))
      (if (<= y yx-max)
        (begin
          (print-progress y yx-max)
          (loop (+ 1 y)
                (let ((gaps-at-y (gaps-between-intervals (intervals-at-y y sensors))))
                  (if gaps-at-y
                    (cons (cons y gaps-at-y) gaps)
                    gaps)))
        )
        (begin
          (print)
          (print "gaps: " gaps))))))


(define test-input "input_15_test.txt")
(define test-data (sensors-from-file test-input))
(define test-sensors (car test-data))
(define test-beacons (cdr test-data))
(define test-intervals-on-y (intervals-at-y 11 test-sensors))
;(define test-sorted-intervals (sort test-intervals-on-y interval<?))

