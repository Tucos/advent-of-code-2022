(import (chicken io))
(import (chicken process-context))
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


(define (make-state snake coords)
  (cons snake coords))

(define (in-range? t h)
  (let ((dx (abs (- (coord-x t)
                    (coord-x h))))
        (dy (abs (- (coord-y t)
                    (coord-y h)))))
    (and (<= dx 1)
         (<= dy 1))))

(define (move-tail t h)
  (if (in-range? t h)
    t
    (let ((ver (if (< (coord-y t) (coord-y h)) down up))
          (hor (if (< (coord-x t) (coord-x h)) right left)))
      (cond
        ((= (coord-y t) (coord-y h))
         (hor t))
        ((= (coord-x t) (coord-x h))
         (ver t))
        (else
          (ver (hor t)))))))


; head moves, if tail is out of range, it also moves
#;(define (move-head move state)
  (let* ((ch (caar state))
         (ct (cdar state))
         (nh (move ch))
         (coords (cdr state)))
    (if (in-range? ct nh)
      (make-state nh ct coords)
      (let ((nt (move-tail ct nh)))
        (make-state nh
                    nt
                    (set-adjoin coords nt))))))

(define (move-snake move state)
  (let* ((ch (caar state))
         (nh (move ch)))
  (let loop ((snake (cdar state))
             (ph nh)
             (acc (cons nh '())))
    (if (null? snake)
      (make-state (reverse acc)
                  (set-adjoin (cdr state) (car acc)))
      (let ((nt (move-tail (car snake) ph)))
        (loop (cdr snake)
              nt
              (cons nt acc)))))))

(define (execute-move move state)
  (let loop ((repeats (cdr move))
             (state state))
    (if (= 0 repeats)
      state
      (loop (- repeats 1)
            (move-snake (car move)
                        state)))))

(define (execute-moves moves)
  (fold execute-move
        (make-state (make-list 10 (coord 0 0))
                    (set-adjoin (set coord-comparator)
                                (coord 0 0)))
        moves))


(define (string->move s)
  (let ((parts (string-split s " ")))
    (cons
      (case (string-ref (car parts) 0)
        ((#\L) left)
        ((#\R) right)
        ((#\U) up)
        ((#\D) down))
      (string->number (cadr parts)))))


(define (moves-from-file filename)
  (let ((port (open-input-file filename)))
    (let loop ((line (read-line port))
               (acc '()))
      (if (eq? line #!eof)
        (reverse acc)
        (loop (read-line port)
              (cons (string->move line) acc))))))

(define (main args)
  (let ((moves (moves-from-file (car args))))
    (print (set-size (cdr (execute-moves moves))))))

(define test-input "input_09_test.txt")
(define test-moves (moves-from-file test-input))


