(import (chicken process-context))
(import (chicken io))
;(import (chicken string))

; A = Rock, B = Paper, C = Scissors
; X = Rock, Y = Paper, Z = Scissors

(define (won? me other)
  (case me
    ((rock) (eqv? 'scissors other))
    ((scissors) (eqv? 'paper other))
    ((paper) (eqv? 'rock other))))

(define (tied? me other)
  (eqv? me other))

(define (score-round me other)
  (+ (case me
       ((rock) 1)
       ((paper) 2)
       ((scissors) 3))
     (cond ((won? me other) 6)
           ((tied? me other) 3)
           (else 0))))

(define (score rounds)
  (foldl + 0 (map (lambda (r) (apply score-round r))
                  rounds)))

(define (parse-char-other c)
  (case c
    ((#\A) 'rock)
    ((#\B) 'paper)
    ((#\C) 'scissors)))

(define (parse-char-me c)
  (case c
    ((#\X) 'rock)
    ((#\Y) 'paper)
    ((#\Z) 'scissors)))

(define (parse-line line)
  (apply
    (lambda (other sp me)
      (list (parse-char-me me)
            (parse-char-other other)))
    (string->list line)))

(define (read-lines port)
  (let loop ((acc '())
             (line (read-line port)))
    (if (eq? #!eof line)
      (reverse acc)
      (loop (cons (parse-line line) acc)
            (read-line port)))))

(print (score
     (call-with-input-file (car (command-line-arguments)) read-lines)))
