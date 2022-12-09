(import (chicken process-context))
(import (chicken io))
;(import (chicken string))

; A = Rock, B = Paper, C = Scissors
; X = lose, Y = draw, Z = win

(define (needed goal other)
  ((case goal
     ((win) car)
     ((draw) cadr)
     ((loss) caddr))
   (case other
     ((rock)     '(paper     rock      scissors))
     ((paper)    '(scissors  paper     rock))
     ((scissors) '(rock      scissors  paper)))))

(define (score-round goal other)
  (+ (case (needed goal other)
       ((rock) 1)
       ((paper) 2)
       ((scissors) 3))
     (case goal
       ((win) 6)
       ((draw) 3)
       (else 0))))

(define (score rounds)
  (foldl + 0 (map (lambda (r) (apply score-round r))
                  rounds)))

(define (parse-char-other c)
  (case c
    ((#\A) 'rock)
    ((#\B) 'paper)
    ((#\C) 'scissors)))

(define (parse-char-goal c)
  (case c
    ((#\X) 'loss)
    ((#\Y) 'draw)
    ((#\Z) 'win)))

(define (parse-line line)
  (apply
    (lambda (other sp goal)
      (list (parse-char-goal goal)
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

