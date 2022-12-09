(import srfi-14) ; char sets
(import (chicken io))
(import (chicken process-context))
(import (chicken string))

(define (char-set-head cset)
  (char-set-ref cset (char-set-cursor cset)))

(define (string->compartment s)
  (->char-set s))

(define (string->rucksack s)
  (map string->compartment
       (string-chop s
                    (/ (string-length s) 2))))

(define (char->priority c)
  (if (char-lower-case? c)
    (+ 1
       (- (char->integer c)
          (char->integer #\a)))
    (+ 27
       (- (char->integer c)
          (char->integer #\A)))))

(define (score rucksacks)
  (apply +
         (map char->priority
              (map char-set-head
                   (map (lambda (rucksack) (apply char-set-intersection rucksack))
                        rucksacks)))))


(define (read-rucksacks port)
  (let loop ((acc '())
             (line (read-line port)))
    (if (eq? #!eof line)
      (reverse acc)
      (loop (cons (string->rucksack line) acc)
            (read-line port)))))

(print (score
         (call-with-input-file (car (command-line-arguments))
                               read-rucksacks)))
