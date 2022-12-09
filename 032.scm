(import srfi-1)  ; lists
(import srfi-14) ; char sets
(import (chicken io))
(import (chicken process-context))
(import (chicken string))

(define (char-set-head cset)
  (char-set-ref cset (char-set-cursor cset)))

(define (string->rucksack s)
  (->char-set s))

(define (char->priority c)
  (if (char-lower-case? c)
    (+ 1
       (- (char->integer c)
          (char->integer #\a)))
    (+ 27
       (- (char->integer c)
          (char->integer #\A)))))

(define (badge rucksacks)
  (char-set-head (apply char-set-intersection rucksacks)))

(define (score groups)
  (apply +
         (map char->priority
              (map badge groups))))

(define (read-groups port)
  (let loop ((groups '()))
    (let ((group (map (lambda (_) (read-line port))
                      (make-list 3))))
      (if (eq? #!eof (car group))
        (reverse groups)
        (loop (cons group groups))))))

(print (apply +
              (map char->priority
                   (map badge
                        (map (lambda (g) (map string->rucksack g))
                             (read-groups (open-input-file (car (command-line-arguments)))))))))


