(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1)
(import srfi-113) ; sets and bags
(import srfi-128) ; comparators


; start-cycle is exlusive, end-cycle inclusive
(define (interesting-cycle start-cycle end-cycle)
  (if (and (< (modulo start-cycle 40) 20)
           (>= (modulo end-cycle 40) 20))
    (- end-cycle (modulo (modulo end-cycle 40) 20))
    #f))


(define (make-state cycle x interesting-xs)
  (cons (cons cycle x) interesting-xs))

(define (add-if-interesting interesting-xs start-cycle end-cycle start-x end-x)
  (let ((ic (interesting-cycle start-cycle end-cycle)))
    (if ic
      (cons (cons ic (if (= ic end-cycle) end-x start-x)) interesting-xs)
      interesting-xs)))

(define (execute-instr state instr)
  (let ((current-cycle (caar state))
        (current-x (cdar state))
        (interesting-xs (cdr state)))
    (case (car instr)
      ((noop) ; 1 cycle, does nothing
       (let ((new-cycle (+ 1 current-cycle))
             (new-x current-x))
         (make-state new-cycle
                     new-x
                     (add-if-interesting interesting-xs
                                         current-cycle
                                         new-cycle
                                         current-x
                                         new-x))))
      ((addx) ; 2 cycles, x += v
       (let ((new-cycle (+ 2 current-cycle))
             (new-x (+ current-x (cdr instr))))
         (make-state new-cycle
                     new-x
                     (add-if-interesting interesting-xs
                                         current-cycle
                                         new-cycle
                                         current-x
                                         new-x)))))))

(define (execute-instrs instrs)
  (let loop ((state (make-state 1 1 '()))
             (instrs instrs))
    (if (null? instrs)
      state
      (loop (execute-instr state (car instrs))
            (cdr instrs)))))

(define (string->instr s)
  (let* ((parts (string-split s " "))
         (instr (string->symbol (car parts))))
    (case instr
      ((noop) (cons instr '()))
      ((addx) (cons instr (string->number (cadr parts)))))))


(define (instrs-from-file filename)
  (let ((port (open-input-file filename)))
    (map string->instr (read-lines port))))

(define (main args)
  (let* ((instrs (instrs-from-file (car args)))
         (state (execute-instrs instrs))
         (interesting-xs (cdr state))
         (strengths (map (lambda (p) (* (car p) (cdr p))) interesting-xs))
         (total (apply + strengths)))
    (print total)))

(define test-input "input_10_test.txt")
(define test-instrs (instrs-from-file test-input))
