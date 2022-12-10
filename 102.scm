(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1)
(import srfi-113) ; sets and bags
(import srfi-128) ; comparators


(define (make-state cycle x crt)
  (cons (cons cycle x) crt))

(define (sprite-ref cycle x)
  (if (and (>= (modulo cycle 40) (- x 1))
           (<= (modulo cycle 40) (+ x 1)))
    #\#
    #\.))

(define (add-crt crt cc nc cx)
  ; start is incl, end is excl
  ; sprite is 3 wide, centered at cx
  (let loop ((c cc)
             (crt crt))
    (if (= c nc)
      crt
      (loop (+ 1 c)
            (cons (sprite-ref c cx)
                  crt)))))

(define (execute-instr state instr)
  (let ((current-cycle (caar state))
        (current-x (cdar state))
        (crt (cdr state)))
    (case (car instr)
      ((noop) ; 1 cycle, does nothing
       (let ((new-cycle (+ 1 current-cycle))
             (new-x current-x))
         (make-state new-cycle
                     new-x
                     (add-crt crt
                              current-cycle
                              new-cycle
                              current-x))))
      ((addx) ; 2 cycles, x += v
       (let ((new-cycle (+ 2 current-cycle))
             (new-x (+ current-x (cdr instr))))
         (make-state new-cycle
                     new-x
                     (add-crt crt
                              current-cycle
                              new-cycle
                              current-x)))))))

(define (execute-instrs instrs)
  (let loop ((state (make-state 0 1 '()))
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
         (state (execute-instrs instrs)))
    (for-each print (string-chop (list->string (reverse (cdr state))) 40))))

(define test-input "input_10_test.txt")
(define test-instrs (instrs-from-file test-input))
