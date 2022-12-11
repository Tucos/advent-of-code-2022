(import (chicken io))
(import (chicken process-context))
(import (chicken sort))
(import (chicken string))
(import srfi-1)
(import srfi-113) ; sets and bags
(import srfi-128) ; comparators

; monkeys have 2 lists, items they already have and items they received

(define (make-monkey items received operation test counter)
  (define (self msg . args)
    (case msg
      ((items) items)
      ((received) items)
      ((operation) operation)
      ((test) test)
      ((counter) counter)
      ((->string) (conc "(make-monkey items=" items " received=" received " counter=" counter ")"))
      ((start-round)
       ; append reversed received onto items
       (make-monkey (append items (reverse received))
                    '()
                    operation
                    test
                    counter))
      ((inspect)
       ; take first item
       ; execute operation
       ; execute test
       ; send to target
       (let loop ((items items)
                  (counter counter)
                  (acc '()))
         (if (null? items)
           (cons (reverse acc)
                 (make-monkey '()
                              received
                              operation
                              test
                              counter))
           (let ((new-item (floor (/ (operation (car items)) 3))))
             (loop (cdr items)
                   (+ counter 1)
                   (cons (cons new-item
                               (test new-item))
                         acc))))))
      ((receive)
       ; cons item onto received
       (make-monkey items
                    (cons (car args)
                          received)
                    operation
                    test
                    counter))
      ))
  self)

(define (list-set lst i v)
  (call-with-values (lambda () (split-at lst i))
                    (lambda (l r) (append l (cons v (cdr r))))))

(define (list-update lst i fn)
  (call-with-values (lambda () (split-at lst i))
                    (lambda (l r) (append l (cons (fn (car r)) (cdr r))))))

(define (play-turn monkeys i)
  (let* ((monkey (list-ref monkeys i))
         (r ((monkey 'start-round) 'inspect))
         (new-monkey (cdr r))
         (passes (car r)))
    (let loop ((new-monkeys (list-set monkeys i new-monkey))
              (passes passes))
      (if (null? passes)
        new-monkeys
        (loop (list-update new-monkeys
                           (cdar passes)
                           (lambda (m) (m 'receive (caar passes))))
              (cdr passes))))))

(define (play-round monkeys)
  ; all monkeys have a go
  (let ((count (length monkeys)))
    (let loop ((monkeys monkeys)
               (i 0))
      (if (= i count)
        monkeys
        (loop (play-turn monkeys i)
              (+ i 1))))))


(define (monkey-from-strings l1 l2 l3 l4 l5 l6)
  (define (items s)
    (map string->number (string-split (cadr (string-split s ":")) ", ")))
  (define (operation s)
    (let* ((parts (string-split (substring s 23)))
           (fn (if (equal? "*" (car parts)) * +))
           (n (string->number (cadr parts))))
      (if n
        (lambda (x) (fn n x))
        (lambda (x) (fn x x)))))
  (define (divisor s)
    (string->number (substring s 21)))
  (define (target s)
    (string->number (substring (cadr (string-split s "y")) 1)))
  (define (test divisor tt tf)
    (lambda (x) (if (= 0 (modulo x divisor)) tt tf)))
  (make-monkey (items l2)
               '()
               (operation l3)
               (test (divisor l4)
                     (target l5)
                     (target l6))
               0))

(define (monkeys-from-file filename)
  (let ((port (open-input-file filename)))
    (let loop ((line (read-line port))
               (line-acc '())
               (monkey-acc '()))
      (define (add-monkey)
        (cons (apply monkey-from-strings (reverse line-acc))
              monkey-acc))
      (cond
        ((eq? line #!eof)
         (reverse (add-monkey)))
        ((= 0 (string-length line))
         (loop (read-line port)
               '()
               (add-monkey)))
        (else
          (loop (read-line port)
                (cons line line-acc)
                monkey-acc))))))

; play 20 rounds, 2 highest counters, multiply
(define (monkey-activity monkeys)
  (let loop ((i 20)
             (monkeys monkeys))
    (if (= i 0)
      (map (lambda (m) (m 'counter)) monkeys)
      (loop (- i 1)
            (play-round monkeys)))))

(define (main args)
  (let* ((monkeys (monkeys-from-file (car args)))
         (activity (monkey-activity monkeys))
         (sa (sort activity >)))
    (print (* (car sa) (cadr sa)))))

(define test-input "input_11_test.txt")
(define test-monkeys (monkeys-from-file test-input))
(define (monkey->string m) (m '->string))
