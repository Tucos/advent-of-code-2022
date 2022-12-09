(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1) ; list


(define (parse-stack cback line stacks actions)
  (let loop ((parts (string-chop line 4))
             (new-stacks '())
             (stacks stacks))
    (if (null? parts)
      (cback (reverse new-stacks) actions)
      (let ((c (string-ref (car parts) 1)))
        (if (char-whitespace? c)
          (loop (cdr parts)
                (cons (car stacks) new-stacks)
                (cdr stacks))
          (loop (cdr parts)
                (cons (cons c (car stacks))
                      new-stacks)
                (cdr stacks)))))))

(define (parse-action cback line stacks actions)
  (cback stacks
         (cons
           ; move x from a to b
           (let ((parts (string-split line " ")))
             (list (string->number (second parts))
                   (string->number (fourth parts))
                   (string->number (sixth parts))))
           actions)))

(define (parse-file port)
  (let* ((line (read-line port))
         (stacks (make-list (/ (+ 1 (string-length line)) 4) '())))
    (let loop ((line line)
               (parser parse-stack)
               (stacks stacks)
               (actions '()))
      (cond ((eq? line #!eof)
             (cons (map reverse stacks) (reverse actions)))
            ((= (string-length line) 0)
             (loop (read-line port)
                   parse-action
                   stacks
                   actions))
            ((char-numeric? (string-ref line 1))
             (loop (read-line port)
                   parser
                   stacks
                   actions))
            (else (parser
                    (lambda (new-stacks new-actions)
                      (loop (read-line port)
                            parser
                            new-stacks
                            new-actions))
                    line
                    stacks
                    actions))))))

(define (execute-move stacks count from to)
  (let ((old-from (list-ref stacks (- from 1))))
    (let loop ((acc '())
               (stacks stacks)
               (i 1))
      (cond ((null? stacks)
             (reverse acc))
            ((= i from)
             (loop (cons (drop (car stacks) count)
                         acc)
                   (cdr stacks)
                   (+ i 1)))
            ((= i to)
             (loop (cons (append (take old-from count)
                               (car stacks))
                         acc)
                   (cdr stacks)
                   (+ i 1)))
            (else
              (loop (cons (car stacks)
                          acc)
                    (cdr stacks)
                    (+ i 1)))))))

(define (execute-action stacks action)
  (let ((count (car action))
        (from (cadr action))
        (to (caddr action))
        (stacks stacks))
    (execute-move stacks count from to)))

(define (execute-actions stacks actions)
  (foldl execute-action
         stacks
         actions))

(define (main . args)
  (print (list->string (map car (let ((r (parse-file (open-input-file (caar args)))))
                                  (execute-actions (car r) (cdr r)))))))


