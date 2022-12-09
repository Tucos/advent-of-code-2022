(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1)

(define (marker? alist)
  (every (lambda (v) (= (cdr v) 1))
         alist))

(define (inc alist c)
  (let loop ((alist alist)
             (acc '()))
    (cond
      ((null? alist)
       (cons (cons c 1)
             acc))
      ((eq? c (caar alist))
       (append (cdr alist)
               (cons (cons c
                           (+ 1 (cdar alist)))
                     acc)))
      (else (loop (cdr alist)
                  (cons (car alist)
                        acc))))))

(define (dec alist c)
  (let loop ((alist alist)
             (acc '()))
    (cond
      ((null? alist)
       acc)
      ((eq? c (caar alist))
       (if (= 1 (cdar alist))
         (append (cdr alist)
                 acc)
         (append (cdr alist)
                 (cons (cons c
                             (- (cdar alist) 1))
                       acc))))
      (else (loop (cdr alist)
                  (cons (car alist)
                        acc))))))

(define (make-alist lst)
  (fold (lambda (c alist) (inc alist c))
        '()
        lst))

(define (find-marker head tail)
  (let loop ((tail tail)
             (window head)
             (alist (make-alist head))
             (i 14))
    ;(print alist)
    (if (marker? alist)
      i
      (let ((dropped (car window))
            (added (car tail)))
        (loop (cdr tail)
              (append (cdr window) (list added))
              (inc (dec alist dropped) added)
              (+ i 1))))))

(define (dostuff s)
  (call-with-values (lambda () (split-at (string->list s) 14)) find-marker))

(define (main args)
  (print (dostuff (car args))))

