(import (chicken io))
(import (chicken process-context))
(import (chicken string))
(import srfi-1)

; type Tree a
;     = Node a (List (Tree a))
;
; type Cxt a
;     = Top
;     | Child { left : List (Tree a), right : List (Tree a) } a (Cxt a)
;
; type alias Loc a =
;     ( Tree a, Cxt a )

; Node
(define (make-node label children)
  ;(print "make-node label: " label "; children " children)
  (lambda (make-node-msg . args)
    (case make-node-msg
      ((label) label)
      ((children) children)
      ((add-child)
       (make-node label
                  (cons (make-node (car args) '())
                        children)))
      ((split-at)
       (let loop ((pred (car args))
                  (head '())
                  (tail children))
         (if (pred ((car tail) 'label))
           (values head
                   (car tail)
                   (cdr tail))
           (loop pred
                 (cons (car tail)
                       head)
                 (cdr tail)))))
      ((->string)
       (string-append "(node"
                      " label="
                      ((car args) label)
                      " children="
                      (->string (map (lambda (c) (c '->string (car args)))
                           children))
                      ")"))
      )))


; Context
(define (make-context-top)
  (lambda (make-context-top-msg . args)
    (case make-context-top-msg
      ((top?) #t)
      ((->string) "(context-top)")
  )))

(define (make-context-child siblings-left siblings-right parent-label parent-context)
  (lambda (make-context-child-msg . args)
    (case make-context-child-msg
      ((top?) #f)
      ((siblings-left) siblings-left)
      ((siblings-right) siblings-right)
      ((parent-label) parent-label)
      ((parent-context) parent-context)
      ((->string)
       (string-append "(context"
                      " siblings-left="
                      (->string (map (lambda (n) (n '->string (car args))) siblings-left))
                      " siblings-right="
                      (->string (map (lambda (n) (n '->string (car args))) siblings-right))
                      " parent-label="
                      ((car args) parent-label)
                      " parent-context="
                      (parent-context '->string (car args))
                      ")"))

      )))

; Location ( node . context )

(define (make-loc node context)
  (define (self make-loc-msg . args)
    (case make-loc-msg
      ((node) node)
      ((context) context)
      ((up)
       (if (context 'top?)
         self
         (make-loc (make-node (context 'parent-label)
                              (append (context 'siblings-left)
                                      (list node)
                                      (context 'siblings-right)))
                   (context 'parent-context))))
      ((top)
       (if (context 'top?)
         self
         ((self 'up) 'top)))
      ((add-child)
       (make-loc (node 'add-child (car args))
                 context))
      ((step-down)
       (call-with-values (lambda () (node 'split-at (car args)))
                         (lambda (child-siblings-left child-node child-siblings-right)
                           (make-loc child-node
                                     (make-context-child child-siblings-left
                                                         child-siblings-right
                                                         (node 'label)
                                                         context)))))
      ((->string)
       (string-append "(loc"
                      " node="
                      (node '->string (car args))
                      " context="
                      (context '->string (car args))
                      ")"))
      ))
  self)



; Labels

(define (make-label-dir name)
  (lambda (make-label-dir-msg . args)
    (case make-label-dir-msg
      ((dir?) #t)
      ((file?) #f)
      ((name) name)
      ((->string) (string-append "(dir"
                                 " name='"
                                 name
                                 "'"
                                 ")"))
      )))

(define (make-label-file name size)
  (lambda (make-label-file-msg . args)
    (case make-label-file-msg
      ((dir?) #f)
      ((file?) #t)
      ((name) name)
      ((size) size)
      ((->string) (string-append "(file"
                                 " name='"
                                 name
                                 "'"
                                 " size="
                                 (number->string size)
                                 ")"))
      )))


(define (label->string lbl)
    (lbl '->string))


(define (cd loc dir)
  (cond
    ((equal? dir "/") (loc 'top))
    ((equal? dir "..") (loc 'up))
    (else (loc 'step-down
               (lambda (lbl)
                 (and (lbl 'dir?) (equal? (lbl 'name) dir)))))))



(define (handle-line line loc)
  (let ((parts (string-split line " ")))
    (if (equal? "$" (car parts))
      (if (equal? "cd" (cadr parts))
        (begin ;(print "cd " (caddr parts))
               (cd loc (caddr parts)))
        (begin ;(print "ls")
               loc ;ls is not needed
               ))
      (if (equal? "dir" (car parts))
        (begin ;(print "dir " (cadr parts))
               (loc 'add-child (make-label-dir (cadr parts))))
        (begin ;(print "file " (cadr parts))
               (loc 'add-child (make-label-file (cadr parts) (string->number (car parts)))))
        )
      )))

(define (make-root)
  (make-loc (make-node (make-label-dir "/")
                       '())
            (make-context-top)))

(define (read-tree-from-file port)
  (fold handle-line
        (make-root)
        (read-lines port)))



(define (calc-sizes root)
  (let loop ((children (root 'children))
             (acc-size 0)
             (acc-children '()))
    (cond
      ((null? children)
       (cons acc-children acc-size))
      ((((car children) 'label) 'file?)
       (loop (cdr children)
             (+ acc-size (((car children) 'label) 'size))
             acc-children))
      (else (let ((child (calc-sizes (car children))))
              (loop (cdr children)
                    (+ acc-size (cdr child))
                    (cons child acc-children)))))))


(define (sum-small-sizes root)
  (let loop ((children (car root))
             (acc-size 0))
    (cond
      ((null? children)
       (+ acc-size
          (if (< (cdr root) 100000)
            (cdr root)
            0)))
      (else (loop (cdr children)
                  (+ acc-size (sum-small-sizes (car children))))))))

(define (main args)
  (print (sum-small-sizes (calc-sizes (((read-tree-from-file (open-input-file (car args))) 'top) 'node))))
  )

