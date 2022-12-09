(import srfi-1)
(import chicken.io)


(define (top-insert top3 x)
  (print top3)
  (print x)
  (let ((t1 (car top3))
        (t2 (cadr top3))
        (t3 (caddr top3)))
    (cond
      ((> x t1)
       (list x t1 t2))
      ((> x t2)
       (list t1 x t2))
      ((> x t3)
       (list t1 t2 x))
      (else
        top3))))


(define (read-data port)
  (define (read-group)
    (let loop ((acc 0)
               (line (read-line port)))
      (cond
        ((eq? #!eof line) #f)
        ((equal? "" line) acc)
        (else (loop (+ acc (string->number line))
                    (read-line port))))))

  (let loop ((top3 '(0 0 0))
             (new-group (read-group)))
    (if new-group
      (loop (top-insert top3 new-group)
            (read-group))
      (apply + top3))))

(print (call-with-input-file "input_01_real.txt" read-data))

