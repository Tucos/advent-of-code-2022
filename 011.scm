(import srfi-1)
(import chicken.io)


(define (read-data port)
  (define (read-group)
    (let loop ((acc 0)
               (line (read-line port)))
      (cond
        ((eq? #!eof line) #f)
        ((equal? "" line) acc)
        (else (loop (+ acc (string->number line))
                    (read-line port))))))

  (let loop ((acc 0)
             (new-group (read-group)))
    (if new-group
      (loop (max new-group acc)
            (read-group))
      acc)))

#;(print (find-greatest-sum data))
(print (call-with-input-file "input_01_real.txt" read-data))
