(import (chibi)
        (chibi string))
(load "common.scm")

(define (walk)
  (let loop ((tree '())
             (line (read-line)))
    (if (eof-object? line) tree
        (cond
         ((string-prefix? "$ cd" line)
          (let ((dest (substring line 5)))
            (if (equal? ".." dest) tree
                (append tree (list (cons dest (walk)))))))
         ((or (string-prefix? "dir" line)
              (string-prefix? "$ ls" line)) (loop tree (read-line)))
         (else
          (loop (append tree (list line)) (read-line)))))))

(define (part1)
  (walk))

(fire "day7.input" "part1" part1)
