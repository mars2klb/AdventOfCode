(import (chibi)
        (chibi string))
(load "common.scm")

(define (compute reg opcodes)
  (cond ((or
          (null? opcodes)
          (string-null? (car opcodes))) '())
        ((equal? (car opcodes) "noop") (append (list reg) (compute reg (cdr opcodes))))
        (else
         (let* ((value (string->number (cadr (string-split (car opcodes)))))
                (value+reg (+ reg value)))
           (append (list reg value+reg) (compute value+reg (cdr opcodes)))))))

(define (slurp)
  (let loop ((line (read-line)))
    (if (eof-object? line) '()
        (cons line (slurp)))))

(define (render idx reg)
  (let ((pos (remainder idx 40)))
    (if (or (= reg pos)
            (= reg (- pos 1))
            (= reg (+ pos 1)))
        (display (yellow "#"))
        (display "."))
    (if (= (modulo idx 40) 39) (newline))
    (+ 1 idx)))

(define (part1)
  (let* ((points '(20 60 100 140 180 220))
         (values (compute 1 (slurp)))
         (tape (map (lambda (i)
                      (* i (list-ref values (- i 2))))
                    points)))
    (apply + tape)))

(define (part2)
  (let ((values (append '(1) (compute 1 (slurp)))))
    (let loop ((i 0))
      (cond ((= 0 i) (begin (display (yellow "#")) (loop 1)))
            ((>= i (length values)) #f)
            (else
             (loop (render i (list-ref values i))))))))

(fire "day10.input" "part1" part1)
(fire "day10.input" "part2" part2)
