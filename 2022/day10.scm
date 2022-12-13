(import (chibi)
        (chibi string))
(load "common.scm")

(define (compute reg opcodes)
  ; (show #t "opcodes:" opcodes nl)
  (cond ((or
          (null? opcodes)
          (string-null? (car opcodes))) '())
        ((equal? (car opcodes) "noop") (append (list reg) (compute reg (cdr opcodes))))
        (else
         (let* ((value (string->number (cadr (string-split (car opcodes)))))
                (value+reg (+ reg value)))
           ;(show #t " add:" value " to:" reg nl)
           (append (list reg value+reg) (compute value+reg (cdr opcodes)))))))

(define (slurp)
  (let loop ((line (read-line)))
    (if (eof-object? line) '()
        (cons line (slurp)))))

(define (part1)
  (let* ((points '(20 60 100 140 180 220))
         (values (compute 1 (slurp)))
         (tape (map (lambda (i)
                      ;(show #t "times " i " by " (list-ref values (- i 2)) nl)
                      (* i (list-ref values (- i 2))))
                    points)))
    ;(show #t "tape:" tape nl)
    ;(show #t "values:" values nl)
    (apply + tape)))


(fire "day10.input" "part1" part1)
