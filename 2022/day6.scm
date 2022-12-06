(import (chibi)
        (chibi io)
        (chibi string)
        (srfi 166))

(load "common.scm")

(define (unique? ch str)
  (= 1 (string-count str (lambda (c) (eqv? c ch)))))

(define (marker? str)
  (let loop ((for (string->list str)))
    (if (null? for) #t
        (if (unique? (car for) str)
            (loop (cdr for))
            #f))))

(define (analyze codes index size)
  (if (marker? (substring codes index size)) index
      (analyze codes (+ 1 index) (+ 1 size))))

(define (part1)
  (let ((line (read-line)))
    (+ 4 (analyze line 0 4))))

(define (part2)
  (let ((line (read-line)))
    (+ 14 (analyze line 0 14))))

(fire "day6.input" "part1" part1)
(fire "day6.input" "part2" part2)
