(import (chibi)
        (chibi string))

(load "common.scm")

(define (get-range job)
  (let ((range (string-split job #\-)))
    (cons (string->number (car range)) (string->number (cadr range)))))

(define (complete-overlap? one two)
  (and (<= (car one) (car two)) (>= (cdr one) (cdr two))))

(define (some-overlap? one two)
  (or (and (<= (car one) (car two))
           (>= (cdr one) (car two)))
      (and (>= (car one) (cdr two))
           (<= (cdr one) (cdr two)))))

(define (handle line cmp)
  (let* ((elves (string-split line #\,))
         (left (get-range (car elves)))
         (right (get-range (cadr elves))))
    (if (or (cmp left right) (cmp right left)) 1 0)))

(define (churn func tally cmp in)
  (let ((line (read-line in)))
    (if (eof-object? line) tally
        (churn func (+ (func line cmp) tally) cmp in))))

(define (part1 in)
  (churn handle 0 complete-overlap? in))

(define (part2 in)
  (churn handle 0 some-overlap? in))

(run "day4.input" "part1" part1)
(run "day4.input" "part2" part2)
