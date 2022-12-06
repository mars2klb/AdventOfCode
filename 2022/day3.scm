(import (scheme small)
        (scheme base)
        (srfi 130) ; string
        (srfi 166))

(define (value of)
  (let ((asc (char->integer of)))
    (if (> asc 96)
        (- asc 96)
        (- asc 38))))

(define (three-way one two three)
  (if (null? one) #f
      (let ((cmp (car one)))
        (if
         (and (string-contains two (string cmp))
              (string-contains three (string cmp))) (value cmp)
              (three-way (cdr one) two three)))))

(define (compare left right)
  (if (null? right) #f
      (if (eq? (car left) (car right)) (car left)
             (compare left (cdr right)))))

(define (inspect line)
  (let* ((midpoint (exact (* 0.5 (string-length line))))
         (left (string->list (string-take line midpoint)))
         (right (string->list (string-drop line midpoint))))
    (let loop ((next left))
      (let ((duplicate-item (compare next right)))
        (if (not duplicate-item)
            (loop (cdr next))
            (value duplicate-item))))))

(define (churn tally in)
  (let ((line (read-line in)))
    (if (eof-object? line) tally
        (churn (+ (inspect line) tally) in))))

(define (churn2 tally in)
  (let ((one (read-line in))
        (two (read-line in))
        (three (read-line in)))
    (if (eof-object? one) tally
        (churn2 (+ (three-way (string->list one) two three) tally) in))))

(define (part1 in)
  (churn 0 in))

(define (part2 in)
  (churn2 0 in))

(let ((file (open-input-file "day3.input")))
  (show #t "part 1: " (part1 file) nl))

(let ((file (open-input-file "day3.input")))
  (show #t "part 2: " (part2 file) nl))
