(import (scheme small)
        (srfi 130)
        (srfi 166))

(define values '((A . 1) (X . 1) (B . 2) (Y . 2) (C . 3) (Z . 3)))

(define (value play)
  (cdr (assq (string->symbol play) values)))

(define (to-beat them)
  (cond ((equal? them "A") "Y")
        ((equal? them "B") "Z")
        ((equal? them "C") "X")))

(define (to-lose them)
  (cond ((equal? them "A") "Z")
        ((equal? them "B") "X")
        ((equal? them "C") "Y")))

(define (win? us them)
  (equal? us (to-beat them)))

(define (tie? us them)
  (eq? (value us) (value them)))

(define (score us them)
  (cond ((tie? us them) (+ 3 (value us)))
        ((win? us them) (+ 6 (value us)))
        (else (value us))))

(define (play1 round)
  (let* ((plays (string-split round " "))
         (them (car plays))
         (us (cadr plays)))
    (score us them)))

(define (play2 round)
  (let* ((plays (string-split round " "))
         (them (car plays))
         (cmd (cadr plays)))
    (cond ((equal? cmd "X") (score (to-lose them) them))
          ((equal? cmd "Y") (score them them))
          ((equal? cmd "Z") (score (to-beat them) them)))))

(define (churn func tally in)
  (let ((line (read-line in)))
    (if (eof-object? line) tally
        (churn func (+ (func line) tally) in))))

(define (part1 in)
  (churn play1 0 in))

(define (part2 in)
  (churn play2 0 in))

(let ((file (open-input-file "day2.input")))
  (show #t "part 1: " (part1 file) nl))

(let ((file (open-input-file "day2.input")))
  (show #t "part 2: " (part2 file) nl))
