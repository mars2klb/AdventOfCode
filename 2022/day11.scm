(import (chibi string)
        (chibi regexp)
        (scheme small)
        (scheme sort)
        (srfi 1)) ; for iota
(load "common.scm")

; TODO: look into record types to avoid all this getter/setter nonsense
(define *troop* '()) ; globals are icky in scheme, but I'm not a good lisper, yet
(define *magic-mod* 1)
(define *chill* #t)

(define (get-id monkey) (list-ref monkey 0))
(define (get-items monkey) (list-ref monkey 1))
(define (get-operation monkey) (list-ref monkey 2))
(define (get-test monkey) (list-ref monkey 3))
(define (get-on-true monkey) (list-ref monkey 4))
(define (get-on-false monkey) (list-ref monkey 5))
(define (get-inspections monkey) (list-ref monkey 6))

(define (set-items! monkey items) (list-set! monkey 1 items))
(define (set-inspections! monkey ins) (list-set! monkey 6 ins))
(define (set-monkey! monkey) (list-set! *troop* (get-id monkey) monkey))

(define (throw what from to)
  (let ((thrower from)
        (catcher (get-monkey to *troop*)))
    (set-items! thrower (cdr (get-items thrower)))
    (set-items! catcher (append (get-items catcher) (list what)))))

(define (get-monkey id troop)
  (cond ((null? troop) #f)
        ((equal? id (get-id (car troop))) (car troop))
        (else
         (get-monkey id (cdr troop)))))

(define (read-monkey)
  (let ((line (read-line)))
    (cond ((eof-object? line) #f)
          ((string-null? line) (read-monkey))
          (else
           (let* ((id (string->number (regexp-match-submatch (regexp-search '(+ digit) line) 0)))
                  (items (map string->number (string-split (substring (read-line) 18) #\,)))
                  (operation (string-split (substring (read-line) 23) #\space))
                  (test (string->number (substring (read-line) 21)))
                  (on-true (string->number (substring (read-line) 29)))
                  (on-false (string->number (substring (read-line) 30))))
             (set! *magic-mod* (* *magic-mod* test))
             (list id items operation test on-true on-false 0))))))

(define (adjust op old)
  (eval (list (string->symbol (car op)) old (if (equal? (cadr op) "old") old (string->number (cadr op))))))

(define (chill worry)
  (if *chill* (floor (/ worry 3)) worry))

(define (inspect monkey)
  (set-inspections! monkey (+ 1 (get-inspections monkey))))

(define (handle-monkey monkey)
  (let loop ((items (get-items monkey)))
    (if (null? items) #f
        (let* ((old (car items))
               (op (get-operation monkey))
               (worry (modulo (chill (adjust op old)) *magic-mod*))
               (test (get-test monkey)))
          (inspect monkey)
          (if (= (modulo worry test) 0)
              (throw worry monkey (get-on-true monkey))
              (throw worry monkey (get-on-false monkey)))
          (loop (get-items (get-monkey (get-id monkey) *troop*)))))))

(define (churn rest)
  (if (not (pair? rest)) *troop*
      (begin
        (handle-monkey (car rest))
        (churn (cdr rest)))))

(define (parse)
  (let ((monkey (read-monkey)))
    (if (not monkey) '()
        (cons monkey (parse)))))

(define (score)
  (let* ((results (list-sort (lambda (l r) (> (get-inspections l) (get-inspections r))) *troop*))
         (one (car results))
         (two (cadr results)))
    (* (get-inspections one) (get-inspections two))))

(define (run-part count)
  (set! *troop* (parse))
  (for-each (lambda (x) (churn *troop*)) (iota count))
  (score))

(define (part1)
  (run-part 20))

(define (part2)
  (set! *chill* #f)
  (run-part 10000))

(fire "day11.input" "part1" part1)
(fire "day11.input" "part2" part2)
