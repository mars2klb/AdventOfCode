(import (chibi io)
        (chibi string)
        (scheme small)
        (srfi 132)
        (srfi 166))

(define (gather tally calories in)
  ;; (show #t "tally:" tally " calories:" calories nl)
  (cond ((or (eof-object? calories)
             (string-null? calories)) tally)
        (else (gather (+ tally (string->number calories)) (read-line in) in))))

(define (make-list current in)
  (let ((calories (gather 0 (read-line in) in)))
    ;; (show #t "make-list calories:" calories nl)
    (if (zero? calories) current
        (cons calories (make-list '() in)))))

(let ((file (open-input-file "day1.input")))
  (show #t "part 1:" (car (list-sort > (make-list '() file))) nl))

(let ((file (open-input-file "day1.input")))
  (let ((heavies (list-sort > (make-list '() file))))
    (show #t "part 2:" (+ (car heavies) (cadr heavies) (caddr heavies)) nl)))

