(set! *arguments* (flatten (list *arguments* "two")))
(load "one.scm")

(define (sleepy-minute guards badge minute)
  (if (null? guards) (cons badge minute)
      (let* ((cop (caar guards))
	     (minutes (vector->list (cdar guards)))
	     (best (best-minute minutes 0 0 0)))
	(if (> (cdr best) (cdr minute)) (sleepy-minute (cdr guards) cop best)
	    (sleepy-minute (cdr guards) badge minute)))))

(let* ((guards (parse (quicksort order *input*) (make-hash-table)))
       (punk (car (sleepiest (hash-table->alist guards) 0 0)))
       (minutes (vector->list (car (hash-table-ref guards punk))))
       (results (sleepy-minute (hash-table->alist guards) 0 '(0 . 0))))
  (format #t "Answer: ~A~%" (* (string->number (car results)) (cadr results))))
