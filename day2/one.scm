(load-from-library "hash-table")

(define *input* (with-input-from-file (car *arguments*) read-file))

(define (count word)
  (let ((letters (string->list word))
	(ht (make-hash-table)))
    (for-each (lambda (c)
		(let ((ticks (hash-table-ref ht c)))
		  (if (not ticks) (hash-table-set! ht c 1)
		      (hash-table-set! ht c (+ 1 (car ticks))))))
	      letters)
    (hash-table->alist ht)))

(define (check word target)
  (cond ((null? word) #f)
	((= (cdar word) target) #t)
	(else
	 (check (cdr word) target))))

(let ((doubles 0)
      (triples 0)
      (counted (map count *input*)))
  (for-each (lambda (word)
	      (if (check word 2) (set! doubles (+ 1 doubles)))
	      (if (check word 3) (set! triples (+ 1 triples))))
	    counted)
  (format #t "~A~%" (* doubles triples)))
