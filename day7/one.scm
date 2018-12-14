(load-from-library "hash-table")
(load-from-library "regex")
(load-from-library "list-to-set")

(define *input* (with-input-from-file (car *arguments*) read-file))
(define *step-rgx* (re-comp "^Step \\([A-Z]\\).*step.\\([A-Z]\\)"))

(define (extract line offsets)
  (substring line (car offsets) (cadr offsets)))

(define (parse x)
  (let ((matches (cdr (re-match *step-rgx* x))))
    (cons (extract x (cadr matches))
	  (extract x (car matches)))))

(define (known input seen)
  (if (null? input) (list->set seen)
      (known (cdr input) (flatten (append seen (list (caar input) (cdar input)))))))

(define (prune thing lst . pruned)
  (if (null? lst) (flatten pruned)
      (let ((next (car lst)))
	;; (format #t "    prune: ~A from ~A~%" thing lst)
	(if (not (string=? thing next)) (prune thing (cdr lst) (append pruned next))
	    (prune thing (cdr lst) pruned)))))

(define (contains? thing lst)
  (if (null? lst) #f
      (if (string=? (car lst) thing) #t
	  (contains? thing (cdr lst)))))

(define (blocked? node table)
  ;; (format #t "blocked? ~A ~A~%" node (hash-table->alist table))
  (hash-table-ref table node))

(define (process node table)
  (let ((alist (hash-table->alist table)))
    ;; (format #t "  alist: ~A~%" alist)
    ;; (format #t "-----------~%")
    (for-each (lambda (deps)
		;; (format #t " deps: ~A~%" deps)
		(if (contains? node (cdr deps))
		    (let ((key (car deps))
			  (new (prune node (cdr deps))))
		      ;; (format #t "  pruned ~A: from: ~A => ~A~%" node (cdr deps) new)
		      ;; (format #t "[~A]" deps)
		      (if (null? new) (hash-table-remove! table key)
			  (hash-table-set! table key new))))
		)
	      alist)))

(define (who-goes? input table)
  ;; (format #t "who-goes? ~A ~A~%" input (hash-table->alist table))
  (if (null? input) #f
      (let ((node (car input)))
	(if (not (blocked? node table)) node
	    (who-goes? (cdr input) table)))))

(define (fill-table input table)
  (if (null? input) table
      (let ((entry (hash-table-ref table (caar input)))
	    (key (caar input))
	    (value (cdar input)))
	(if (not entry) (hash-table-set! table (caar input) (list value))
	    (hash-table-set! table key (flatten (append (car entry) value))))
	(fill-table (cdr input) table))))

(define (churn entries table result)
  (if (null? entries) result
      (let ((node (who-goes? entries table)))
	(process node table)
	(churn (prune node entries) table (format #f "~A~A" result node)))))

(define (dump table)
  (let ((t (hash-table->alist table)))
    (for-each (lambda (x)
		(format #t "~A depends on ~A~%" (car x) (cdr x)))
	      t)))

(if (= 1 (length *arguments*))
    (let* ((input (map parse *input*))
	   (table (fill-table input (make-hash-table)))
	   (candidates (sort string<? (known input '()))))
      (format #t "~A~%" (churn candidates table ""))
      (dump table)))
