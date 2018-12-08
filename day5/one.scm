(define *input* (car (with-input-from-file (car *arguments*) read-file)))

(define (match? lhs rhs)
  (and
   (char=? (char-upcase lhs) (char-upcase rhs))
   (or
    (and
     (char-lower-case? lhs)
     (char-upper-case? rhs))
    (and
     (char-lower-case? rhs)
     (char-upper-case? lhs)))))

(define (collapse full . condensed)
  (cond ((null? full) (list->string (flatten condensed)))
	((null? (cdr full)) (list->string (flatten (append condensed (car full)))))
	((match? (car full) (cadr full))
	 (collapse (cddr full) condensed))
	(else
	 (collapse (cdr full) (append condensed (car full))))))

(define (crush blob . last)
  (if (null? last) (crush blob (collapse (string->list blob)))
      (let ((shrunken (car last)))
	(if (= (string-length blob) (string-length shrunken)) shrunken
	    (crush shrunken (collapse (string->list shrunken)))))))

(if (= (length *arguments*) 1)
    (let ((result (crush *input*)))
      (format #t "~A~%" result)
      (format #t "Result: ~A~%" (string-length result))))
