(define *input* (with-input-from-file (car *arguments*) read-file))

(define (contains? lst value)
  (cond ((null? lst) #f)
	((= (car lst) value) #t)
	(else
	 (contains? (cdr lst) value))))

(define (consider current input known)
  (if (null? input) (consider current *input* known)
      (let ((new (+ current (string->number (car input)))))
	(format #t "~A~%" new)
	(if (contains? known new) new
	    (consider new (cdr input) (flatten (list known new)))))))

(let ((frequency (consider 0 '() '(0))))
  (format #t "~A~%" frequency))
