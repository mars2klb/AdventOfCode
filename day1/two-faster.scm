(load-from-library "rb-tree")
(define *input* (with-input-from-file (car *arguments*) read-file))

(define (contains? lst value)
  (rbt-find lst value))

(define (consider current input known)
  (if (null? input) (consider current *input* known)
      (let ((new (+ current (string->number (car input)))))
	(format #t "~A~%" new)
	(if (contains? known new) new
	    (consider new (cdr input) (rbt-insert known new #t))))))

(let* ((tree (make-rbt <))
       (frequency (consider 0 '() (rbt-insert tree 0 #t))))
  (format #t "~A~%" frequency))
