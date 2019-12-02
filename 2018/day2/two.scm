(define *input* (with-input-from-file (car *arguments*) read-file))
(define *maxlen* (string-length (car *input*)))
(define *target* (- *maxlen* 1))

(define (prune left right)
  (if (or (null? left) (null? right)) (newline)
      (begin
	(if (char=? (car left) (car right))
	    (format #t "~A" (car left)))
	(prune (cdr left) (cdr right)))))

(define (gauge left right depth)
  (cond ((or (null? right) (null? left)) depth)
	((not (char=? (car left) (car right))) depth)
	(else
	 (gauge (cdr left) (cdr right) (+ 1 depth)))))

(define (rank box others score . last)
  (cond ((or (null? box) (null? others)) score)
	((string=? box (car others)) (rank box (cdr others) score))
	((= score (* 2 *maxlen*)) 0)
	((= score *target*) (prune (string->list box) (string->list (car last))))
	(else
	 (let ((left (gauge (string->list box) (string->list (car others)) 0))
	       (right (gauge (reverse (string->list box)) (reverse (string->list (car others))) 0)))
	   (rank box (cdr others) (+ left right) (car others))))))

(for-each (lambda (box)
	    (rank box *input* 0))
	  *input*)
