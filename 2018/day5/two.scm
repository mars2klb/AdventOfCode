(set! *arguments* (flatten (list *arguments* "two")))
(load "one.scm")

(define *input* (car (with-input-from-file (car *arguments*) read-file)))

(define *letters* (string->list "abcdefghijklmnopqrstuvwxyz"))

(define (react letter polymers . reacted)
  (if (null? polymers) (list->string (flatten reacted))
      (if (char=? (char-upcase (car polymers)) (char-upcase letter))
	  (react letter (cdr polymers) reacted)
	  (react letter (cdr polymers) (append reacted (car polymers))))))

(define (reduce letter)
  (let ((reacted (react letter (string->list *input*))))
    (crush reacted)))

(define (shortest reductions best)
  (if (null? reductions) best
      (if (or (= 0 (string-length best))
	      (< (string-length (car reductions))
		 (string-length best)))
	  (shortest (cdr reductions) (car reductions))
	  (shortest (cdr reductions) best))))

(let* ((reductions (map reduce *letters*))
       (shorty (shortest reductions "")))
  (format #t "shortest: ~A ~A~%" shorty (string-length shorty)))
