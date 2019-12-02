(load-from-library "regex")

(define *input* (with-input-from-file (car *arguments*) read-file))
(define *coord-rgx* (re-comp "#\\([0-9]+\\).@.\\([0-9]+\\),\\([0-9]+\\):.\\([0-9]+\\)x\\(.*\\)"))
(define *dimensions* 0)

(define (claim:id claim)
  (car claim))
(define (claim:x claim)
  (cadr claim))
(define (claim:y claim)
  (caddr claim))
(define (claim:width claim)
  (cadddr claim))
(define (claim:height claim)
  (cadddr (cdr claim)))

(define (extract entry matches)
  (let ((offsets (cdr matches)))
    (list (string->number (substring entry (caar offsets) (cadar offsets)))
	  (string->number (substring entry (caar (cdr offsets)) (cadar (cdr offsets))))
	  (string->number (substring entry (caar (cddr offsets)) (cadar (cddr offsets))))
	  (string->number (substring entry (caar (cdddr offsets)) (cadar (cdddr offsets))))
	  (string->number (substring entry (caar (cddddr offsets)) (cadar (cddddr offsets)))))))

(define (ponder x)
  (let ((matches (re-match *coord-rgx* x)))
    (if (not matches) (error "failed to parse entry" x)
	(extract x matches))))

(define (get-dimensions claims max)
  (if (null? claims) max
      (let* ((claim (car claims))
	     (width (+ 1 (claim:x claim) (claim:width claim)))
	     (height (+ 1 (claim:y claim) (claim:height claim))))
	(cond ((< max width) (get-dimensions (cdr claims) width))
	      ((< max height) (get-dimensions (cdr claims) height))
	      (else
	       (get-dimensions (cdr claims) max))))))

(define (offset x y w)
  (+ x (* y w)))

(define (mark-row grid offset left)
  (if (= left 0) grid
      (begin
	(vector-set! grid offset (+ 1 (vector-ref grid offset)))
	(mark-row grid (+ 1 offset) (- left 1)))))

(define (mark grid claims)
  (if (null? claims) grid
      (let* ((claim (car claims))
	     (x (claim:x claim))
	     (y (claim:y claim))
	     (w (claim:width claim))
	     (h (claim:height claim)))
	(let loop ((start (offset x y *dimensions*))
		   (rows 0))
	  (mark-row grid start w)
	  (if (< rows (- h 1)) (loop (offset x (+ y 1 rows) *dimensions*) (+ rows 1))
	      grid))
	(mark grid (cdr claims)))))

(define (tally inches borked)
  (cond ((null? inches) borked)
	((> (car inches) 1) (tally (cdr inches) (+ 1 borked)))
	(else
	 (tally (cdr inches) borked))))

(define (dump grid width offset)
  (if (null? grid) #f
      (begin
	(display (car grid))
	(if (and (= 0 (modulo offset width))) (newline))
	(dump (cdr grid) width (+ 1 offset)))))

(if (= 1 (length *arguments*))
    (let* ((claims (map ponder *input*))
	   (dim (get-dimensions claims 0))
	   (grid (make-vector (* dim dim) 0)))
      (format #t "dimensions: ~Ax~A~%" dim dim)
      (set! *dimensions* dim)
      (mark grid claims)
      (format #t "tally: ~A~%" (tally (vector->list grid) 0))
      ;; (dump (vector->list grid) dim 1)
      ))
