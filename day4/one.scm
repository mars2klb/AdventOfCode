(load-from-library "quicksort")
(load-from-library "hash-table")
(load-from-library "regex")

(define *input* (with-input-from-file (car *arguments*) read-file))
(define *guard-rgx* (re-comp ".*Guard #\\([0-9]+\\).*"))
(define *asleep-rgx* (re-comp ".*:\\([0-9][0-9]\\)].falls"))
(define *awake-rgx* (re-comp ".*:\\([0-9][0-9]\\)].wakes"))

(define (str line coords)
  (substring line (car coords) (cadr coords)))

(define (extract rgx entry)
  (let ((matches (re-match rgx entry)))
    (if matches (str entry (cadr matches)) #f)))

(define (guard? entry)
  (extract *guard-rgx* entry))

(define (asleep? entry)
  (extract *asleep-rgx* entry))

(define (awake? entry)
  (extract *awake-rgx* entry))

(define (mark asleep nap guards badge)
  (let ((nap-time (car (hash-table-ref guards badge))))
    (let loop ((tick asleep))
      (if (<= (+ asleep nap) tick) guards
	  (begin
	    (vector-set! nap-time tick (+ 1 (vector-ref nap-time tick)))
	    (hash-table-set! guards badge nap-time)
	    (loop (+ tick 1)))))))

(define (ponder entries guards badge)
  (if (null? entries) (cons guards entries)
      (let* ((asleep (asleep? (car entries)))
	     (awake (awake? (cadr entries)))
	     (nap 0))
	(if (or (not asleep) (not awake)) (cons guards entries)
	    (let ((nap (- (string->number awake) (string->number asleep))))
	      (ponder (cddr entries) (mark (string->number asleep) nap guards badge) badge))))))

(define (tally naps minutes)
  (if (null? naps) minutes
      (tally (cdr naps) (+ (car naps) minutes))))

(define (sleepiest guards badge sleep)
  (if (null? guards) (cons badge sleep)
      (let ((cop (caar guards))
	    (nap (tally (vector->list (cdar guards)) 0)))
	(if (> nap sleep) (sleepiest (cdr guards) cop nap)
	    (sleepiest (cdr guards) badge sleep)))))

(define (best-minute minutes offset score best)
  (if (null? minutes) (cons best score)
      (if (> (car minutes) score) (best-minute (cdr minutes) (+ 1 offset) (car minutes) offset)
	  (best-minute (cdr minutes) (+ 1 offset) score best))))

(define (parse entries guards)
  (if (null? entries) guards
      (let* ((entry (car entries))
	     (badge (guard? entry)))
	(if badge
	    (begin
	      (if (not (hash-table-ref guards badge))
		  (hash-table-set! guards badge (make-vector 60 0)))
	      (let* ((result (ponder (cdr entries) guards badge))
		     (g (car result))
		     (new-entries (cdr result)))
		(parse new-entries g)))
	    (error "unexpected non-guard record" (car entries))))))

(define (order lhs rhs)
  (string<=? (substring lhs 0 19) (substring rhs 0 19)))

(if (= (length *arguments*) 1)
    (let* ((guards (parse (quicksort order *input*) (make-hash-table)))
	   (punk (car (sleepiest (hash-table->alist guards) 0 0)))
	   (minutes (vector->list (car (hash-table-ref guards punk))))
	   (minute (car (best-minute minutes 0 0 0))))
      (format #t "Guard: ~A~%" punk)
      (format #t "Minute: ~A~%" minute)
      (format #t "Answer: ~A~%" (* (string->number punk) minute))))
