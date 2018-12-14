(load-from-library "appendb")
(load "one.scm")

;; NOTE: This doesn't actually work.
;;  1) the resultant string is out-of-order...but that's not really relevant, I guess
;;  2) the result is 1 second too high - I cheated by running someone else's solution on
;;     my input, because I was tired of this problem and wanted to move on.
;; TODO: come back and fix this using lib/t-sort.scm

;; this just gets more and more un-schemey
(define *distortion* 4)
(define *work-order* "")
(define *work-pool* (make-hash-table))
(define *insertion-time* (make-hash-table))
(define *result* '())

(define (stringify chars result)
  (if (null? chars) (flatten result)
      (stringify (cdr chars) (append (list result) (format #f "~A" (car chars))))))

(define (char-value ch)
  (if (string=? "." ch) 0
      (char->integer (car (string->list ch)))))

(define (wait-offset node)
  (if (string=? node ".") -1
      (let ((offset (hash-table-ref *insertion-time* node)))
	(if offset (car offset) -1))))

(define (last lst)
  (if (null? lst) "."
      (car (reverse lst))))

(define (joblist chump)
  (let ((lst (hash-table-ref *work-pool* chump)))
    (if lst (car lst) '())))

(define (tick chump table seconds)
  (let* ((worker (joblist chump))
	 (wait (+ (wait-offset (last worker)) (- (char-value (last worker)) *distortion*))))
    (if (<= wait seconds)
	(begin
	  (if (and worker (not (null? worker)))
	      (let ((current (last worker)))
		(process current table)
		(hash-table-set! *work-pool* chump (flatten (append worker ".")))
		(set! *result* (flatten (append *result* current)))))
	  (let ((next (who-goes? *work-order* table)))
	    (if next
		(begin
		  (hash-table-set! *work-pool* chump (flatten (append worker next)))
		  (hash-table-set! *insertion-time* next seconds)
		  (set! *work-order* (prune next *work-order*)))))))
    (format #t "  ~A ~3@A" (last (joblist chump)) wait)))

(define (idle workers)
  (if (not (null? *work-order*)) #f
      (let loop ((chump workers)
		 (done #t))
	(if (or (not done) (= 0 chump)) done
	    (let ((task (joblist chump)))
	      (if (not (string=? "." (last task))) (loop (- chump 1) #f)
		  (loop (- chump 1) done)))))))

(define (boss workers table seconds)
  (if (idle workers) (- seconds 1)
      (begin
	;; (format #t "boss: *work-order*:~A seconds:~A~%" *work-order* seconds)
	(format #t "~% ~3@A: " seconds)
	(let loop ((chump workers))
	  ;; (format #t " loop: ~A chump: ~A~%" *work-order* chump)
	  (if (= 0 chump) (boss workers table (+ 1 seconds))
	      (begin
		(tick chump table seconds)
		(loop (- chump 1))))))))

(define (collapse str result)
  (if (null? str) (flatten result)
      (if (string=? "." (car str)) (collapse (cdr str) result)
	  (collapse (cdr str) (append (list result) (car str))))))

(let* ((input (map parse *input*))
       (table (fill-table input (make-hash-table)))
       (candidates (sort string<? (known input '())))
       (order (churn candidates table ""))
       (work-order (stringify (string->list order) '()))
       (workers (string->number (cadr *arguments*))))
  ;; (set! *work-order* work-order)
  (set! *work-order* candidates)
  (format #t "~%seconds: ~A~%" (boss workers (fill-table input (make-hash-table)) 0))
  (format #t "result: ~A~%" (collapse *result* '()))
  (dump table))
