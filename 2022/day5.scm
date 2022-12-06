(import (chibi)
        (chibi string)
        (chibi regexp))

(load "common.scm")

(define (list-set new-pile pile-offset obj)
  (cond
   ((= pile-offset 0) (cons obj (cdr new-pile)))
   ((null? list) '())
   (else (cons (car new-pile) (list-set (cdr new-pile) (- pile-offset 1) obj)))))

(define (ensure-len pile offset fill)
  (cond
   ((= offset 0) pile)
   ((null? pile) (cons fill (ensure-len pile (- offset 1) fill)))
   (else (cons (car pile) (ensure-len (cdr pile) (- offset 1) fill)))))

(define (build current-pile pile-offset crate)
  (let ((new-pile (ensure-len current-pile (+ pile-offset 1) '())))
    (list-set new-pile pile-offset (cons crate (list-ref new-pile pile-offset)))))

(define (read-crates)
  (let* ((line (read-line))
         (len (string-length line)))
    (if (equal? (string-ref line 1) #\1)
        '()
        (do ((i 0 (+ i 1))
             (coff 1 (+ coff 4))
             (crates (read-crates)
                     (if (equal? (string-ref line coff) #\space) crates
                         (build crates i (string-ref line coff)))))
            ((>= coff len) crates)))))

(define (numberize c)
  (string->number (string c)))

(define (read-moves)
  ;; we're going to be ridiculously inflexible because we know the input limits
  (let ((line (read-line)))
    (if (eof-object? line) '()
        (let ((count (numberize (string-ref line 5)))
              (from (numberize (string-ref line 12)))
              (to (numberize (string-ref line 17))))
          (cons (list count from to) (read-moves))))))

(define (move chart moves)
  (let* ((count (list-ref moves 1))
         (from-where (list-ref moves 2))
         (to-where (list-ref moves 3))
         (from (list-ref chart from-where))
         (to (list-ref chart to-where)))
  (show #t "  move: chart:" chart " moves:" moves nl)
  (show #t "   count:" count " from:" from " to:" to nl)
  (show #t "    foo:" (list-set! chart from-where (reverse (cdr (reverse from))))))
  chart)

(define (do-moves chart moves)
  (if (null? moves) chart
      (do-moves (move chart (car moves)) (cdr moves))))

(define (part1)
  (let* ((chart (read-crates))
         (_ (read-line))
         (moves (read-moves)))
    (show #t "chart:" chart nl)
    (show #t "moves:" moves nl)
    (do-moves chart moves)))

(part1)
