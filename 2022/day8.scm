(import (chibi)
        (chibi io))
(load "common.scm")

(define above '(0 . -1))
(define below '(0 . 1))
(define left '(-1 . 0))
(define right '(1 . 0))

(define (tree-height tree forest)
  (let ((x (car tree))
        (y (cdr tree)))
    (string->number (string (string-ref (number->string (list-ref forest y)) x)))))

(define (next tree direction)
   (cons (+ (car tree) (car direction)) (+ (cdr tree) (cdr direction))))

(define (visible? tree forest direction)
  (let ((forest-width (length (string->list (number->string (list-ref forest 0)))))
        (forest-height (length forest))
        (height (tree-height tree forest)))
    (show #t "forest width:" forest-width nl)
    (show #t "forest height:" forest-height nl)
    (let loop ((other (next tree direction)))
      (show #t " tree:" tree " other:" other nl)
      (cond ((or (< (car other) 0)
                 (< (cdr other) 0)
                 (>= (car other) forest-width)
                 (>= (cdr other) forest-height)) #t)
            ((> height (tree-height other forest)) (loop (next other direction)))
            (else #f)))))

(define (visible-somewhere? tree forest)
  (or (visible? tree forest left)
      (visible? tree forest right)
      (visible? tree forest above)
      (visible? tree forest below)))

(define (range value)
  (if (> value 0) (list value (range (- value 1))) 0))

;; (define (trees forest . acc)
;;   (let ((width (length (string->list (number->string (list-ref forest 0)))))
;;         (height (length forest)))



(define (score forest)
  (let ((width (length (string->list (number->string (list-ref forest 0)))))
        (height (length forest)))
    (let row-loop ((row-index 1)
                   (visible (+ (* 2 width) (* 2 height))))
      (show #t "row " row-index " of " height nl)
      (if (>= row-index height) visible
          (let column-loop ((col-index 1))
            (show #t " col " col-index " of " width nl)
            (if (>= col-index width) (row-loop (+ 1 row-index) visible)
                (let ((tree (cons col-index row-index)))
                  (if (visible-somewhere? tree forest)
                      (column-loop (+ 1 col-index) (+ 1 visible))
                      (column-loop (+ 1 col-index) visible)))))))))

(define (part1)
  (show #t (range 5) nl)
  (let ((forest (port->list read (current-input-port))))
    (score forest)))

(fire "day8.test" "part1" part1)
