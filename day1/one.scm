(define (compute current input)
  (if (null? input) current
      (compute (+ current (string->number (car input))) (cdr input))))

(format #t "~A~%" (compute 0 (with-input-from-file "input" read-file)))
