(import (chibi)
        (scheme file))

(define (compute current input)
  (let ((value (read input)))
    (if (eof-object? value) current
        (compute (+ current value) input))))

(display (compute 0 (open-input-file "test.input")))
(newline)
