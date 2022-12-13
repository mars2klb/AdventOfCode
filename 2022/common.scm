(import (chibi term ansi)
        (chibi io)
        (srfi 166))

;; TODO: passing the label _and_ the function is stupid
(define (fire filename label func)
  (current-input-port (open-input-file filename))
  (show #t (bold (cyan label)) ":" (func) nl))

(define (run filename label func)
  (let ((file (open-input-file filename)))
    (show #t label ":" (func file) nl)))
