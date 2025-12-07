#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim))

;; We first load the input data in rows.
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
               (reverse lines)
              (loop (cons line lines)))))
      (loop '()))))

;; With all these helpers in place, we run the main program.
(let* ((input (load-input-file "example.txt")))
  (display input)
  (newline))
