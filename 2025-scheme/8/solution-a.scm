#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim))

;; Load input file.
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
               (reverse lines)
               (loop (cons (map string->number (string-split line #\,)) lines)))))
      (loop '()))))


;; Define some helper procedures.
(define (square x)
  (* x x))

(define (distance p1 p2)
  (let ((dx (- (car p1) (car p2)))
        (dy (- (cadr p1) (cadr p2)))
        (dz (- (caddr p1) (caddr p2))))
    (sqrt (+ (square dx) (square dy) (square dz)))))



;; With all these helpers in place, we run the main program.
(let* ((numbers (load-input-file "example.txt")))
  (display numbers)
  (newline)
  (display (car numbers))
  (newline)
  (display (cadr numbers))
  (newline)
  (display (distance (car numbers) (cadr numbers)))
  (newline))
