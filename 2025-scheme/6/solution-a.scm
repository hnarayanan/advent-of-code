#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-13))

;; Load the input data in rows, though it's semantically to be read in
;; columns
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
              (map string-tokenize lines)
              (loop (cons line lines)))))
      (loop '()))))

;; Use list to process rows in parallel and end up transposing the
;; input
(define (process-input-data rows)
  (apply map list rows))

;; Then take the string representations of operators and numbers and
;; make it a set of expressions that can be evaluated
(define (string->op s)
  (cond
   ((string=? s "*") *)
   ((string=? s "+") +)))

(define (eval-expr expr)
  (let ((op (string->op (car expr)))
        (args (map string->number (cdr expr))))
    (apply op args)))

;; With all these helpers in place, we run the main program.
(define input-data (process-input-data (load-input-file "input.txt")))
(display (apply + (map eval-expr input-data)))

(newline)
