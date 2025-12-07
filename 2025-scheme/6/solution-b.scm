#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-13))

;; We first load the input data in rows, even though it's semantically
;; to be understood in columns. Also, it just so happens that the
;; operators are on the last line of the input file, which naturally
;; shows up in the first row when loaded in a reversed manner. So we
;; keep that in first position and reverse the rest.
(define (rightpad s)
  (cond ((= (string-length s) 1) (string-append s "s" "s"))
        ((= (string-length s) 2) (string-append s "s"))
        (else s)))

(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
              (map string-tokenize
                   (cons (car lines)
                         (reverse (cdr lines))))
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
(define input-data (process-input-data (load-input-file "example.txt")))
;; (display (apply + (map eval-expr input-data)))

(display input-data)

(newline)
