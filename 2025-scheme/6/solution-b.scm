#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-13))

;; We first load the input data in rows, even though it's semantically
;; to be understood in columns. Also, it just so happens that the
;; operators are on the last line of the input file, which naturally
;; shows up in the first row when loaded in a reversed manner. So we
;; keep that in first position and reverse the rest.
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

;; Using the list procedure to process rows in parallel and ends up
;; transposing a list of lists
(define (transpose ll)
  (apply map list ll))

;; Then take the string representations of operators and numbers and
;; make it a set of expressions that can be evaluated
(define (right-pad s)
  (cond ((= (string-length s) 1) (string-append s "_" "_" "_"))
        ((= (string-length s) 2) (string-append s "_" "_"))
        ((= (string-length s) 3) (string-append s "_"))
        (else s)))

(define (string->op s)
  (cond
   ((string=? s "*") *)
   ((string=? s "+") +)))

(define (something l)
  (cdr l))

(define (transposed->cephalopod ll)
  (define (loop remaining numbers)
    (if (null? remaining)
        numbers
        (append (cdr remaining) numbers)))
  (loop ll '()))

(define (numbers->cephalopod-numbers l)
  (let* ((padded (map string-reverse (map right-pad l)))
         (transposed (transpose (map string->list padded)))
         (cephalopod (transposed->cephalopod transposed)))
;;    (display cephalopod)
;;    (newline)
    transposed))

(define (construct-expr expr)
  (let ((op (car expr))
        (args (numbers->cephalopod-numbers (cdr expr))))
    (append (list op) args)))

;; (define (eval-expr expr)
;;   (let ((op (string->op (car expr)))
;;         (args (map string->number (cdr expr))))
;;     (apply op args)))

;; With all these helpers in place, we run the main program.
(define input-data (transpose (load-input-file "example.txt")))
;; (display (apply + (map eval-expr input-data)))

(display (reverse (map construct-expr input-data)))

(newline)
