#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-13))

;; We first load the input data in rows. It just so happens that the
;; operators are on the last line of the input file, which naturally
;; shows up in the first row when loaded in a reversed manner. So we
;; keep that in first position and reverse the rest.
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
              (cons
               (car lines)
               (reverse (cdr lines)))
              (loop (cons line lines)))))
      (loop '()))))

;; Parsing this input file is a little tricky, so we rely on the
;; operator row to determine what the natural column delimiters are.
;; This took me nearly half a day to arrive at.
(define (find-column-starts line)
  (define (loop i remaining positions)
    (if (null? remaining)
        (reverse positions)
        (if (char=? (car remaining) #\space)
            (loop (1+ i) (cdr remaining) positions)
            (loop (1+ i) (cdr remaining) (cons i positions)))))
  (loop 0 (string->list line) '()))

(define (parse line column-starts)
  (define width (string-length line))
  (define (loop cols result)
    (if (null? (cdr cols))
        (reverse (cons (substring line (car cols) width) result))
        (loop (cdr cols)
              (cons (substring line (car cols) (cadr cols)) result))))
  (loop column-starts '()))

(define (transpose ll)
  (apply map list ll))

(define (chars->number chars)
  (let ((digits (filter char-numeric? chars)))
    (string->number (list->string digits))))

(define (column->numbers col)
  (let* ((char-lists (map string->list col))
         (transposed (transpose char-lists))
         (reversed (reverse transposed)))
    (filter number? (map chars->number reversed))))

(define (string->op s)
  (cond
   ((string=? s "*") *)
   ((string=? s "+") +)))

;; With all these helpers in place, we run the main program.
(define input (load-input-file "input.txt"))
(define column-starts (find-column-starts (car input)))
(define operators (string-tokenize (car input)))
(define input-numbers (map (lambda (line) (parse line column-starts))
                           (cdr input)))
(define columns (transpose input-numbers))
(define number-lists (map column->numbers columns))

(define result (apply + (map (lambda (op nums) (apply (string->op op) nums))
              operators
              number-lists)))
(display result)
(newline)
