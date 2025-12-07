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
(define (find-column-starts l)
  (define (loop i remaining positions)
    (if (null? remaining)
        (reverse positions)
        (if (char=? (car remaining) #\space)
            (loop (1+ i) (cdr remaining) positions)
            (loop (1+ i) (cdr remaining) (cons i positions)))))
  (loop 0 (string->list l) '()))

(define (parse ll column-starts)
  (display ll)
  (newline)
  (display column-starts)
  (newline))

;; ;; Using the list procedure to process rows in parallel and ends up
;; ;; transposing a list of lists
;; (define (transpose ll)
;;   (apply map list ll))

(define (string->op s)
  (cond
   ((string=? s "*") *)
   ((string=? s "+") +)))

;; ;; (define (eval-expr expr)
;; ;;   (let ((op (string->op (car expr)))
;; ;;         (args (map string->number (cdr expr))))
;; ;;     (apply op args)))

;; ;; With all these helpers in place, we run the main program.
(define input (load-input-file "example.txt"))
(define column-starts (find-column-starts (car input)))
(define input-numbers (parse (cdr input) column-starts))
(define operators (string-tokenize (car input)))

;; ;; (define input-data (transpose (load-input-file "example.txt")))
;; ;; (display (apply + (map eval-expr input-data)))

;; ;; (display (reverse (map construct-expr input-data)))
(display operators)

(newline)
