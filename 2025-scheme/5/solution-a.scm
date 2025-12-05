#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-11))

;; Given a file containing fresh ingredient ID ranges and available
;; ingredient IDs, get two lists out of them. First a list of range
;; bounds and then a list of individual IDs.
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
              (begin
                (reverse lines))
              (loop (cons line lines)))))
      (loop '()))))

(define (classify s)
  (cond
   ((string-index s #\-) 'range)
   ((string-null? s) 'spacer)
   (#t 'id)))

(define (parse-range s)
  (let ((parts (string-split s #\-)))
    (cons (string->number (car parts))
          (string->number (cadr parts)))))

(define (parse-id s)
  (string->number s))

(define (parse-input-data data)
  (define (loop lines ranges ids)
    (if (null? lines)
        (values (reverse ranges) (reverse ids))
        (let* ((line (car lines))
               (type (classify line)))
          (cond
           ((eq? type 'range)
            (loop (cdr lines) (cons (parse-range line) ranges) ids))
           ((eq? type 'id)
            (loop (cdr lines) ranges (cons (parse-id line) ids)))
           ((eq? type 'spacer)
            (loop (cdr lines) ranges ids))))))
  (loop data '() '()))


(define input-data (load-input-file "example.txt"))
(let-values (((ranges ids) (parse-input-data input-data)))
  (display ranges)
  (newline)
  (display ids))
(newline)
