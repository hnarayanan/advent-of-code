#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-1)
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

;; We now have enough information to check whether a given ingredient
;; ID belongs to any of the fresh ingredient ID ranges.
(define (in-range? n low high)
  (<= low n high))

(define (in-any-range? n ranges)
  (any (lambda (range) (in-range? n (car range) (cdr range)))
       ranges))

;; With all these helpers in place, we run the main program.
(define input-data (load-input-file "input.txt"))
(let-values (((ranges ids) (parse-input-data input-data)))
  (define fresh-ingredients (filter(lambda (id) (in-any-range? id ranges)) ids))
  (display (length fresh-ingredients)))
(newline)
