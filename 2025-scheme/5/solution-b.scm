#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-11))

;; Given an input file, get a list ranges
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines)))))
      (loop '()))))

(define (classify s)
  (cond
   ((string-index s #\-) 'range)
   ((string-null? s) 'spacer)
   (else 'id)))

(define (parse-range s)
  (let ((parts (string-split s #\-)))
    (cons (string->number (car parts))
          (string->number (cadr parts)))))

(define (parse-input-data data)
  (define (loop lines ranges)
    (if (null? lines)
        (reverse ranges)
        (let* ((line (car lines))
               (type (classify line)))
          (cond
           ((eq? type 'range)
            (loop (cdr lines) (cons (parse-range line) ranges)))
           (else
            (loop (cdr lines) ranges))))))
  (loop data '()))

;; Given a list of ranges, merge them in a systematic way
(define (merge-into-ranges range ranges)
  (cond
   ;; There are no existing ranges, just merge the first one in
   ((null? ranges)
    (list range))
   ;; Current range entirely before new range - keep it, continue
   ((< (cdr (car ranges)) (car range))
    (cons (car ranges) (merge-into-ranges range (cdr ranges))))
   ;; Current range entirely after new range - insert here, done
   ((> (car (car ranges)) (cdr range))
    (cons range ranges))
   ;; Overlap - merge into bigger range, continue with that
   (else
    (merge-into-ranges
     (cons (min (car range) (car (car ranges)))
           (max (cdr range) (cdr (car ranges))))
     (cdr ranges)))))

(define (merged-ranges input-ranges)
  (fold merge-into-ranges '() input-ranges))

(define (range-size range)
  (1+ (- (cdr range) (car range))))

;; With all these helpers in place, we run the main program.
(define input-ranges (parse-input-data (load-input-file "input.txt")))
(define total-ids (apply + (map range-size (merged-ranges input-ranges))))
(display total-ids)
(newline)
