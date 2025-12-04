#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-1))

;; Given an input file, return a list of lines of its contents
(define (file->lines path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines)))))
      (loop '()))))

;; Given a list of lines containing paper roll positions, construct a
;; full grid of positions and states that we can work with
(define (mark-position character)
  (cond ((char=? character #\.) 'empty)
        ((char=? character #\@) 'paper-roll)))

(define (lines->grid lines)
  (define (loop remaining grid-with-paper-state)
    (if (null? remaining)
        (reverse grid-with-paper-state)
        (loop (cdr remaining)
              (cons
               (map mark-position (string->list (car remaining)))
               grid-with-paper-state))))
  (loop lines '()))


;; Fetch the locations of the rolls of paper from the input file and
;; process them
(define paper-rolls-lines (file->lines "example.txt"))
(define paper-rolls-grid (lines->grid paper-rolls-lines))
(display paper-rolls-grid)
