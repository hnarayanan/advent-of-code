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
(define (mark-position pos character)
  (cond ((char=? character #\.) (cons pos 'empty))
        ((char=? character #\@) (cons pos 'paper-roll))))

(define (make-grid-row row cols)
  (map (lambda (col) (cons row col)) (iota cols)))

(define (lines->grid lines)
  (define (loop remaining grid-with-paper-state row)
    (if (null? remaining)
        (reverse grid-with-paper-state)
        (let* ((row-list (string->list (car remaining)))
               (row-length (length row-list)))
          (loop (cdr remaining)
                (cons
                 (map mark-position (make-grid-row row row-length) row-list)
                 grid-with-paper-state)
                (1+ row)))))
  (loop lines '() 0))


;; Fetch the locations of the rolls of paper from the input file and
;; process them
(define paper-rolls-lines (file->lines "example.txt"))
(define paper-rolls-grid (lines->grid paper-rolls-lines))
(display paper-rolls-grid)
