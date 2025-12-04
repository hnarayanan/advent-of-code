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
  (cond ((char=? character #\.) 0)
        ((char=? character #\@) 1)))

(define (lines->grid lines)
  (define (loop remaining grid-with-paper-state)
    (if (null? remaining)
        (reverse grid-with-paper-state)
        (loop (cdr remaining)
              (cons
               (map mark-position (string->list (car remaining)))
               grid-with-paper-state))))
  (loop lines '()))

;; Write some helper procedures to fetch the state at a given grid
;; position and determine if they can be accessed by a forklift
(define (paper-roll-in-pos? grid row col)
  (if (and (>= row 0)
           (>= col 0)
           (< row (length grid))
           (< col (length (car grid))))
      (list-ref (list-ref grid row) col)
      0))

(define (can-forklift-access-in-pos? grid row col)
  (let* ((N  (cons (1- row)     col))
         (NE (cons (1- row) (1+ col)))
         (E  (cons     row  (1+ col)))
         (SE (cons (1+ row) (1+ col)))
         (S  (cons (1+ row)     col))
         (SW (cons (1+ row) (1- col)))
         (W  (cons     row  (1- col)))
         (NW (cons (1- row) (1- col)))
         (neighbourhood (list N NE E SE S SW W NW))
         (neighbours (map (lambda (pos)
                            (paper-roll-in-pos? grid (car pos) (cdr pos)))
                          neighbourhood)))
    (if (< (apply + neighbours) 4)
        1
        0)))

;; Finally, we write a procedure that finds all the paper rolls that a
;; forklift can access on the entire grid
(define (all-paper-rolls-forklifts-can-access grid)
  (map (lambda (row row-idx)
         (map (lambda (cell col-idx)
                (if (and (= 1 (paper-roll-in-pos? grid row-idx col-idx))
                         (= 1 (can-forklift-access-in-pos? grid row-idx col-idx)))
                    1
                    0))
              row
              (iota (length row))))
       grid
       (iota (length grid))))


;; Fetch the locations of the rolls of paper from the input file and
;; process them
(define paper-rolls-lines (file->lines "input.txt"))
(define paper-rolls-grid (lines->grid paper-rolls-lines))
(define accessible-paper-rolls (all-paper-rolls-forklifts-can-access paper-rolls-grid))
(define number-of-accesible-paper-rolls (apply + (apply append accessible-paper-rolls)))
(display number-of-accesible-paper-rolls)
(newline)
