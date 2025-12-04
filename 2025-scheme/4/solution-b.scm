#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-1))


;; Given a list of lines containing paper roll positions, construct a
;; full grid of positions and states that we can work with
(define (load-initial-state path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
              (map string->list (reverse lines))
              (loop (cons line lines)))))
      (loop '()))))

;; Write some helper procedures to fetch the state at a given grid
;; position and determine if they can be accessed by a forklift
(define (paper-roll? character)
  (cond ((char=? character #\.) #f)
        ((char=? character #\@) #t)))

(define (can-forklift-access? character)
  (cond ((char=? character #\.) #f)
        ((char=? character #\@) #f)
        ((char=? character #\x) #t)))

(define (paper-roll-in-pos? grid row col)
  (if (and (>= row 0)
           (>= col 0)
           (< row (length grid))
           (< col (length (car grid))))
      (paper-roll? (list-ref (list-ref grid row) col))
      #f))

(define (neighbourhood row col)
  (let ((N  (cons (1- row)     col))
        (NE (cons (1- row) (1+ col)))
        (E  (cons     row  (1+ col)))
        (SE (cons (1+ row) (1+ col)))
        (S  (cons (1+ row)     col))
        (SW (cons (1+ row) (1- col)))
        (W  (cons     row  (1- col)))
        (NW (cons (1- row) (1- col))))
    (list N NE E SE S SW W NW)))

(define (count-true . bools)
  (apply + (map (lambda (b) (if b 1 0)) bools)))

(define (count-accessible . cells)
  (apply count-true (map can-forklift-access? cells)))

(define min-neighbours-blocking 4)
(define (can-forklift-access-in-pos? grid row col)
  (let ((neighbours (map (lambda (pos)
                           (paper-roll-in-pos? grid (car pos) (cdr pos)))
                         (neighbourhood row col))))
    (< (apply count-true neighbours) min-neighbours-blocking)))

;; Finally, we write a procedures that work on the entire grid,
;; starting with one that marks all the paper rolls that a forklift
;; can access
(define (mark-accessible-paper-rolls grid)
  (map (lambda (row row-idx)
         (map (lambda (cell col-idx)
                (if (and (paper-roll? cell)
                         (can-forklift-access-in-pos? grid row-idx col-idx))
                    #\x
                    cell))
              row
              (iota (length row))))
       grid
       (iota (length grid))))

;; Count all the accessible paper rolls on the grid
(define (count-accessible-paper-rolls grid)
  (apply count-accessible (apply append grid)))

;; Remove the accessible paper rolls on the grid
(define (remove-accessible-paper-rolls grid)
  (map (lambda (row)
         (map (lambda (cell)
                (if (can-forklift-access? cell)
                    #\.
                    cell))
              row))
       grid))

;; Fetch the locations of the rolls of paper from the input file and
;; process them
(define initial-state (load-initial-state "example.txt"))
(define initial-accessible (mark-accessible-paper-rolls initial-state))
(define initial-accessible-count (count-accessible-paper-rolls initial-accessible))
(define next-state (remove-accessible-paper-rolls initial-accessible))

(display next-state)
(newline)
