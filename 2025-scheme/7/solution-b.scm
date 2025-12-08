#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-1))

;; We first load the input data in rows. Unlike our last attempt,
;; let's not evolve the grid this time, let's treat it as a fixed
;; manifold over which our beam propagates, creating a timeline every
;; time it splits.
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
               (reverse lines)
              (loop (cons (string->list line) lines)))))
      (loop '()))))

;; Let's create some helpers to track the position of the beam as it
;; flows through the grid, and check whether it's in bounds.
(define (make-pos row col)
  (cons row col))

(define (pos-row pos) (car pos))
(define (pos-col pos) (cdr pos))

(define (grid-height grid)
  (length grid))

(define (grid-width grid)
  (length (car grid)))

(define (in-bounds? grid pos)
  (let ((r (pos-row pos))
        (c (pos-col pos)))
    (and (>= r 0)
         (<  r (grid-height grid))
         (>= c 0)
         (<  c (grid-width grid)))))

;; Then we create some helpers to understand the what is in a cell of
;; the grid.
(define (cell-at grid pos)
  (let ((r (pos-row pos))
        (c (pos-col pos)))
    (list-ref (list-ref grid r) c)))

(define (start? c)
  (char=? c #\S))

(define (empty? c)
  (char=? c #\.))

(define (splitter? c)
  (char=? c #\^))

;; With some understanding of the grid, we can "propagate" our beam
;; through the grid. We do this in a recursive way that builds up a
;; tree of all possibilities.
(define (find-start grid)
  (let ((start-col (list-index start? (car grid))))
    (make-pos 0 start-col)))

(define (dead? grid pos)
  (not (in-bounds? grid pos)))

(define (at-splitter? grid pos)
  (and (in-bounds? grid pos)
       (splitter? (cell-at grid pos))))

(define (step-forward pos)
  (make-pos (+ 1 (pos-row pos))
            (pos-col pos)))

(define (go-left pos)
  (make-pos (pos-row pos)
            (- (pos-col pos) 1)))

(define (go-right pos)
  (make-pos (pos-row pos)
            (+ (pos-col pos) 1)))

(define (timeline-tree grid pos path)
  (define new-path (cons pos path))
  (cond ((dead? grid pos) (reverse new-path))
        ((at-splitter? grid pos)
         (cons (timeline-tree grid (go-left pos)  new-path)
               (timeline-tree grid (go-right pos) new-path)))
        (else
         (timeline-tree grid (step-forward pos) new-path))))

;; Once done, we can count up the valid timelines introduced by the
;; different possibilities.
(define (leaf? tree)
  (not (pair? tree)))

(define (count-leaves tree)
  (if (leaf? tree)
      1
      (+ (count-leaves (car tree))
         (count-leaves (cdr tree)))))

(define (count-tachyon-timelines grid)
  (let* ((start-pos (find-start grid))
         (tree      (timeline-tree grid start-pos '())))
    (count-leaves tree)))

;; With all these helpers in place, we run the main program.
(let* ((grid (load-input-file "example.txt"))
       (timelines (count-tachyon-timelines grid)))
  (display timelines)
  (newline))
