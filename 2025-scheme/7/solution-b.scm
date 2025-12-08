#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 hash-table)
             (ice-9 rdelim)
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
;; through the grid.
(define (find-start grid)
  (let ((start-col (list-index start? (car grid))))
    (make-pos 0 start-col)))

(define (dead? grid pos)
  (not (in-bounds? grid pos)))

(define (at-splitter? grid pos)
  (and (in-bounds? grid pos)
       (splitter? (cell-at grid pos))))

(define (step-forward pos)
  (make-pos (1+ (pos-row pos))
            (pos-col pos)))

(define (go-left pos)
  (make-pos (pos-row pos)
            (- (pos-col pos) 1)))

(define (go-right pos)
  (make-pos (pos-row pos)
            (+ (pos-col pos) 1)))

;; We do this in a recursive way that can count up the valid timelines
;; introduced by the different possibilities. It turns out (and I know
;; this the hard way) doing this in a naively recursive fashion is
;; exceedingly expensive and will not work for the input data. So we
;; "memoise" it by following the example of SICP Exercise 3.27 in
;; https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-22.html#%_sec_3.3.3
(define (memoize f)
  (let ((table (make-hash-table)))
    (lambda (x)
      (let ((hit (hash-ref table x #f)))
        (if hit
            hit
            (let ((result (f x)))
              (hash-set! table x result)
              result))))))

(define (make-count-timelines grid)
  (letrec ((count
            (memoize
             (lambda (pos)
               (cond
                 ((dead? grid pos)
                  1)
                 ((at-splitter? grid pos)
                  (+ (count (go-left pos))
                     (count (go-right pos))))
                 (else
                  (count (step-forward pos))))))))
    count))


;; With all these helpers in place, we run the main program.
(let* ((grid (load-input-file "input.txt"))
       (start-pos (find-start grid))
       (count-from (make-count-timelines grid))
       (timelines (count-from start-pos)))
  (display timelines)
  (newline))
