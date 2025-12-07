#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim))

;; We first load the input data in rows.
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
               (reverse lines)
              (loop (cons (string->list line) lines)))))
      (loop '()))))

;; Then we create some helper procedures to make sense of it.
(define (beamy? c)
  (cond ((char=? c #\S) #t)
        ((char=? c #\|) #t)
        (else #f)))

(define (empty? c)
  (char=? c #\.))

(define (splitter? c)
  (char=? c #\^))

;; Up to this puzzle, we have managed to get away with pure recursive
;; loops with no mutation. Now we've reached a place where that
;; approach clearly fights what we need to do:
;;
;; For each row, we need to look at the previous row and wherever
;; there is an S or |, we want to propagate | to the next row. If the
;; position was previously empty in the next row, this is easy, we
;; just copy over a |. But if it's not, as in it's a ^, we need to
;; modify the positions before and after to both be |.

;; There are ways to solve this (multiple passes, where we first
;; lookup what needs to be done and then do it) without mutation, but
;; the approach we follow below is easier. It turns out that while
;; lists are good for walking left to right, they are not good for
;; arbitrary lookups and changes. This then means we convert them to
;; vectors, update them as needed, and then send them back as lists.

copy | over to our row If it's a .
;; in our row, we just copy over | in that position If it's a ^ in our
;; row, we copy over | to before and after that position
(define (propagate-one-step prev current)
  (define (loop remaining-prev remaining-current next)
    (if (or (null? remaining-prev)
            (null? remaining-current))
        (reverse next)
        (loop (cdr remaining-prev)
              (cdr remaining-current)
              (cons (update-position (car remaining-prev) (car remaining-current)) next))))
  (loop prev current '()))

(define (propagate-grid manifold)
  (let loop ((prev-row (car manifold))
             (rows (cdr manifold))
             (grid (list (car manifold))))
    (if (null? rows)
        (reverse grid)
        (let* ((current-row (car rows))
               (updated-row (propagate-one-step prev-row current-row)))
          (display updated-row)
          (newline)
          (loop current-row
                (cdr rows)
                (cons updated-row grid))))))

;; (define (count-splits manifold)
;;   (define (loop remaining updated count)
;;     (if (= (length remaining) 1)
;;         count
;;         (loop (cdr remaining) updated (1+ count))))
;;   (loop manifold '() 0))

;; With all these helpers in place, we run the main program.
(let* ((manifold (load-input-file "example.txt")))
  (propagate-grid manifold)
;;  (display (count-splits manifold))
  (newline))
