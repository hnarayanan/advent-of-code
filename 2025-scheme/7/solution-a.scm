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
(define (propagate-one-step prev-row curr-row)
  (let* ((len (length curr-row))
         (prev (list->vector prev-row))
         (curr (list->vector curr-row))
         (new (vector-copy curr))
         (count 0))

    (define (loop i)
      (if (= i len)
          (cons count (vector->list new))
          (let* ((p (vector-ref prev i))
                 (c (vector-ref curr i)))
            (when (beamy? p)
              (cond
               ((empty? c)
                (vector-set! new i #\|))
               ((splitter? c)
                (set! count (1+ count))
                (when (> i 0)
                  (vector-set! new (1- i) #\|))
                (when (< (1+ i) len)
                  (vector-set! new (1+ i) #\|)))
               (else
                #f)))
            (loop (1+ i)))))
    (loop 0)))

(define (propagate-grid manifold)
  (define (loop prev-row rows grid count)
    (if (null? rows)
        count
        (let* ((current-row (car rows))
               (new-row-count (propagate-one-step prev-row current-row))
               (new-row (cdr new-row-count))
               (row-hit-count (car new-row-count)))
          (loop new-row
                (cdr rows)
                (cons new-row grid)
                (+ count row-hit-count)))))
  (loop (car manifold)
        (cdr manifold)
        (list (car manifold))
        0))

;; With all these helpers in place, we run the main program.
(let* ((manifold (load-input-file "example.txt")))
  (display (propagate-grid manifold))
  (newline))
