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
           ((eq? type 'id)
            (loop (cdr lines) ranges))
           ((eq? type 'spacer)
            (loop (cdr lines) ranges))))))
  (loop data '()))

;; We now have enough information to check whether a given ingredient
;; ID belongs to any of the fresh ingredient ID ranges.
(define (in-range? n low high)
  (<= low n high))

(define (in-any-range? n ranges)
  (any (lambda (range) (in-range? n (car range) (cdr range)))
       ranges))

;; And with this we can be smarter about how we construct the list of
;; ranges to begin with.
(define (insert-sorted range ranges)
  (cond
   ((null? ranges) (list range))
   ((< (car range) (car (car ranges)))
    (cons range ranges))
   (else
    (cons (car ranges)
          (insert-sorted range (cdr ranges))))))

(define (increase-range-upper-bound range ranges)
  ;; TODO
  )

(define (decrease-range-lower-bound range ranges)
  ;; TODO
  )

(define (merge-into-ranges range ranges)
  (cond
   ;; Case 0: No existing ranges
   ((null? ranges)
    (list range))
   ;; Case 1: Neither bound overlaps any existing ranges, just insert
   ;; the new range in the right place
   ((and (not (in-any-range? (car range) ranges))
         (not (in-any-range? (cdr range) ranges)))
    (insert-sorted range ranges))
   ;; Case 2: Only lower bound overlaps, then we find that range and
   ;; increase the upper bound
   ((and (in-any-range? (car range) ranges)
         (not (in-any-range? (cdr range) ranges)))
    (increase-range-upper-bound range ranges))
   ;; Case 3: Only upper bound overlaps, then we find that range and
   ;; decrease the lower bound
   ((and (not (in-any-range? (car range) ranges))
         (in-any-range? (cdr range) ranges))
    (decrease-range-lower-bound range ranges))

   ;; Case 4: Both overlap — ???
   ))

(define (merged-ranges input-ranges)
  (define (loop remaining ranges)
    (if (null? (cdr remaining))
        ranges
        (loop (cdr remaining) (merge-into-ranges (car remaining) ranges))))
  (loop input-ranges '()))

;; With all these helpers in place, we run the main program.
(define input-ranges (parse-input-data (load-input-file "example.txt")))
(display (merged-ranges input-ranges))
(newline)
