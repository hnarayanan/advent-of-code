#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-1))

;; Given an input file, return a list of ranges
(define (string->range s)
  (let ((parts (string-split s #\-)))
    (cons (string->number (car parts))
          (string->number (cadr parts)))))

(define (file->ranges path)
  (call-with-input-file path
    (lambda (port)
      (map string->range (string-split (read-line port) #\,)))))

;; Given the limits of a range of numbers, find all numbers in-between
(define (range->set range)
  (let ((start (car range))
        (end (cdr range)))
    (iota (1+ (- end start)) start)))

;; Setup some procedures to get the all integer factors of a number
(define (divisible? dividend divisor)
  (zero? (modulo dividend divisor)))

(define (factorise number)
  (filter (lambda (divisor) (divisible? number divisor))
          (iota (1- number) 1)))

;; Given a string, split it up into equally-sized pieces
(define (split-string string)
  (let* ((total-length (string-length string))
         (section-lengths (factorise total-length)))
    (map (lambda (section-length)
           (let ((starts (iota (/ total-length section-length) 0 section-length)))
             (map (lambda (start)
                    (substring string start (+ start section-length)))
                  starts)))
         section-lengths)))

;; Check if all given strings are equal
(define (all-equal? strings)
  (let ((first (car strings)))
    (define (check remaining)
      (cond
       ((null? remaining) #t)
       ((string=? first (car remaining))
        (check (cdr remaining)))
       (else #f)))
    (check (cdr strings))))

;; Check if any of the splits are equal
(define (any-equal-split? splits)
  (cond
   ((null? splits) #f)
   ((all-equal? (car splits)) #t)
   (else (any-equal-split? (cdr splits)))))

;; Check if the id is invalid
(define (invalid? id)
  (let ((id-parts (split-string (number->string id))))
    (any-equal-split? id-parts)))

;; Process the input file to find all invalid ids and total them
(define ranges (file->ranges "input.txt"))
(define sets (map range->set ranges))
(define invalid-ids (append-map (lambda (set) (filter invalid? set))
                                sets))
(define total (apply + invalid-ids))
(display total)
(newline)
