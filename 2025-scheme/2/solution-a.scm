#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (ice-9 format)
             (srfi srfi-1))

;; Given an input file, return a list of ranges
(define (string->range s)
  (let ((parts (string-split s #\-)))
    (cons (string->number (car parts))
          (string->number (cadr parts)))))

(define (file->ranges path)
  (call-with-input-file path
    (lambda (port)
      (let ((string-ranges (string-split (read-line port) #\,)))
        (define (process-string-ranges string-ranges numeric-ranges)
          (if (null? string-ranges)
              numeric-ranges
              (let* ((current (car string-ranges))
                     (pair (string->range current)))
                (process-string-ranges (cdr string-ranges)
                                       (cons pair numeric-ranges)))))
        (process-string-ranges string-ranges '())))))

;; Given the limits of a range of numbers, find all numbers in-between
(define (range->set range)
  (let ((start (car range))
        (end (cdr range)))
    (iota (1+ (- end start)) start 1)))

;; Identify whether an id is invalid
(define (invalid? id)
  (let* ((id-string (number->string id))
         (id-length (string-length id-string)))
    (cond
     ((odd? id-length) #f)
     ((even? id-length)
      (let ((first-half (substring id-string 0 (/ id-length 2)))
            (second-half (substring id-string (/ id-length 2) id-length)))
        (string=? first-half second-half))))))

;; Process the input file to find all invalid ids and total them
(define ranges (file->ranges "example.txt"))
(define sets (map (lambda (range)
                    (range->set range))
                  ranges))
(define invalid-ids (append-map (lambda (set)
                                  (filter invalid? set))
                                sets))
(define total (apply + invalid-ids))
(display total)
