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

;; Identify whether an id is invalid
(define (invalid? id)
  (let* ((id-string (number->string id))
         (id-length (string-length id-string))
         (half (/ id-length 2)))
    (cond
     ((odd? id-length) #f)
     (else
      (string=? (substring id-string 0 half)
                (substring id-string half))))))

;; Process the input file to find all invalid ids and total them
(define ranges (file->ranges "input.txt"))
(define sets (map range->set ranges))
(define invalid-ids (append-map (lambda (set) (filter invalid? set))
                                sets))
(define total (apply + invalid-ids))
(display total)
(newline)
