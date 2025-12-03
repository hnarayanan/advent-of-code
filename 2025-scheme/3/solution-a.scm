#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (ice-9 format))

;; Given an input file, return a string representation of battery
;; banks
(define (file->battery-banks path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((battery-banks '()))
        (let ((battery-bank (read-line port)))
          (if (eof-object? battery-bank)
              (reverse battery-banks)
              (loop (cons battery-bank battery-banks))))))))

;; Write some helpers to convert the strings to list of numbers
;; representing the battery banks
(define (char->digit char)
  (- (char->integer char) (char->integer #\0)))

(define (string->digits string)
  (map char->digit (string->list string)))

;; Given a list of digits, find the first largest one and its position
(define (find-max-digit-and-position list-of-digits)
  (define (loop digits pos max argmax)
    (format #t "Digits: ~a Max: ~a Argmax: ~a.~%" digits max argmax)
    (if (null? digits)
        (cons max argmax)
        (let ((first-digit (car digits))
              (other-digits (cdr digits)))
          (if (> first-digit max)
              (loop other-digits (1+ pos) first-digit pos)
              (loop other-digits (1+ pos) max argmax)))))
  (loop list-of-digits 0 0 0))

;; TODO: Find the second largest digit given a list of digits


;; Fetch battery bank information from the input file and process them
(define battery-bank-strings (file->battery-banks "example.txt"))
(define battery-banks (map string->digits battery-bank-strings))
(define max-joltages (map find-max-digit-and-position battery-banks))
(display max-joltages)
(newline)
