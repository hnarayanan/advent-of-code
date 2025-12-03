#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim))

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
(define char->digit
  (lambda (char)
    (- (char->integer char) (char->integer #\0))))

(define string->digits
  (lambda (string)
    (map char->digit (string->list string))))

;; Given a list of battery joltages in a bank, find the maximum two
;; digit number we can construct
(define find-max-two-digit-number
  (lambda (list-of-digits previous-max)
    (if (null? list-of-digits)
        previous-max
        (let ((first-digit (car list-of-digits))
              (other-digits (cdr list-of-digits)))
          (if (> first-digit previous-max)
              (find-max-two-digit-number other-digits first-digit)
              (find-max-two-digit-number other-digits previous-max))))))
;; TODO: Expand the following to find a two digit number, not just the
;; maximum singular digit

;; Fetch battery bank information from the input file and process them
(define battery-bank-strings (file->battery-banks "example.txt"))
(define battery-banks (map string->digits battery-bank-strings))
;; (display (map find-max-two-digit-number battery-banks))
;; TODO: Possibly need to rewrite the max function to just take a list
;; of integers, but encode within it the starting position of 0.
(newline)
