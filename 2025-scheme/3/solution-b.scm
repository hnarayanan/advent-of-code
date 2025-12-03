#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-1))

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

;; Given a list of digits, find the largest two-digit number we can
;; construct
(define (find-max-two-digit-number digits)

  (define (find-max-nth-digit-and-position digits n pos max argmax)
    (if (= (length digits) (1- n))
        (cons max argmax)
        (let ((first-digit (car digits))
              (other-digits (cdr digits)))
          (if (> first-digit max)
              (find-max-nth-digit-and-position other-digits n (1+ pos) first-digit pos)
              (find-max-nth-digit-and-position other-digits n (1+ pos) max argmax)))))

  (define max-first-digit-and-pos (find-max-nth-digit-and-position digits 2 0 0 0))
  (define max-first-digit (car max-first-digit-and-pos))
  (define max-first-pos (cdr max-first-digit-and-pos))
  (define max-second-digit-and-pos
    (find-max-nth-digit-and-position (drop digits (1+ max-first-pos)) 1 0 0 0))
  (define max-second-digit (car max-second-digit-and-pos))
  (define max-second-pos (cdr max-second-digit-and-pos))

  (+ (* 10 max-first-digit) max-second-digit))

;; Fetch battery bank information from the input file and process them
(define battery-bank-strings (file->battery-banks "input.txt"))
(define battery-banks (map string->digits battery-bank-strings))
(define max-joltages (map find-max-two-digit-number battery-banks))
(display (apply + max-joltages))
(newline)
