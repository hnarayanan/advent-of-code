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

;; Find the largest start digit (and its position) of an n-digit
;; number given a list of digits
(define (find-max-digit-and-pos digits n)
  (define (loop remaining pos max argmax)
    (if (= (length remaining) (1- n))
        (cons max argmax)
        (let ((first (car remaining))
              (rest (cdr remaining)))
          (if (> first max)
              (loop rest (1+ pos) first pos)
              (loop rest (1+ pos) max argmax)))))
  (loop digits 0 0 0))

;; Given a list of digits, construct the largest n-digit number that we can
(define (find-max-n-digit-number digits n)
  (define (loop remaining found-digits m)
    (if (zero? m)
        found-digits
        (let* ((result (find-max-digit-and-pos remaining m))
               (digit (car result))
               (pos (cdr result)))
          (loop (drop remaining (1+ pos))
                (cons digit found-digits)
                (1- m)))))
  (loop digits '() n))

;; ;; Fetch battery bank information from the input file and process them
;; (define battery-bank-strings (file->battery-banks "input.txt"))
;; (define battery-banks (map string->digits battery-bank-strings))
;; (define max-joltages (map find-max-n-digit-number battery-banks))
;; (display (apply + max-joltages))
;; (newline)



;;   ;; (define max-second-pos (cdr max-second-digit-and-pos))

;;   ;; (+ (* 10 max-first) max-second-digit))
