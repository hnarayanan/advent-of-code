#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (ice-9 format))

;; Given an input file, return a list of turns
(define (file->turns path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((turns '()))
        (let ((turn (read-line port)))
          (if (eof-object? turn)
              (reverse turns)
              (loop (cons turn turns))))))))

;; Given a turn, try to find the delta we need to move the pointer. As
;; we do this, make a note of any overall counter changes.
(define (get-sign turn)
  (cond ((char=? (string-ref turn 0) #\R) 1)
        ((char=? (string-ref turn 0) #\L) -1)))

(define (get-looped-magnitude turn)
  (let ((magnitude (string->number (substring turn 1 (string-length turn)))))
    (cons (modulo magnitude 100)
          (quotient magnitude 100))))

(define (turn->delta turn)
  (let* ((sign (get-sign turn))
         (mag-result (get-looped-magnitude turn))
         (magnitude (car mag-result))
         (count-inc (cdr mag-result)))
    (cons (* sign magnitude)
          count-inc)))

;; Let's make a looped add function, that when:
;;   pointer > 99, replace it with pointer - 100,
;;   pointer < 0, replace it with 99 + pointer
;; In both these cases, since we crossed the 0 mark, we increment
;; the counter
(define (looped+ pointer delta)
  (let ((new-pointer (+ pointer delta)))
    (cond
     ((> new-pointer 99)
      (unless (or (zero? pointer) (zero? (- new-pointer 100)))
        (set! counter (1+ counter)))
      (display "During this rotation it crosses 0 once -- ")
      (- new-pointer 100))
     ((< new-pointer 0)
      (unless (or (zero? pointer) (zero? (+ new-pointer 100)))
        (set! counter (1+ counter)))
      (display "During this rotation it crosses 0 once -- ")
      (+ new-pointer 100))
     (else
      new-pointer))))

;; Fetch turns from the file to loop over
(define turns (file->turns "input.txt"))

;; Start a pointer at 50, counter at 0
(define pointer 50)
(define counter 0)
(format #t "The dial starts by pointing at ~a~%" pointer)

;; For each turn, find the new pointer position
;; If it's at 0, increment the counter
(for-each
 (lambda (turn)
   (let* ((delta-result (turn->delta turn))
          (delta (car delta-result))
          (count-inc (cdr delta-result)))
     (set! counter (+ counter count-inc))
     (set! pointer (looped+ pointer delta))
     (when (zero? pointer)
       (display "The pointer is 0.")
       (newline)
       (set! counter (1+ counter)))
     (format #t "The dial is rotated ~a to point at ~a.~%" turn pointer)))
 turns)

;; Display the result
(format #t "The password is ~a.~%" counter)
