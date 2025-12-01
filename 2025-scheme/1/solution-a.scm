#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim))
(use-modules (ice-9 format))

;; Given an input file, return a list of turns
(define (file->turns path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((turns '()))
        (let ((turn (read-line port)))
          (if (eof-object? turn)
              (reverse turns)
              (loop (cons turn turns))))))))

;; Given a turn, try to find the delta we need to move the pointer
(define (get-sign turn)
  (cond ((char=? (string-ref turn 0) #\R) 1)
        ((char=? (string-ref turn 0) #\L) -1)))

(define (get-looped-magnitude turn)
  (modulo (string->number (substring turn 1 (string-length turn))) 100))

(define (turn->delta turn)
  (let ((sign (get-sign turn))
        (magnitude (get-looped-magnitude turn)))
    (* sign magnitude)))

;; Let's make a looped add function, that when:
;;   pointer > 99, replace it with pointer - 100,
;;   pointer < 0, replace it with 99 + pointer
(define (looped+ pointer delta)
  (let ((pointer (+ pointer delta)))
    (cond
     ((> pointer 99) (- pointer 100))
     ((< pointer 0) (+ pointer 100))
     (#t pointer))))

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
   (let ((delta (turn->delta turn)))
     (set! pointer (looped+ pointer delta))
     (when (zero? pointer)
       (set! counter (1+ counter)))
     (format #t "The dial is rotated ~a to point at ~a.~%" turn pointer)))
 turns)

;; Display the result
(format #t "The password is ~a.~%" counter)
