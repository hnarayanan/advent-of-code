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
      (let ((crosses (if (or (zero? pointer) (zero? (- new-pointer 100))) 0 1)))
        (cons (- new-pointer 100) crosses)))
     ((< new-pointer 0)
      (let ((crosses (if (or (zero? pointer) (zero? (+ new-pointer 100))) 0 1)))
        (cons (+ new-pointer 100) crosses)))
     (else
      (cons new-pointer 0)))))

;; Introduce a recursive procedure to loop over the turns and update
;; the pointer and counter
(define (process-turns turns pointer counter)
  (if (null? turns)
      counter
      (let* ((turn (car turns))
             (delta-result (turn->delta turn))
             (delta (car delta-result))
             (count-inc (cdr delta-result))
             (loop-result (looped+ pointer delta))
             (new-pointer (car loop-result))
             (cross-inc (cdr loop-result))
             (new-counter (+ counter count-inc cross-inc))
             (final-counter (if (zero? new-pointer)
                                (begin
                                  (1+ new-counter))
                                new-counter)))
        (format #t "The dial is rotated ~a to point at ~a.~%" turn new-pointer)
        (process-turns (cdr turns) new-pointer final-counter))))

;; Fetch turns from the file to process
(define turns (file->turns "input.txt"))

;; Start the pointer at 50 and process the turns
(define password (process-turns turns 50 0))
(format #t "The new password would be ~a.~%" password)
