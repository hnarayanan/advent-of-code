#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim))

;; Load input file.
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
               (reverse lines)
               (loop (cons (map string->number (string-split line #\,)) lines)))))
      (loop '()))))

;; Define some helper procedures.
(define (square x)
  (* x x))

(define (distance p1 p2)
  (let ((dx (- (car p1) (car p2)))
        (dy (- (cadr p1) (cadr p2)))
        (dz (- (caddr p1) (caddr p2))))
    (sqrt (+ (square dx) (square dy) (square dz)))))

(define (make-point points)
  (lambda (i)
    (list-ref points i)))

(define (make-pair-distance points)
  (lambda (i j)
    (let* ((point (make-point points))
           (d (distance (point i) (point j))))
      (list i j d))))

;; With all these helpers in place, we run the main program.
(let* ((points (load-input-file "example.txt"))
       (point (make-point points))
       (pair-distance (make-pair-distance points)))
  (display points)
  (newline)
  (display (point 0))
  (newline)
  (display (pair-distance 0 1))
  (newline))
