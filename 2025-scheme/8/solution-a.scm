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
      (list d i j))))

(define (pairs l)
  (if (null? l)
      '()
      (append (map (lambda (x) (cons (car l) x))
                   (cdr l))
              (pairs (cdr l)))))

;; With all these helpers in place, we run the main program.
(let* ((points (load-input-file "example.txt"))
       (pair-distance (make-pair-distance points))
       (pair-refs (pairs (iota (length points))))
       (all-pair-distances (map (lambda (pair-ref) (pair-distance (car pair-ref) (cdr pair-ref))) pair-refs)))

  (display (all-pair-distances))
  (newline)
  (display (length pair-refs))
  (newline))
