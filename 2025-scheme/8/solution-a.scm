#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim)
             (srfi srfi-1))

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

;; First we define some helper procedures around generating sets of
;; pairs of points and the distances between them.
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

(define (get-pair pair-distance)
  (list (cadr pair-distance) (caddr pair-distance)))

(define (unordered-pairs l)
  (if (null? l)
      '()
      (append (map (lambda (x) (cons (car l) x))
                   (cdr l))
              (unordered-pairs (cdr l)))))

;; Now that we have a well-defined set of pairs of points to consider,
;; we define some helper procedures to put them into circuits.
(define (in-which-circuits? circuits p)
  '()) ;; TODO: Actually implement

(define (neither-in-circuits? circuits pair)
  (let ((p1 (car pair))
        (p2 (cadr pair)))
    (and (null? (in-which-circuits? circuits p1))
         (null? (in-which-circuits? circuits p2)))))

(define (both-in-same-circuit? circuits pair)
  (let ((p1 (car pair))
        (p2 (cadr pair)))
    (equal? (in-which-circuits? circuits p1)
            (in-which-circuits? circuits p2))))

(define (add-pair-to-circuits circuits pair)
  (let ((p1 (car pair))
        (p2 (cadr pair)))
    (cond ((null? circuits)
           (list pair))
          ((neither-in-circuits? circuits pair)
           (append (list pair) circuits))
          ((both-in-same-circuit? circuits pair)
           circuits))))

(define (create-circuits pairs)
  (define (loop remaining circuits)
    (if (null? remaining)
        (reverse circuits)
        (loop (cdr remaining) (add-pair-to-circuits circuits (car remaining)))))
  (loop pairs '()))

;; With all these helpers in place, we run the main program.
(let* ((points (load-input-file "example.txt"))
       (pair-distance (make-pair-distance points))
       (pair-refs (unordered-pairs (iota (length points))))
       (pair-distances (map (lambda (pair-ref)
                              (pair-distance (car pair-ref) (cdr pair-ref)))
                            pair-refs))
       (sorted-pair-distances (sort pair-distances (lambda (x y) (< (car x) (car y)))))
       (connections 10)
       (pairs (map get-pair (take sorted-pair-distances connections)))
       (circuits (create-circuits pairs)))

  (display pairs)
  (newline))
