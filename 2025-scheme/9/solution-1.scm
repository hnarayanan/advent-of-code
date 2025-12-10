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

;; First we define some helper procedures around generating sets of
;; pairs of tiles and the areas they cover.
(define (area t1 t2)
  (let ((dx (1+ (- (car t1) (car t2))))
        (dy (1+ (- (cadr t1) (cadr t2)))))
    (* dx dy)))

(define (make-tile tiles)
  (lambda (i)
    (list-ref tiles i)))

(define (make-pair-area tiles)
  (lambda (i j)
    (let* ((tile (make-tile tiles))
           (a (area (tile i) (tile j))))
      (list a i j))))

(define (get-pair pair-area)
  (list (cadr pair-area) (caddr pair-area)))

(define (unordered-pairs l)
  (if (null? l)
      '()
      (append (map (lambda (x) (cons (car l) x))
                   (cdr l))
              (unordered-pairs (cdr l)))))

;; With all these helpers in place, we run the main program.
(let* ((tiles (load-input-file "input.txt"))
       (pair-area (make-pair-area tiles))
       (pair-refs (unordered-pairs (iota (length tiles))))
       (pair-areas (map (lambda (pair-ref)
                          (pair-area (car pair-ref) (cdr pair-ref)))
                        pair-refs))
       (sorted-pair-areas (sort pair-areas (lambda (x y) (> (car x) (car y)))))
       (largest-area (car (car sorted-pair-areas))))

  (display largest-area)
  (newline))
