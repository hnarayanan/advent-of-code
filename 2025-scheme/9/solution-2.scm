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
  (lambda (tile-ref)
    (list-ref tiles tile-ref)))

(define (make-pair-area tiles)
  (lambda (tile-1-ref tile-2-ref)
    (let* ((tile (make-tile tiles))
           (a (area (tile tile-1-ref) (tile tile-2-ref))))
      (cons (cons tile-1-ref tile-2-ref) a))))

(define (get-pair pair-area)
  (list (car (car pair-area)) (cdr (car pair-area))))

(define (unordered-pairs l)
  (if (null? l)
      '()
      (append (map (lambda (x) (cons (car l) x))
                   (cdr l))
              (unordered-pairs (cdr l)))))

;; With all these helpers in place, we run the main program.
(let* ((tiles (load-input-file "example.txt"))
       (pair-area (make-pair-area tiles))
       (pair-refs (unordered-pairs (iota (length tiles))))
       (pair-areas (map (lambda (pair-ref)
                          (pair-area (car pair-ref) (cdr pair-ref)))
                        pair-refs))
       (sorted-pair-areas (sort pair-areas (lambda (x y) (> (cdr x) (cdr y)))))
       (largest-area (car (car sorted-pair-areas))))

  (display (car sorted-pair-areas))
  (newline)
  (display largest-area)
  (newline))
