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
(define (in-which-circuit-idx circuits p)
  (list-index (lambda (circuit) (member p circuit)) circuits))

(define (neither-in-any-circuits? circuits pair)
  (let ((p1 (car pair))
        (p2 (cadr pair)))
    (and (not (in-which-circuit-idx circuits p1))
         (not (in-which-circuit-idx circuits p2)))))

(define (both-in-same-circuit? circuits pair)
  (let* ((p1 (car pair))
         (p2 (cadr pair))
         (idx1 (in-which-circuit-idx circuits p1))
         (idx2 (in-which-circuit-idx circuits p2)))
    (and idx1 idx2 (equal? idx1 idx2))))

(define (only-one-in-an-existing-circuit? circuits pair)
  (let* ((p1 (car pair))
         (p2 (cadr pair))
         (idx1 (in-which-circuit-idx circuits p1))
         (idx2 (in-which-circuit-idx circuits p2)))
    (or (and idx1 (not idx2))
        (and idx2 (not idx1)))))

(define (extend-circuit-with-point circuits pair)
  (let* ((p1 (car pair))
         (p2 (cadr pair))
         (idx1 (in-which-circuit-idx circuits p1))
         (idx2 (in-which-circuit-idx circuits p2))
         (idx (or idx1 idx2))
         (new-point (if idx1 p2 p1))
         (old-circuit (list-ref circuits idx))
         (new-circuit (cons new-point old-circuit)))
    (append (take circuits idx)
            (list new-circuit)
            (drop circuits (1+ idx)))))

;;TODO:  both-in-different-circuits

(define (add-pair-to-circuits circuits pair)
  (let ((p1 (car pair))
        (p2 (cadr pair)))
    (cond ((null? circuits)
           (display "Empty circuits list")
           (newline)
           (list pair))
          ((neither-in-any-circuits? circuits pair)
           (display "Neither in any circuits")
           (newline)
           (append (list pair) circuits))
          ((both-in-same-circuit? circuits pair)
           (display "Both in same circuits")
           (newline)
           circuits)
          ((only-one-in-an-existing-circuit? circuits pair)
           (display "Only one in an existing circuit")
           (newline)
           (extend-circuit-with-point circuits pair))
          (else
           (display "Un-handled case")
           (newline)
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

  (display circuits)
  (newline))
