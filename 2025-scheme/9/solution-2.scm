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

(define (unordered-pairs lst)
  (if (null? lst)
      '()
      (append (map (lambda (x) (cons (car lst) x))
                   (cdr lst))
              (unordered-pairs (cdr lst)))))

;; Then we write some helpers to construct some representation of the
;; boundary given tile positions.
;; Tiles are ordered in a specific way
;; We load them in twos
(define (consecutive-pairs lst)
  (define first (car lst))
  (define (loop remaining)
    (if (null? (cdr remaining))
        (list (list (car remaining) first))
        (cons (list (car remaining) (cadr remaining))
              (loop (cdr remaining)))))
  (loop lst))
;; For each pair, depending on which coordinate is the same
;; We use the same coordinate as the key on the correct data structure
(define (classify-segment pair)
  (let* ((p1 (car pair))
         (p2 (cadr pair))
         (x1 (car p1)) (y1 (cadr p1))
         (x2 (car p2)) (y2 (cadr p2)))
    (if (= y1 y2)
        (list 'h y1 (cons (min x1 x2) (max x1 x2)))
        (list 'v x1 (cons (min y1 y2) (max y1 y2))))))
;; And add the other pair to a related boundary in the other direction
;; This uses the merging algorithm from a previous puzzle


;; With all these helpers in place, we run the main program.
(let* ((tiles (load-input-file "example.txt"))
       (extracted-pairs (consecutive-pairs tiles))
       ;; (tile (make-tile tiles))
       ;; (pair-area (make-pair-area tiles))
       ;; (pair-refs (unordered-pairs (iota (length tiles))))
       ;; (pair-areas (map (lambda (pair-ref)
       ;;                    (pair-area (car pair-ref) (cdr pair-ref)))
       ;;                  pair-refs))
       ;; (sorted-pair-areas (sort pair-areas (lambda (x y) (> (cdr x) (cdr y)))))
       ;; (largest-pair-area (car sorted-pair-areas))
       )

  (display tiles)
  (newline)
  (display extracted-pairs)
  (newline)
  (display (map classify-segment extracted-pairs))
  (newline))
