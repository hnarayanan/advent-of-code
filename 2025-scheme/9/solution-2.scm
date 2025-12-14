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

(define (unordered-pairs lst)
  (if (null? lst)
      '()
      (append (map (lambda (x) (cons (car lst) x))
                   (cdr lst))
              (unordered-pairs (cdr lst)))))

;; Then we write some helpers to construct some representation of the
;; boundary given tile positions. Because tiles are ordered in a very
;; specific way, we load them in pairs (remembering to wrap around the
;; last value back to the first).

;; For each pair, depending on which coordinate is fixed, we use this
;; to identify if it's a horizontal or vertical boundary, and then
;; insert this section of the boundary to a data structure that holds
;; these alongside the fixed coordinate. This is a key idea here, and
;; the actual algorithm for this we copy over from a previous puzzle.

;; I know, this is a lot, and you can find visualisation of the notes
;; for this in the following post:
;; https://hachyderm.io/@harish/115697048993880425
(define (consecutive-pairs lst)
  (define first (car lst))
  (define (loop remaining)
    (if (null? (cdr remaining))
        (list (list (car remaining) first))
        (cons (list (car remaining) (cadr remaining))
              (loop (cdr remaining)))))
  (loop lst))

(define (classify-segment pair)
  (let* ((p1 (car pair))
         (p2 (cadr pair))
         (x1 (car p1)) (y1 (cadr p1))
         (x2 (car p2)) (y2 (cadr p2)))
    (if (= y1 y2)
        (list 'horz y1 (cons (min x1 x2) (max x1 x2)))
        (list 'vert x1 (cons (min y1 y2) (max y1 y2))))))

(define (split-segments segments)
  (list (filter (lambda (s) (eq? (car s) 'horz)) segments)
        (filter (lambda (s) (eq? (car s) 'vert)) segments)))

(define (insert-range new existing)
  (cond
   ((null? existing)
    (list new))
   ((< (cdr (car existing)) (car new))
    (cons (car existing) (insert-range new (cdr existing))))
   ((> (car (car existing)) (cdr new))
    (cons new existing))
   (else
    (insert-range
     (cons (min (car new) (car (car existing)))
           (max (cdr new) (cdr (car existing))))
     (cdr existing)))))

(define (add-segment-to-boundaries seg boundaries)
  (let* ((fixed-coord (cadr seg))
         (range (caddr seg))
         (existing (assoc fixed-coord boundaries)))
    (if existing
        (let ((merged (insert-range range (cdr existing))))
          (cons (cons fixed-coord merged)
                (filter (lambda (e) (not (= (car e) fixed-coord))) boundaries)))
        (cons (cons fixed-coord (list range)) boundaries))))

(define (group-segments segs)
  (sort (fold add-segment-to-boundaries '() segs)
        (lambda (a b) (< (car a) (car b)))))

(define (corner-valid? p)
  #t)

;; The following procedure comes in with two tile references
;; representing two corners of a rectangle. By definition, these two
;; are red tiles and fall on the boundary of the polygon made by the
;; red tiles. We don't need to check them. What we do instead is to
;; construct the remaining pair of opposite corners and check if
;; they're both valid.
(define (pair-area-valid? pair-area tiles)
  (let* ((tile (make-tile tiles))
         (tile-1-ref (car (car pair-area)))
         (tile-2-ref (cdr (car pair-area)))
         (tile-1 (tile tile-1-ref))
         (tile-2 (tile tile-2-ref))
         (opp-tile-1 (list (car tile-1) (cadr tile-2)))
         (opp-tile-2 (list (car tile-2) (cadr tile-1))))
    (and
     (corner-valid? opp-tile-1)
     (corner-valid? opp-tile-2))))


;; With all these helpers in place, we run the main program.
(let* ((tiles (load-input-file "example.txt"))
       (extracted-pairs (consecutive-pairs tiles))
       (segments (map classify-segment extracted-pairs))
       (split (split-segments segments))
       (h-segs (car split))
       (v-segs (cadr split))
       (h-boundaries (group-segments h-segs))
       (v-boundaries (group-segments v-segs))
       (pair-area (make-pair-area tiles))
       (pair-refs (unordered-pairs (iota (length tiles))))
       (pair-areas (map (lambda (pair-ref)
                          (pair-area (car pair-ref) (cdr pair-ref)))
                        pair-refs))
       (sorted-pair-areas (sort pair-areas (lambda (x y) (> (cdr x) (cdr y)))))
       ;; (largest-pair-area (car sorted-pair-areas))
       )

;;  (display h-boundaries)
  (newline)
;;  (display v-boundaries)
  (newline)
;;  (display sorted-pair-areas)
  (newline)
  (pair-area-valid? (car sorted-pair-areas) tiles)
  )
