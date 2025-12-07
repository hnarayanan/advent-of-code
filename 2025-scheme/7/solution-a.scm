#!/usr/bin/env -S guile -s
!#

(use-modules (ice-9 rdelim))

;; We first load the input data in rows.
(define (load-input-file path)
  (call-with-input-file path
    (lambda (port)
      (define (loop lines)
        (let ((line (read-line port)))
          (if (eof-object? line)
               (reverse lines)
              (loop (cons (string->list line) lines)))))
      (loop '()))))

;; Then we create some helper procedures to make sense of it.
(define (beamy? c)
  (cond ((char=? c #\S) #t)
        ((char=? c #\|) #t)
        (else #f)))

(define (empty? c)
  (char=? c #\.))

(define (splitter? c)
  (char=? c #\^))


;; For each row, we look at the previous row
;; Wherever these is an S or |, we want to copy | over to our row
;; If it's a . in our row, we just copy over | in that position
;; If it's a ^ in our row, we copy over | to before and after that position
(define (update-position prev curr)
  (if (beamy? prev)
      (if (empty? curr)
          #\|
          #\?)
      curr))

(define (propagate-one-step prev current)
  (define (loop remaining-prev remaining-current next)
    (if (or (null? remaining-prev)
            (null? remaining-current))
        (reverse next)
        (loop (cdr remaining-prev)
              (cdr remaining-current)
              (cons (update-position (car remaining-prev) (car remaining-current)) next))))
  (loop prev current '()))

(define (propagate-grid manifold)
  (let loop ((prev-row (car manifold))
             (rows     (cdr manifold))
             (acc      (list (car manifold))))
    (if (null? rows)
        (reverse acc)
        (let* ((current-row (car rows))
               (updated-row (propagate-one-step prev-row current-row)))
          (display updated-row)
          (newline)
          (loop current-row
                (cdr rows)
                (cons updated-row acc))))))

;; (define (count-splits manifold)
;;   (define (loop remaining updated count)
;;     (if (= (length remaining) 1)
;;         count
;;         (loop (cdr remaining) updated (1+ count))))
;;   (loop manifold '() 0))

;; With all these helpers in place, we run the main program.
(let* ((manifold (load-input-file "example.txt")))
  (propagate-grid manifold)
;;  (display (count-splits manifold))
  (newline))
