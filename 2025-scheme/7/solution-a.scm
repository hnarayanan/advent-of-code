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

(define (identify-beam-positions l)
  (define (loop remaining i positions)
    (if (null? remaining)
        (reverse positions)
        (if (beamy? (car remaining))
            (loop (cdr remaining) (1+ i) (cons i positions))
            (loop (cdr remaining) (1+ i) positions) )))
  (loop l 0 '()))


;; For each row, we look at the previous row
;; Wherever these is an S or |, we want to copy that over to our row
;; If it's a . in our row, we just copy over | in that position
;; If it's a ^ in our row, we copy over | to before and after that position

(define (count-splits manifold)
  (define (loop remaining updated count)
    (if (= (length remaining) 1)
        count
        (loop (cdr remaining) updated (1+ count))))
  (loop manifold '() 0))


;; With all these helpers in place, we run the main program.
(let* ((manifold (load-input-file "example.txt")))
;;  (display manifold)
;;  (newline)
  (display (identify-beam-positions (list-ref manifold 14)))
;;  (display (count-splits manifold))
  (newline))
