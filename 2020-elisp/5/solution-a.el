(defun read-seat-codes-data (filename)
  "Opens the input file and reads a collection of codes from boarding
passes that map to seats on the plane."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))

(defun halve-list-by-section (lst section)
  "Given a list, return the first or second halves of it based on the
section being asked for."
  (let ((len (floor (/ (length lst) 2)))
        (other-lst '()))
    (dotimes (_ len)
      (push (pop lst) other-lst))
    (cond
     ((equal section 'first-half) (reverse other-lst))
     ((equal section 'second-half) lst))))

(defun code->section (code)
  "Map codes in boarding passes to sections (first or second halves)
of lists"
  (cond
   ((or (equal code ?F) (equal code ?L)) 'first-half)
   ((or (equal code ?B) (equal code ?R)) 'second-half)))

(defun identify-seat-id (codes)
  "Given the full codes of a seat on a boarding pass, find the ID of
the seat (based on the column and row numbers."
  (let ((rows (number-sequence 0 127))
        (cols (number-sequence 0 7))
        (rows-codes (string-to-list (substring codes 0 7)))
        (cols-codes (string-to-list (substring codes 7 10))))

    (while rows-codes
      (setq row-code (car rows-codes))
      (setq rows (halve-list-by-section rows (code->section row-code)))
      (setq rows-codes (cdr rows-codes)))
    (setq row (car rows))

    (while cols-codes
      (setq col-code (car cols-codes))
      (setq cols (halve-list-by-section cols (code->section col-code)))
      (setq cols-codes (cdr cols-codes)))
    (setq col (car cols))

    (+ (* row 8) col)))

(defun identify-all-seat-ids (all-seat-codes)
  "Map all seat codes to their seat ids"
  (let ((seat-ids '()))
    (while all-seat-codes
      (setq seat-code (car all-seat-codes))
      (push (identify-seat-id seat-code) seat-ids)
      (setq all-seat-codes (cdr all-seat-codes)))
    seat-ids))

(defun identify-highest-seat-id (all-seat-codes)
  "Go through all seat codes and find the one with the highest ID"
    (apply #'max (identify-all-seat-ids all-seat-codes)))

(let ((all-seat-codes (read-seat-codes-data "input.txt")))
  (message "%d" (identify-highest-seat-id all-seat-codes)))
