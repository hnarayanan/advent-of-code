(defun read-file-into-repeated-grid (filename times)
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((lines (mapcar (lambda (line) (apply 'concat (make-list times line)))
                         (split-string (buffer-string) "\n" t))))
      lines)))

(defun count-trees-in-path (map-grid delta-x delta-y)
  (let ((tree-count 0)
        (x delta-x)
        (y delta-y))
    (dotimes (i (- (/ (length map-grid) delta-y) delta-y))
      (let ((content-at-position (string (aref (nth y map-grid) x))))
        (if (equal content-at-position "#")
            (setq tree-count (1+ tree-count))))
      (setq x (+ delta-x x))
      (setq y (+ delta-y y)))
    tree-count))


(let* ((map-grid (read-file-into-repeated-grid "input.txt" 100))
       (path-1 (count-trees-in-path map-grid 1 1))
       (path-2 (count-trees-in-path map-grid 3 1))
       (path-3 (count-trees-in-path map-grid 5 1))
       (path-4 (count-trees-in-path map-grid 7 1))
       (path-5 (count-trees-in-path map-grid 1 2)))
  (message "%d" (* path-1 path-2 path-3 path-4 path-5)))


;; (let ((map-grid (read-file-into-repeated-grid "input.txt" 100)))
;;   (message "%d" (count-trees-in-path map-grid 1 2)))


