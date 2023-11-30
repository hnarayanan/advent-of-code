(defun read-file-into-number-list (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (mapcar 'string-to-number
            (split-string (buffer-string) "\n" t))))

(defun find-solution-from-list (input)
  ;; Find two numbers in the input list that sum to 2020, and then
  ;; return their product
  (catch 'break
    (while input
      (setq first (car input))
      (setq remaining (cdr input))
      (while remaining
        (setq second (car remaining))
        (if (equal 2020 (+ first second))
            (throw 'break (* first second)))
        (setq remaining (cdr remaining))
        )
      (setq input (cdr input))
      )
    nil)
  )

(setq input (read-file-into-number-list "input.txt"))
(setq output (find-solution-from-list input))

(message "%d" output)
