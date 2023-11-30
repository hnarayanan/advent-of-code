(defun read-file-into-number-list (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (mapcar 'string-to-number
            (split-string (buffer-string) "\n" t))))

(defun find-solution-from-list (input)
  ;; Find three numbers in the input list that sum to 2020, and then
  ;; return their product
  (catch 'break
    (while input
      (setq first (car input))
      (setq first-remaining (cdr input))
      (while first-remaining
        (setq second (car first-remaining))
        (setq second-remaining (cdr first-remaining))
        (while second-remaining
          (setq third (car second-remaining))
          (if (equal 2020 (+ first second third))
              (throw 'break (* first second third)))
          (setq second-remaining (cdr second-remaining)))
        (setq first-remaining (cdr first-remaining))
        )
      (setq input (cdr input))
      )
    nil)
  )

(setq input (read-file-into-number-list "input.txt"))
(setq output (find-solution-from-list input))

(message "%d" output)
