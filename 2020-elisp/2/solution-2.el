(defun read-file-into-rules-and-passwords (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (mapcar (lambda (inner-list)
              (list (mapcar 'string-to-number
                            (split-string (car (split-string (car inner-list) " ")) "-"))
                    (cadr (split-string (car inner-list) " "))
                    (string-trim-left (cadr inner-list))))
            (mapcar (lambda (str) (split-string str ":" t))
                    (split-string (buffer-string) "\n" t)))))

(defun verify-password (rule-and-password)
  (setq position-1 (- (nth 0 (nth 0 rule-and-password)) 1))
  (setq position-2 (- (nth 1 (nth 0 rule-and-password)) 1))
  (setq character (nth 1 rule-and-password))
  (setq password (mapcar (lambda (char) (string char)) (nth 2 rule-and-password)))
  (if (xor (equal character (nth position-1 password))
           (equal character (nth position-2 password)))
           t
           nil))

(defun count-valid-passwords-from-list (rules-and-passwords)
  (setq valid 0)
  (while rules-and-passwords
    (setq rule-and-password (car rules-and-passwords))
    (if (verify-password rule-and-password)
        (setq valid (1+ valid)))
    (setq rules-and-passwords (cdr rules-and-passwords)))
  valid)

(setq rules-and-passwords (read-file-into-rules-and-passwords "input.txt"))
(message "%d" (count-valid-passwords-from-list rules-and-passwords))



