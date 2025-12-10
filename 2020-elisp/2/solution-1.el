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

(defun count-characters (character string)
  (setq count 0)
  (setq stringlist (mapcar (lambda (char) (string char)) string))
  (while stringlist
    (setq charac (car stringlist))
    (if (equal charac character)
        (setq count (1+ count)))
    (setq stringlist (cdr stringlist)))
  count)

(defun verify-password (rule-and-password)
  (setq min (nth 0 (nth 0 rule-and-password)))
  (setq max (nth 1 (nth 0 rule-and-password)))
  (setq character (nth 1 rule-and-password))
  (setq password (nth 2 rule-and-password))
  (setq character-count (count-characters character password))
  (if (and (>= character-count min) (<= character-count max))
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



