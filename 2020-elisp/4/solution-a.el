(defun read-passports-data (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n\n+" t)))

(defun has-required-fields (passport-data)
  (let ((required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
        (valid t))
    (while required-fields
      (let ((required-field (car required-fields)))
        (if (not (string-match (concat required-field ":") passport-data))
            (setq valid nil))
        )
      (setq required-fields (cdr required-fields)))
    valid
    ))

(defun count-valid-passports (passports-data)
  (let ((valid-passport-count 0))
    (while passports-data
      (let ((passport-data (car passports-data)))
        (if (has-required-fields passport-data)
            (setq valid-passport-count (1+ valid-passport-count))))
      (setq passports-data (cdr passports-data)))
    valid-passport-count))

(let* ((passports-data (read-passports-data "input.txt"))
       (valid-passport-count (count-valid-passports passports-data)))
  (message "%d" valid-passport-count))
