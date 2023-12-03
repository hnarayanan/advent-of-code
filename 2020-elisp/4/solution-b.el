(defun string-list-to-dict (string-list)
  (mapcar (lambda(string)
            (let ((key-value (split-string string ":" t)))
              (cons (car key-value) (cadr key-value))))
          string-list))

(defun read-passport-data-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n\n+" t)))

(defun valid-byr? (byr)
  (and (>= byr 1920) (<= byr 2002)))

(defun valid-iyr? (iyr)
  (and (>= iyr 2010) (<= iyr 2020)))

(defun valid-eyr? (eyr)
  (and (>= eyr 2020) (<= eyr 2030)))

(defun valid-hgt? (hgt)
  (let ((valid nil))
    (if (> (length hgt) 1)
        (let ((unit (substring hgt -2))
              (value (string-to-number (substring hgt 0 -2))))
          (if (and (equal unit "cm") (and (>= value 150) (<= value 193)))
              (setq valid t)
            (if (and (equal unit "in") (and (>= value 59) (<= value 76)))
              (setq valid t)))))
    valid))


(defun valid-hcl? (str)
  (string-match-p "#[0-9a-fA-F]\\{6\\}" str))

(defun valid-ecl? (ecl)
  (let ((valid-colors '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))
    (member ecl valid-colors)))

(defun valid-pid? (pid)
  (and (= (length pid) 9) (string-match-p "^[0-9]+$" pid)))

(defun valid-fields? (passport-data)
  (let ((valid t)
        (properties (string-list-to-dict (split-string passport-data "[ \n]+" t))))

    (let ((byr (cdr (assoc "byr" properties))))
      (if (not byr)
          (setq valid nil)
        (if (not (valid-byr? (string-to-number byr)))
            (setq valid nil))))

    (let ((iyr (cdr (assoc "iyr" properties))))
      (if (not iyr)
          (setq valid nil)
        (if (not (valid-iyr? (string-to-number iyr)))
            (setq valid nil))))

    (let ((eyr (cdr (assoc "eyr" properties))))
      (if (not eyr)
          (setq valid nil)
        (if (not (valid-eyr? (string-to-number eyr)))
            (setq valid nil))))

    (let ((hgt (cdr (assoc "hgt" properties))))
      (if (not hgt)
          (setq valid nil)
        (if (not (valid-hgt? hgt))
            (setq valid nil))))

    (let ((hcl (cdr (assoc "hcl" properties))))
      (if (not hcl)
          (setq valid nil)
        (if (not (valid-hcl? hcl))
            (setq valid nil))))

    (let ((ecl (cdr (assoc "ecl" properties))))
      (if (not ecl)
          (setq valid nil)
        (if (not (valid-ecl? ecl))
            (setq valid nil))))

    (let ((pid (cdr (assoc "pid" properties))))
      (if (not pid)
          (setq valid nil)
        (if (not (valid-pid? pid))
            (setq valid nil))))

    valid))

(defun count-valid-passports (passports-data)
  (let ((valid-passport-count 0))
    (while passports-data
      (let ((passport-data (car passports-data)))
        (if (valid-fields? passport-data)
            (setq valid-passport-count (1+ valid-passport-count))))
      (setq passports-data (cdr passports-data)))
    valid-passport-count))

(let* ((passports-data (read-passport-data-file "input.txt"))
       (valid-passport-count (count-valid-passports passports-data)))
  (message "%d" valid-passport-count))
