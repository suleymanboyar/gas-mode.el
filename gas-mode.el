 ;;; minimal-gas-mode.el --- mode for editing assembly code with AT&T sintax

;; Copyright (C) 2021 Bryan Hernández

;; Author: Bryan Hernández <masterenoc@tutamail.com>
;; Keywords: languages

;;; Commentary:

;;; Code:
(defgroup gas nil
  "Customization for assembly code with AT&T syntax"
  :prefix "gas-"
  :group 'languages)

(defvar gas-comments '("//" ("/\*" . "\*/") "#")
  "Available comments for AT&T syntax.")

(defvar gas-initial-indent-regex "\.*:\\|\\.section"
  "Element which should have less or null indentation, relies on `gas-initial-indent'
to set the indentation needed")

(defcustom gas-initial-indent 0
  "This is the indent use for tags and .section sections"
  :type 'integer
  :group 'gas)

(defcustom gas-indentation 8
  "Indentation for each line"
  :type 'integer
  :group 'gas)

(defun gas-move-to-first-char ()
  "Move point to the first character that is not a whitespace"
  (let ((char (following-char)))
    (if (= char 32)
        (progn (forward-char)
               (gas-move-to-first-char))
      char)))

(defvar gas-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?$ "'" table)
    (modify-syntax-entry ?% "'" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n "> 4c" table)
    (modify-syntax-entry ?# ". 1c" table)
    table)
  "Syntax table for gas mode")

(defvar gas-last-evaluated-token ""
  "Last token evaluated for indentation calculation")

(defun gas-read-token ()
  "Read a token"
  (let ((char (following-char)))
    (if (or
         (= char 32)
         (= char 10))
        t
      (setq gas-last-evaluated-token (concat gas-last-evaluated-token (list char)))
      (forward-char)
      (gas-read-token))))

(defun gas-next-token ()
  "Return the next token"
  (setq gas-last-evaluated-token "")
  (gas-move-to-first-char)
  (gas-read-token))

(defun gas-indent-line ()
  "Indentation function"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (gas-next-token)
    (cond ((string-match-p gas-initial-indent-regex gas-last-evaluated-token)
           (indent-line-to gas-initial-indent))
          (t (indent-line-to gas-indentation))))
  (if (equal gas-last-evaluated-token "")
      (move-to-column gas-indentation)))

;;;###autoload
(define-derived-mode gas-mode prog-mode "gas"
  "Major mode for editing assembly code with AT&T syntax."
  (kill-all-local-variables)
  (setq-local major-mode 'gas-mode)
  (setq-local mode-name "gas")
  (setq-local electric-indent-inhibit t)
  (setq-local indent-line-function 'gas-indent-line)
  (set-syntax-table gas-mode-syntax-table))

(provide 'gas-mode)
