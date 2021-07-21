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

(defvar gas-initial-indent-regex '(".*:\\|\.section")
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

(defun gas-next-char ()
  "Return the next char"
  (let ((char (following-char)))
    (forward-char)
    char))

(defun gas-read-until-nonwhitespace ()
  "Return the first character found that is not a whitespace"
  (let ((char (following-char)))
    (if (= char 32)
        (progn (forward-char)
               (gas-next-char))
      char)))

(defvar gas-last-evaluated-token ""
  "Last token evaluated for indentation calculation")

(defun gas-read-pseudo-op ()
  "Read a pseudo operator"
  (let ((char (gas-next-char)))
    (if (= char 32) t
      (concat gas-last-evaluated-token (list char))
      (gas-read-pseudo-op))))

(defun gas-next-token ()
  "Return the next token"
  (let ((char (gas-read-until-nonwhitespace)))
    (cond ((= char 46)
           (gas-read-pseudo-op))
          (t
           (gas-read-token)))))

(defun gas-indent-line ()
  "Indentation function"
  (interactive)
  (let ((beg nil) (end nil))
    (end-of-line)
    (setq end (point))
    (beginning-of-line)))

;;;###autoload
(define-derived-mode gas-mode prog-mode "gas"
  "Major mode for editing assembly code with AT&T syntax."
  (kill-all-local-variables)
  (setq-local major-mode 'gas-mode)
  (setq-local mode-name "gas")
  (setq-local electric-indent-inhibit t)
  (setq-local indent-line-function 'gas-indent-line))

(provide 'gas-mode)
