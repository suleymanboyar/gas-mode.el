;;; gas-mode.el --- Mode for editing assembly code with AT&T sintax -*- lexical-biding: t -*-

;; Copyright (C) 2021 Bryan Hernández

;; Author: Bryan Hernández <masterenoc@tutamail.com>
;; Keywords: languages
;; Version: 0.1

;;; Commentary:

;; Provides font-locking and indentation support for Assembly code with AT&T
;; syntax, usually code written for GAS (Gnu Assembler).
;;
;; If you are installing manually, you should add the following code to your
;; .emacs file:
;;      (require 'gas-mode)
;;      (add-to-list 'auto-mode-alist '("\\.asm\\'" . gas-mode))
;;
;; Currently works with basic AT&T syntax, having basic font-locking and
;; indentation.
;;
;; TODO list of improvements or features:
;;
;;   - Improve indentation command to indent tags and .section to either
;;     the beginning of the line (depends on the value of `gas-initial-indent')
;;     or to the normal indentation (`gas-indentation') on the second time the
;;     command is executed.
;;
;;   - Add blinking and indentation to some nested directives like '.macro'.
;;
;;   - Add support for company.
;;
;;   - Add support for imenu.

;;; Code:
(defgroup gas nil
  "Major mode for editing assembly code with AT&T syntax."
  :prefix "gas-"
  :group 'languages)

(defvar gas-opening-blocks-regex (regexp-opt '(".section" ".macro" ".rept"))
  "Regex that matches elements that should have less or null indentation,
relies on `gas-initial-indent'to set the indentation needed.")

(defvar gas-closing-blocks-regex (regexp-opt '(".endm" ".endr"))
  "Regex of the directives use to close another directives")

(defcustom gas-initial-indent 0
  "The indentation to use for the elements matched with `gas-opening-blocks-regex'"
  :type 'integer
  :group 'gas)

(defcustom gas-indentation 8
  "The normal indentation used for instructions and directives"
  :type 'integer
  :group 'gas)

(defvar gas-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?$ "'" table)
    (modify-syntax-entry ?% "'" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table to use in Gas mode")

(defconst gas-instructions
  ;; Probably need t and s suffix
  '("mov" "movb" "movq" "movl" "movw" "xchg" "xchgb"
    "xchgw" "xchgl" "xchgq" "cmpxchg" "movz" "movzb"
    "movzw" "movzl" "movzq" "movs" "movsb" "movsw"
    "movsl" "movsq" "lea" "cmov" "cmovz" "cmov"
    "cmovnz" "cmovne" "cmovo" "cmovno" "cmovs"
    "cmovns" "cmovc" "cmovb" "cmovnae" "cmovnc"
    "cmovnb" "cmovae" "cmovbe" "cmovvp" "cmovpe"
    "cmovnp" "cmovpo" "cmovge" "cmovnl" "cmovnge"
    "cmovl" "cmovng" "cmovle" "cmova" "cmovnbe"
    "cmovg" "cmovnle" "push" "pop" "pusha" "popa"
    "xlat" "in" "out" "lds" "les" "lahf" "sahf"
    "pushf" "popf" "test" "cmp" "jmp" "je" "jne" "jg"
    "jge" "ja" "jae" "jl" "jle" "jb" "jbe" "jo" "jno"
    "jz" "jnz" "js" "jns" "jcxz" "jecxz" "jrcxz"
    "call" "ret" "loop" "loopcc" "enter" "hit" "nop"
    "lock" "wait" "add" "sub" "mul" "imul" "div"
    "idiv" "neg" "adc" "sbb" "inc" "dec" "and" "or"
    "xor" "not" "shr" "shl" "sar" "sal" "shld" "shrd"
    "ror" "rol" "rcr" "rcl" "pushad" "popad" "sti"
    "cli" "std" "cld" "stc" "clc" "cmc" "sysenter"
    "sysexit" "rdtsc" "int" "leave" "movsd" "addsd"
    "subsd" "mulsd" "divsd" "sqrtsd" "leaq" "subq"
    "pushq" "bts" "btr" "bt" "btsq" "btsb" "btsl"
    "btsw" "btq" "btw" "btl" "btb" "btrq" "btr"
    "btrb" "brtw" "btrl" "syscall")
  "Instructions used in assembly programming")

(defconst gas-pseudo-ops
  '(".abort" ".align" ".altmacro" ".ascii" ".asciz"
    ".attach_to_group" ".balign" ".bundle_align_mode"
    ".byte" ".cfi_startproc" ".cfi_endproc" ".comm"
    ".data" ".dc" ".dcb" ".ds" ".def" ".desc" ".dim"
    ".double" ".eject" ".else" ".elseif" ".end"
    ".endef" ".endfunc" ".eendif" ".equ" ".equiv"
    ".eqv" ".err" ".error" ".exitm" ".extern" ".fail"
    ".file" ".fill" ".float" ".func" ".global"
    ".gnu_attribute" ".hidden" ".hword" ".ident"
    ".if" ".incbin" ".include" ".int" ".internal"
    ".irp" ".irpc" ".lcomm" ".lflags" ".line"
    ".linkonce" ".list" ".ln" ".loc" ".loc_mark_labels"
    ".local" ".long" ".macro" ".mri" ".noaltmacro"
    ".nolist" ".nop" ".nops" ".octa" ".offset" ".org"
    ".p2align" ".popsection" ".previous" ".print"
    ".protected" ".psize" ".purgem" ".pushsection"
    ".quad" ".reloc" ".rept" ".sbttl" ".scl" ".section"
    ".set" ".short" ".single" ".size" ".skip"
    ".sleb128" ".space" ".stabd" ".string" ".struct"
    ".subsection" ".symber" ".tag" ".text" ".title"
    ".tls_common" ".type" ".uleb128" ".val" ".version"
    ".vtable_entry" ".vtable_inherit" ".warning" ".weak"
    ".weakref" ".word" ".zero" ".2byte" ".4byte"
    ".8byte" ".bss" ".rodata" ".globl" ".endm")
  "Assembler directives (a.k.a Pseudo operators) used by Gas")

(defconst gas-instructions-regex
  (regexp-opt gas-instructions)
  "Regex that matches all the elements in `gas-instructions'")

(defconst gas-mode-font-lock
  `((,(regexp-opt gas-pseudo-ops) . font-lock-builtin-face)
    ("[a-zA-Z0-9_]+?:" . font-lock-variable-name-face)
    ("%[a-zA-Z0-9]+" . font-lock-variable-name-face)
    (,(concat "\\b" gas-instructions-regex "\\b") . font-lock-builtin-face)) ;; match the exact instruction
  "Font-lock regexes used to fontify assembly code with AT&T syntax ")

(defvar gas-last-evaluated-token ""
  "Last token evaluated for indentation calculation")

(defvar gas-consecutive-indentation 0
  "Count the times `indent-for-tab-command' is executed in the same line")

(defvar gas-current-indentation 0
  "Helps to avoid calculation when the indentation is trivial")

(defun gas-move-to-first-char ()
  "Move point to the first character that is not a whitespace"
  (let ((char (following-char)))
    (if (= char 32)
        (progn (forward-char)
               (gas-move-to-first-char))
      char)))

(defun gas-read-token ()
  "Read a token of text. It returns when it finds a newline or a space character
meaning the end of the token and sets `gas-last-evaluated-token' char by char."
  (let ((char (following-char)))
    (if (or
         (= char 32)
         (= char 10)
         (= char 0))
        t
      (setq gas-last-evaluated-token (concat gas-last-evaluated-token (list char)))
      (forward-char)
      (gas-read-token))))

(defun gas-next-token ()
  "Sets point to the beginning of the next token"
  (setq gas-last-evaluated-token "")
  (gas-move-to-first-char)
  (gas-read-token))

(defun gas-calculate-indentation ()
  "Calculate de indentation based on the previous line and sets `gas-current-indentation'
and `gas-last-evaluated-token'"
  (save-excursion
    (let ((line (forward-line -1)) (indt (current-indentation)))
      ;; If beginning of buffer is reached should indent to 0
      ;; since it did not find any suitable line
      (if (= line -1)
          (progn
            (setq gas-current-indentation 0)
            (setq gas-last-evaluated-token ""))
        (gas-next-token)

        ;; if last line is empty (0 indent), recursively evaluate more lines
        (cond ((equal gas-last-evaluated-token "")
               (gas-calculate-indentation))

              ((string-match-p gas-opening-blocks-regex gas-last-evaluated-token)
               (setq gas-current-indentation (+ indt gas-indentation)))
              (t
               (setq gas-current-indentation indt)))))))

(defun gas-manual-indentation ()
  "Manual indentation for lines. The indentation is a multiple of `gas-indentation' and
is calculated depending how many times `indent-for-tab-command' is executed in a row"
  (let ((indt (* gas-indentation (- gas-consecutive-indentation 1))))
    (indent-line-to indt)
    (setq gas-current-indentation indt)))

(defun gas-indent-line ()
  "Indentation function used to calculate the indentation level."
  (interactive)

  ;; clean `gas-consecutive-indentation' if last command executed is not itself
  (if (eq last-command 'indent-for-tab-command)
      (setq gas-consecutive-indentation (+ 1 gas-consecutive-indentation))
    (setq gas-consecutive-indentation 0))

  ;; Heuristic to determine if line needs more or less indentation
  (save-excursion
    (if (not (= gas-consecutive-indentation 0))
        (gas-manual-indentation)
      (gas-calculate-indentation)
      (beginning-of-line)
      ;; get token of the current line and evaluate to set proper indentation
      (gas-next-token)
      (if (string-match-p gas-closing-blocks-regex gas-last-evaluated-token)
          (indent-line-to (- gas-current-indentation gas-indentation))
        (indent-line-to gas-current-indentation))))

  ;; move pointer to the beginning of the line if is before the indentation
  (if (< (current-column) gas-current-indentation)
      (move-to-column gas-current-indentation)))

;;;###autoload
(define-derived-mode gas-mode prog-mode "gas"
  "Major mode for editing assembly code with AT&T syntax."
  (kill-all-local-variables)
  (setq-local major-mode 'gas-mode)
  (setq-local mode-name "gas")
  (setq-local electric-indent-inhibit t)
  (setq-local indent-line-function 'gas-indent-line)
  (set-syntax-table gas-mode-syntax-table)
  (setq-local comment-column 40)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-padding 1)
  (setq-local comment-multi-line t)
  (setq-local font-lock-defaults '(gas-mode-font-lock nil t nil nil)))

(provide 'gas-mode)
