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
;;   - Add support for company.
;;
;;   - Add support for imenu.

;;; Code:
(defgroup gas nil
  "Major mode for editing assembly code with AT&T syntax."
  :prefix "gas-"
  :group 'languages)

(defvar gas-opening-blocks-regex (regexp-opt '(".section" ".macro" ".rept"))
  "Regex that matches elements that marks the beginning a blocks, which means
new lines will have a relative indentation from these elements.")

(defvar gas-closing-blocks-regex (regexp-opt '(".endm" ".endr"))
  "Regex of the directives use to close another directives")

(defvar gas-tag-regex "[.a-zA-Z0-9_]+?:"
  "Regex of tags")

(defvar gas-directive-regex "\\.[a-zA-Z0-9_]+"
  "Regex of directives")

(defcustom gas-initial-indent 0
  "The indentation to use for '.section' directives."
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
    (modify-syntax-entry ?_ "_" table)
    table)
  "Syntax table to use in Gas mode")

(defconst gas-instructions
  '("mov" "movb" "movw" "movl" "movq"
   "push" "pushb" "pushw" "pushl" "pushq"
   "pop" "popb" "popw" "popl" "popq"
   "add" "addb" "addw" "addl" "addq"
   "sub" "subb" "subw" "subl" "subq"
   "mul" "mulb" "mulw" "mull" "mulq"
   "div" "divb" "divw" "divl" "divq"
   "inc" "incb" "incw" "incl" "incq"
   "dec" "decb" "decw" "decl" "decq"
   "and" "andb" "andw" "andl" "andq"
   "or" "orb" "orw" "orl" "orq"
   "xor" "xorb" "xorw" "xorl" "xorq"
   "not" "notb" "notw" "notl" "notq"
   "shl" "shlb" "shlw" "shll" "shlq"
   "shr" "shrb" "shrw" "shrl" "shrq"
   "jmp" "jmpb" "jmpw" "jmpl" "jmpq"
   "call" "callb" "callw" "calll" "callq"
   "ret" "retb" "retw" "retl" "retq"
   "cmp" "cmpb" "cmpw" "cmpl" "cmpq"
   "test" "testb" "testw" "testl" "testq"
   "je" "jeb" "jew" "jel" "jeq"
   "jne" "jneb" "jnew" "jnel" "jneq"
   "jg" "jgb" "jgw" "jgl" "jgq"
   "jge" "jgeb" "jgew" "jgel" "jgeq"
   "jl" "jlb" "jlw" "jll" "jlq"
   "jle" "jleb" "jlew" "jlel" "jleq"
   "loop" "loopb" "loopw" "loopl" "loopq"
   "nop" "nopb" "nopw" "nopl" "nopq"
   "adc" "addps" "addsd" "brtw" "bt"
   "btb" "btl" "btq" "btr" "btrb"
   "btrl" "btrq" "bts" "btsb" "btsl"
   "btsq" "btsw" "btw" "clc" "cld"
   "cli" "cmcsysenter" "cmov" "cmova" "cmovae"
   "cmovb" "cmovbe" "cmovc" "cmovg" "cmovge"
   "cmovl" "cmovle" "cmovnae" "cmovnb" "cmovnbe"
   "cmovnc" "cmovne" "cmovng" "cmovnge" "cmovnl"
   "cmovnle" "cmovno" "cmovnp" "cmovns" "cmovnz"
   "cmovo" "cmovpe" "cmovpo" "cmovs" "cmovvp"
   "cmovz" "cmpsb" "cmpxchg" "cpuid" "divsd"
   "enter" "hit" "idiv" "imul" "in"
   "int" "ja" "jae" "jbe" "jcxz"
   "jecxz" "jlejb" "jno" "jns" "jnz"
   "jo" "jrcxz" "js" "jz" "lahf"
   "ldmxcsr" "lds" "lea" "leaq" "leave"
   "les" "lock" "loopcc" "movdqa" "movs"
   "movsb" "movsd" "movsl" "movsq" "movss"
   "movsw" "movups" "movz" "movzb" "movzl"
   "movzq" "movzw" "mulsd" "neg" "out"
   "paddd" "pextrd" "popa" "popad" "popf"
   "pusha" "pushad" "pushf" "rcl" "rcr"
   "rdtsc" "rep" "repe" "repne" "rol"
   "ror" "sahf" "sal" "sar" "sbb"
   "scasb" "scasw" "shld" "shrd" "sqrtsd"
   "stc" "std" "sti" "stmxcsr" "stosb"
   "stosq" "stosw" "subsd" "syscall" "sysexit"
   "wait" "xchg" "xchgb" "xchgl" "xchgq"
   "xchgw" "xlat")
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
  `((,gas-tag-regex . font-lock-variable-name-face)
    (,(regexp-opt gas-pseudo-ops) . font-lock-builtin-face)
    ("%[a-zA-Z0-9]+" . font-lock-variable-name-face)
    (,(concat "\\b" gas-instructions-regex "\\b") . font-lock-builtin-face)) ;; match the exact instruction
  "Font-lock regexes used to fontify assembly code with AT&T syntax ")

(defvar gas-last-evaluated-token ""
  "Last token evaluated for indentation calculation, used only for debugging and bug
reports")

(defvar gas-consecutive-indentation 0
  "Count the times `indent-for-tab-command' is executed in the same line")

(defvar gas-current-indentation 0
  "Helps to avoid calculation when the indentation is trivial")

(defvar gas-manual-indentation-level 0
  "Stores the indentation when the first manual indentation is used")

(defun gas-move-to-first-char ()
  "Move point to the first character that is not a whitespace"
  (let ((char (following-char)))
    (if (= char 32)
        (progn (forward-char)
               (gas-move-to-first-char))
      char)))

(defun gas-read-token (&optional string)
  "Read a token of text. It returns a token (if exists) from the current pointer position
to the nearest newline or space character"
  (let ((char (following-char)))
    (if (or
         (= char 32)
         (= char 10)
         (= char 0))
        (concat string)
      (forward-char)
      (gas-read-token (append string `(,char))))))

(defun gas-next-token ()
  "Moves pointer to the next non-whitespace character, read a token and return it"
  (gas-move-to-first-char)
  (setq gas-last-evaluated-token (gas-read-token))) ;; set for debugging

(defun gas-calculate-indentation-from-prev-lines ()
  "Calculate de indentation based on the previous lines and return the indentation and
the token used to calculate the indentation as a cons cell"
  (let ((token nil))
    ;; If beginning of buffer is reached should indent to 0
    ;; since it did not find any suitable line
    (if (= (forward-line -1) -1)
        (cons gas-indentation "")
      (setq token (gas-next-token))
      ;; if last line is empty (0 indent), recursively evaluate more lines
      (cond ((equal token "")
             (gas-calculate-indentation-from-prev-lines))

            ((string-match-p gas-opening-blocks-regex token)
             (cons (+ (current-indentation) gas-indentation) token))

            (t
             (cons (current-indentation) token))))))

(defun gas-calculate-indentation ()
  "Calculate de indentation based on the first token of the current line and previous lines
indentation and token"
  (save-excursion
    (let ((token (progn (beginning-of-line) (gas-next-token)))
          (prev-indt (gas-calculate-indentation-from-prev-lines)))
      (cond
       ((string-match-p ".section" token)
        gas-initial-indent)

       ((string-match-p gas-closing-blocks-regex token)
        (- (car prev-indt) gas-indentation))

       ((string-match-p gas-tag-regex (cdr prev-indt))
        (if (string-equal "" (gas-next-token))
            (+ gas-indentation (car prev-indt))
          (car prev-indt)))

       ((string-match-p gas-directive-regex (cdr prev-indt))
        (setq token (gas-calculate-indentation-from-prev-lines))
        (if (string-match-p gas-tag-regex (cdr token))
            (car token)
          (car prev-indt)))

       (t
        (car prev-indt))))))

(defun gas-manual-indentation ()
  "Manual indentation for lines. The indentation is a multiple of `gas-indentation' and
is calculated depending how many times `indent-for-tab-command' is executed in a row"
  (setq gas-consecutive-indentation (1+ gas-consecutive-indentation))
  (save-excursion
    (when (= gas-consecutive-indentation 1)
      (setq gas-manual-indentation-level (current-indentation)))
    (when (= (* gas-indentation (1- gas-consecutive-indentation)) gas-manual-indentation-level)
      (setq gas-consecutive-indentation (1+ gas-consecutive-indentation)))
    (indent-line-to (* gas-indentation (1- gas-consecutive-indentation)))))

(defun gas-indent-line ()
  "Indentation function used to calculate the indentation level."
  (interactive)
  ;; Heuristic to determine if line needs more or less indentation
  (if (eq last-command 'indent-for-tab-command)
      (gas-manual-indentation)
    (setq gas-consecutive-indentation 0)
    (save-excursion
      (indent-line-to (gas-calculate-indentation))))
  ;; move pointer to the beginning of the line if it is before the indentation
  (if (< (current-column) (current-indentation))
      (move-to-column (current-indentation))))

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
