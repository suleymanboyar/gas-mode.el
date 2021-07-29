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

(defvar gas-initial-indent-regex "\.*:\\|\\.section"
  "Element which should have less or null indentation, relies on `gas-initial-indent'
to set the indentation needed")

(defcustom gas-initial-indent 0
  "This is the indent use for tags and .section directive"
  :type 'integer
  :group 'gas)

(defcustom gas-indentation 8
  "Indentation for each line"
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
  "Syntax table for gas mode")

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
    "sysexit" "rdtsc" "int" "leave")
  "Instructions used in assembly programming")

(defconst gas-instructions-regex
  (regexp-opt gas-instructions)
  "regex of `gas-instructions'")

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
    ".8byte" ".bss" ".rodata")
  "Assembler directives (a.k.a Pseudo operators) used by gas")

(defconst gas-registers
  '("%rax" "%eax" "%ax" "%ah" "%al" "%rcx" "%ecx"
    "%cx" "%cx" "%ch" "%cl" "%rdx" "%edx" "%dx"
    "%dh" "%dl" "%rbx" "%ebx" "%bx" "%bh" "%bl"
    "%rsp" "%esp" "%sp" "%spl" "%rbp" "%ebp"
    "%bp" "%bpl" "%rsi" "%esi" "%si" "%sil"
    "%rdi" "%edi" "%di" "%dil" "%eflags" "%rip"
    "%r8" "%r9" "%r10" "%r11" "%r12" "%r13"
    "%r14" "%r15" "%xmm0" "%xmm1" "%xmm2"
    "%xmm3" "%xmm4" "%xmm5" "%xmm6" "%xmm7"
    "%xmm8" "%xmm9" "%xmm10" "%xmm11" "%xmm12"
    "%xmm12" "%xmm13" "%xmm14" "%xmm15" "%ymm0"
    "%ymm1" "%ymm2" "%ymm3" "%ymm4" "%ymm5" "%ymm6"
    "%ymm7" "%ymm8" "%ymm9" "%ymm10" "%ymm11"
    "%ymm12" "%ymm13" "%ymm14" "%ymm15" "%zmm0"
    "%zmm1" "%zmm2" "%zmm3" "%zmm4" "%zmm5" "%zmm6"
    "%zmm7" "%zmm8" "%zmm9" "%zmm10" "%zmm11"
    "%zmm12" "%zmm13" "%zmm14" "%zmm15")
  "Register available for gas (x86 and x86_64 registerl")

(defconst gas-mode-font-lock
  `((,(regexp-opt gas-pseudo-ops) . font-lock-builtin-face)
    ("[a-zA-Z0-9_]+?:" . font-lock-variable-name-face)
    ("%[a-zA-Z0-9]+" . font-lock-variable-name-face)
    (,(concat "\\b" gas-instructions-regex "\\b") . font-lock-builtin-face)) ;; match the exact instruction
  "Keywords used by the AT&T assembly syntax")

(defvar gas-last-evaluated-token ""
  "Last token evaluated for indentation calculation")

(defun gas-move-to-first-char ()
  "Move point to the first character that is not a whitespace"
  (let ((char (following-char)))
    (if (= char 32)
        (progn (forward-char)
               (gas-move-to-first-char))
      char)))

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
  (set-syntax-table gas-mode-syntax-table)
  (setq-local comment-column 40)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-padding 1)
  (setq-local comment-multi-line t)
  (setq-local font-lock-defaults '(gas-mode-font-lock nil t nil nil)))

(provide 'gas-mode)
