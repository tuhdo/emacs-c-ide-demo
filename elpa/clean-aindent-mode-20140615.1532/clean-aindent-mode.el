;;; clean-aindent-mode.el --- Simple indent and unindent, trims indent white-space

;; This is free and unencumbered software released into the public domain.
;; (http://unlicense.org)

;; Author: peter marinov <efravia@gmail.com>
;; Created: 2013-08-17
;; Last: 2014-06-14
;; Version: 20140615.1532
;; X-Original-Version: 1.4.0
;; License: C0 (public domain)
;; URL: https://github.com/pmarinov/clean-aindent-mode
;; Doc URL: http://www.emacswiki.org/emacs/CleanAutoIndent
;; Keywords: indentation whitespace backspace

;; This file is not part of GNU Emacs.

;;; Commentary:

;; === Description of the features
;; 1. Extension of 'newline-and-indent' that keeps track of the last
;; auto-indent operation and, if it is abandoned, would take care to
;; trim down the unused white space characters.
;;
;; 2. Simple indent, if activated, where cursor is aligned with
;; indent of the lines above.
;;
;; 3. Backspace Unindent. Extension of M-backspace.
;; When cursor is in the indentation space of a line, or at the first
;; character and you press M-backspace it will move the entire line to
;; be aligned to the line above or any other that is with indentation
;; smaller than the current.
;;
;; === To activate
;; 'M-x clean-aindent-mode'
;; or
;; add this to your init.el:
;; (clean-aindent-mode t)
;;
;; By default auto-indent is bound to 'C-j'. Bind it to 'RET' for most
;; convenient use of the features. Add this to your init.el:
;; (define-key global-map (kbd "RET") 'newline-and-indent)
;;
;; === Options
;; M-x customize, search for 'auto indent', toggle to on,
;; then 'Apply and Save'.
;; or
;; add this to your init.el:
;; (set 'clean-aindent-is-simple-indent t)
;;

;;; Change Log:
;;
;; 2014-06-14, pmarinov, v1.4.0
;;     - Implement as an advice to 'newline-and-indent'
;;
;; 2014-06-01, pmarinov, v1.3.0
;;     - Activate via a minor mode
;;     - Further cleanup of the name space
;;
;; 2014-05-27, pmarinov, v1.2.0
;;     Changed: Move all function under the same namespace (function prefix)
;;
;; 2014-03-07, pmarinov, v1.1.0
;;     Added: Simple auto indent feature. Configurable via M-x customize.
;;
;; 2013-08-31, pmarinov, v1.0.0
;;     First implementation.
;;


(defgroup clean-aindent nil
  "Settings for 'clean-aindent-mode'"
  :group 'indent)

(defcustom clean-aindent-is-simple-indent nil
  "Determines if indentation should use the smart language mode or simple mode"
  :tag "Clean auto indent is in simple mode"
  :group 'clean-aindent
  :type 'boolean)

;;
;; Implementation of Clean auto indent and simple indent
;;

(defun clean-aindent--get-indent-len()
  "Computes the length of the line at 'clean-aindent--last-indent."
  (let ((eol-pos 0))
    (save-excursion
      (goto-char clean-aindent--last-indent)
      (end-of-line)
      (setq eol-pos (point))
      (beginning-of-line)
      ;; return ln-len = eol-pos - pos
      (- eol-pos (point)))))

(defun clean-aindent--abandonedp()
  "Checks if last auto-indent position was abandoned.
Verifies if cursor moved away and that the indent was left
unaltered."
  (if (not clean-aindent--last-indent)
    nil
    ;; (message "clean-aindent--last-indent %d point %d" clean-aindent--last-indent (point))
    (if (= clean-aindent--last-indent (point))
      nil
      ;; Checking for indent length is to detect if something was not
      ;; typed to alter it. Altered indent shouldn't be trimmed.
      (if (not (= clean-aindent--last-indent-len (clean-aindent--get-indent-len)))
        nil
        t))))

(defun clean-aindent--trim-last-point()
  "Deletes the whitespaces inserted at last indentation"
  (save-excursion
    (goto-char clean-aindent--last-indent)
    ; Select the entire line
    (let ((s 0)
         (e 0))
      (beginning-of-line)
      (setq s (point))
      (end-of-line)
      (setq e (point))
      (delete-trailing-whitespace s e)
      (end-of-line)
      (message "auto trimmed %d chars" (- e (point))))))

(defun clean-aindent--check-last-point()
  "Checks if last pos of auto-indent was abandoned and deletes it"
  (if (clean-aindent--abandonedp)
    (clean-aindent--trim-last-point))
  ;; Once we leave the position, clean the indent bookmark
  (if
    (and
      clean-aindent--last-indent
      (not (= clean-aindent--last-indent (point))))
    (setq clean-aindent--last-indent nil)))

(defun clean-aindent--find-indent()
  "Searches lines backward, finds first non-blank. Returns
indentation value"
  (save-excursion
    ;; Walk lines backward, until first non-blank
    (clean-aindent--prev-line)
    ;; Return indentation of that line
    (current-indentation)))

(defun clean-aindent--simple-newline-and-indent()
  "Simple auto indent. Indentation is based only on previous line
indentation, regardless of language settings."
  ;; First remove any trailing spaces from the current line
  (save-excursion
    (let ((s 0)
         (e 0))
      (beginning-of-line)
      (setq s (point))
      (end-of-line)
      (setq e (point))
      (delete-trailing-whitespace s e)
      (end-of-line)))
  ;; Insert a new line and indent
  (newline)
  (indent-to (clean-aindent--find-indent) 0))

(defadvice newline-and-indent (around clean-aindent)
  "Advice for newline-and-indent(), implements clean auto-indent.
Removes unneeded whitespaces by keeping track of the place of the
last indentation so that they can be deleted in case the indentation was
abandoned."
  (clean-aindent--check-last-point)  ;; In case of consequtive aindent calls
  (if clean-aindent-is-simple-indent
      (clean-aindent--simple-newline-and-indent)  ;; Run our simple indent feature
    (progn
     ad-do-it))  ;; Run 'newline-and-indent' here
  (setq clean-aindent--last-indent nil)
  ;; Make local: track on per buffer basis
  (make-local-variable 'clean-aindent--last-indent)
  ;; Track position and length of the indentation
  (setq clean-aindent--last-indent (point))
  (setq clean-aindent--last-indent-len (clean-aindent--get-indent-len))
  (make-local-variable 'clean-aindent--last-indent-len))

;;
;; Backspace-unindent implementation functions
;;

(defun clean-aindent--get-line-len()
  "Computes length of current line"
  (save-excursion
    (beginning-of-line nil)
      (let ((pos (point)))
        (end-of-line nil)
        (- (point) pos))))

(defun clean-aindent--line-emptyp()
  "Checks if line is empty"
  (save-excursion
    (beginning-of-line nil)
    (if (= (point) 1)
      nil
      (= (clean-aindent--get-line-len) 0))))

(defun clean-aindent--prev-line()
  "Move cursor to previous line, skip empty lines"
  (let ((c (point)))
    (while
      (and
        (= 0 (forward-line -1))
        (clean-aindent--line-emptyp)))
    ;; return 't if we moved, nil if already beginning of buffer
    (not (= c (point)))))

(defun clean-aindent--find-u-indent(start)
  "Searches lines backward, finds the one that is indented less
than certain indentation t"
  (save-excursion
    (let (c)
      (while
        (and
          (setq c (current-indentation))
          (> c 0)
          ;; Find an indent smaller than _start_
          (<= start c)
          ;; Walk lines backward
          (clean-aindent--prev-line)))
      ;; _c_ is the computed unindent size
      c)))

(defun clean-aindent--inside-indentp()
  "Returns true if cursor is in the leading whitespace or first
non-blank character of a line"
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line nil)
      (skip-chars-forward " \t")
      (if (<= pos (point))
        t
        nil))))

(defun clean-aindent--line-point()
  "Get (point) at the beginning of the current line"
  (save-excursion
    (beginning-of-line)
    (point)))

(defun clean-aindent--goto-column(col)
  "Moves the cursor to a certain column position.
Column position is different from char position because of TABs"
  (beginning-of-line nil)
  (while (< (current-column) col)
    (right-char)))

(defun clean-aindent--bsunindent(arg)
  "Unindents.
Bound to `M-backspace' key. Searches lines backward, finds the one that
is indented less than the current one. Unindents current line to
align with that smaller indentation"
  (interactive "p")
  (if (not (clean-aindent--inside-indentp))
      (kill-word (- arg))  ;; Original "C-backspace" key function
    ;; else: cursor is inside indent space, do unindent
    (let*
        ((ln (clean-aindent--line-point))
        (c (current-indentation))
        (n (clean-aindent--find-u-indent c))  ;; compute new indent
        (s (+ ln n)))  ;; start of region to delete
      (if (not (= s c))
        (progn
          ;; (message "new unindent %d" n)
          ;; Delete characters between s to c
          (clean-aindent--goto-column c)
          (backward-delete-char-untabify (- c n)))))))


;;
;; Initial setup
;;

(defvar clean-aindent--last-indent nil)
(defvar clean-aindent--last-indent-length 0)

(defvar clean-aindent-mode--keymap (make-keymap) "clean-aindent-mode keymap.")
(define-key clean-aindent-mode--keymap (kbd "M-DEL") 'clean-aindent--bsunindent)

;;;###autoload
(define-minor-mode clean-aindent-mode
  "Activates clean auto indent for function 'newline-and-indent' and
back-space unindent for M-DEL (meta-backspace).

clean-aindent mode is a global minor mode.

1. Extension of 'newline-and-indent' that keeps track of the last
auto-indent operation and, if it is abandoned, would take care to
trim down the unused white space characters.

2. Simple indent, if activated, where cursor is aligned with
indent of the lines above.

3. Backspace Unindent. Extension of M-backspace. When cursor is
in the indentation space of a line, or at the first character and
you press M-backspace it will move the entire line to be aligned
to the line above or any other that is with indentation smaller
than the current."
  :init-value nil  ; The initial value - disabled by default
  :global t        ; Global minor mode
  :keymap clean-aindent-mode--keymap
  ;; BODY
  (if clean-aindent-mode
      ;; Activate
      (progn
      (ad-enable-advice 'newline-and-indent 'around 'clean-aindent)
      (ad-activate 'newline-and-indent)
      (add-hook 'post-command-hook 'clean-aindent--check-last-point))
    ;; Deactivate
    (progn
    (ad-disable-advice 'newline-and-indent 'around 'clean-aindent)
    (ad-activate 'newline-and-indent)
    (remove-hook 'post-command-hook 'clean-aindent--check-last-point))))

(provide 'clean-aindent-mode)
;;; clean-aindent-mode.el ends here
