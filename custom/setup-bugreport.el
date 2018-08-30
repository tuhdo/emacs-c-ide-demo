
;; reference http://ergoemacs.org/emacs/elisp_syntax_coloring.html

(defun android-trace-modify ()
  "Better to show android traces file with org mode."
  (goto-char (point-min))
  (insert "* android trace file")
  (while (search-forward-regexp "^Cmd line" nil t) (replace-match "** Cmd line"))
  (goto-char (point-min)))

(defun android-trace-view ()
  "Show android trace file"
  (interactive)
  (message "show android trace")
  (kill-ring-save (point-min) (point-max))
  (pop-to-buffer (get-buffer-create (format "* scratch * %s *" (buffer-name))))
  (yank)
  (goto-char (point-min))
  (android-trace-modify)
  (org-mode)
  (message "show android trace done"))


;; create the list for font-lock.
;; each category of keyword is given a particular face
(defvar android-trace-font-lock-keywords nil "Syntax table for `android-trace-mode'.")
(setq android-trace-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("Cmd line" "Build fingerprint" "ABI" "Build type" "at " "native:" "kernel:" "Command line"))
             (x-types '("utm=" "stm=" "tid="))
             (x-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
             (x-events '("held by" "epoll" "held by thread" "waiting to lock" "locked"))
             (x-functions '("getAndExecuteCommand" "waitForResponse" "pthread_mutex_lock"))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-constants-regexp (regexp-opt x-constants 'words))
             (x-events-regexp (regexp-opt x-events 'words))
             (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
		  ;; reference http://ergoemacs.org/emacs/elisp_font_lock_mode.html
		  ("\\[[^\]]+\\]" . font-lock-comment-face)
		  ("^<[^>]+>" . font-lock-comment-face)
		  ("<aka>\\([^<]+?\\)</aka>" . (1 font-lock-keyword-face))
		  ("=\\([0-9]+\\)" . (1 font-lock-variable-name-face))
		  ("^=[^$]+" . font-lock-variable-name-face)
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-events-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

(defvar android-trace-mode-syntax-table nil "Syntax table for `android-trace-mode'.")

(setq android-trace-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        ;; python style comment: “# …”
        (modify-syntax-entry ?= "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        synTable))

;;;###autoload
(define-derived-mode android-trace-mode text-mode  "android trace mode"
  "Major mode for editing LSL (Linden Scripting Language)…"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((android-trace-font-lock-keywords)))
  (set-syntax-table android-trace-mode-syntax-table))


(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))

(defun android-trace-get-cmd-at-point(&optional pline pcol)
  (let (start end result)
	(setq result "Not Found")
	(save-excursion
	  (if (re-search-backward "Cmd line" nil t)
		  (progn
			(setq start (point))
			(move-end-of-line nil)
			(setq end (point))
			(setq result (buffer-substring-no-properties start end))))
	  result)))

;; for debug
(defun android-trace-cmd-at-point()
  (interactive)
  (let (result)
	(setq result (android-trace-get-cmd-at-point))
	(if result (message result) (message "Not Found"))))

(add-hook 'android-trace-mode-hook
		  (lambda ()
			(which-function-mode t)
			(add-hook 'which-func-functions 'android-trace-get-cmd-at-point t t)))

;; Show the current function name in the header line
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))

(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

(setq-default which-func-unknown "N/A")

;; refs https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html
;; filename startup with something
(add-to-list 'auto-mode-alist '("/dumptrace_[\\.]*" . android-trace-mode))
(add-to-list 'auto-mode-alist '("/anr_[\\.]*" . android-trace-mode))
(add-to-list 'auto-mode-alist '("/bugreport_[\\.]*" . android-trace-mode))
;; endup with .trace
(add-to-list 'auto-mode-alist '("\\.trace\\'" . android-trace-mode))
;;(add-to-list 'auto-mode-alist '("anr[[:ascii:]]*\\'" 'android-trace-mode))
(provide 'setup-bugreport)
