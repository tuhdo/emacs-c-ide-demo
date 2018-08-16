

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
(setq android-trace-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("Cmd line" "daemon" "sysTid" "utm" "stm" "tid" "at" "native" "kernel"))
             (x-types '("float" "integer" "key" "list" "rotation" "string" "vector"))
             (x-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
             (x-events '("held by" "at_target" "attach"))
             (x-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-constants-regexp (regexp-opt x-constants 'words))
             (x-events-regexp (regexp-opt x-events 'words))
             (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-events-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode android-trace-mode c-mode "android trace mode"
  "Major mode for editing LSL (Linden Scripting Language)…"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((android-trace-font-lock-keywords))))

(provide 'setup-bugreport)
