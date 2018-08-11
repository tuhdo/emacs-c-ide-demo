
(require 'smartparens)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

(add-hook 'emacs-lisp-mode
		  (lambda ()
			(set (make-local-variable 'company-backends)
				 '((company-elisp company-keywords company-dabbrev)))))

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(provide 'setup-elisp)
