
;;; setup-general.el --- Initialization file for Emacs

;;; Commentary: Emacs Startup File --- initialization for Emacs
;; Copyright (C) 2012-2017 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Modified: 29 Nov 2017
;; Version: 2.4
;; Package-Requires: ((emacs "24.3") (bind-key "2.4"))
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/jwiegley/use-package

;; general setup, theme, global operations etc.

(setq make-backup-files nil)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(setq backup-directory-alist (quote (("." . "~/.backups"))))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-=") 'er/expand-region)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; ;; Compilation
;; (global-set-key (kbd "<f5>") (lambda ()
;;                                (interactive)
;;                                (setq-local compilation-read-command nil)
;;                                (call-interactively 'compile)))

;; setup GDB
(setq-default
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; company
(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends))

;; Package: projejctile
(use-package projectile
  :init
  (projectile-mode t)
  (setq projectile-enable-caching t))

;; Package zygospore
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)
(windmove-default-keybindings)

(require 'recentf)
(recentf-mode t)
(setq-default recentf-max-menu-item 20)

(use-package dracula-theme
  :ensure t
  :config
  :init
  (add-hook 'after-init-hook
			'(lambda ()
			   (load-theme 'dracula t))))

(use-package dashboard
  :ensure t
  :config
  (add-hook 'after-init-hook
            '(lambda ()
               (dashboard-setup-startup-hook))))

(set-face-attribute 'default nil :height 120)
(when (memq window-system '(mac ns))
  (set-face-attribute 'default nil :height 140))

(setq org-agenda-files '("~/Dropbox/org"))

(define-key global-map (kbd "C-c t") 'helm-tramp)

(use-package sr-speedbar
  :ensure t
  :init
  (global-set-key (kbd "s-s") 'sr-speedbar-toggle))

;; hlt-hlight
(use-package highlight
  :ensure t
  :init
  (global-set-key [f8] 'hlt-highlight-symbol)
  (global-set-key [f9] 'hlt-unhighlight-symbol))

(global-hl-line-mode t)
(global-auto-revert-mode t)

;; aggressive-indent
(use-package aggressive-indent
  :ensure t
  :init)

(add-hook 'prog-mode-hook
          '(lambda ()
             (smartparens-mode t)
             (aggressive-indent-mode t)
             (define-key global-map (kbd "M-r") 'helm-gtags-find-rtag)
             (define-key global-map (kbd "M-t") 'helm-dwim-target)
             (linum-mode t)
             (show-paren-mode t)))

;; function-args
(use-package function-args
  :ensure t
  :init
  ())

;; (setq sml/theme 'smart-mode-line-powerline)
;; (add-hook 'after-init-hook 'sml/setup)

(use-package flycheck
  :ensure t
  :init
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode t))

(use-package minions
  :ensure t
  :init (minions-mode)
  :config
  (setq
   minions-mode-line-lighter "#"
   minions-direct '(flycheck-mode)))

(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; (add-hook 'after-init-hook #'global-flycheck-mode)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; for occur
(defun occur-dwim ()
  "Call `occur' with a sane default." (interactive)
  (push (if (region-active-p)
			(buffer-substring-no-properties (region-beginning)
											(region-end))
		  (let ((sym (thing-at-point 'symbol)))
			(when (stringp sym)
			  (regexp-quote sym))))
		regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defvar scratch-run-alist
  '(("java"   . java-mode)
    ("c++"    . c++-mode)
    ("perl"   . perl-mode)
    ("python" . python-mode)
    ("js"     . javascript-mode)
    ("j"      . j-mode)
    ("tcl"    . tcl-mode))
  "生成草稿buffer的简短mode名称列表")
(defun scratch-run ()
  "Run a scratch"
  (interactive)
  (let ((mode (ido-completing-read
               "What kind of scratch mode ?:"
               (append (all-completions ""
                                        obarray
                                        (lambda (s)
                                          (and (fboundp s)
                                               (string-match "-mode$" (symbol-name s)))))
                       (mapcar 'car scratch-run-alist)))))
    (pop-to-buffer (get-buffer-create (format "* scratch * %s *" mode)))
    (funcall (if (assoc mode scratch-run-alist)
                 (cdr (assoc mode scratch-run-alist))
               (intern mode)))
    ))

(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "M-o") 'ace-window))

(use-package popwin
  :ensure t
  :init
  :config
  (popwin-mode 1))

(use-package which-key
  :ensure t
  :init
  (which-key-mode t))

(require 's)
(defun retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  " set theUrl to get URL of active tab of first window\n"
                  " set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

(provide 'setup-general)
