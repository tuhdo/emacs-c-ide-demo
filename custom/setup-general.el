
;; general setup, theme, global operations etc.

(setq make-backup-files nil)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
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
  (projectile-global-mode)
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
(setq recentf-max-menu-item 10)

(add-hook 'after-init-hook
          '(lambda ()
             (load-theme 'dracula t)))

(use-package dashboard
  :ensure t
  :config
  (add-hook 'after-init-hook
            '(lambda ()
               (dashboard-setup-startup-hook))))

(set-face-attribute 'default nil :height 140)

(setq org-agenda-files '("~/DropBox/org"))

(define-key global-map (kbd "C-c t") 'helm-tramp)

(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; hlt-hlight
(global-set-key [f8] 'hlt-highlight-symbol)
(global-set-key [f9] 'hlt-unhighlight-symbol)

(global-hl-line-mode t)
(global-auto-revert-mode t)

(add-hook 'prog-mode-hook
          '(lambda ()
             (smartparens-mode t)
             (aggressive-indent-mode t)
             (define-key global-map (kbd "M-r") 'helm-gtags-find-rtag)
             (define-key global-map (kbd "M-t") 'helm-dwim-target)
             (linum-mode t)
             (show-paren-mode t)))

;; function-args
(require 'function-args)
(fa-config-default)

;; (setq sml/theme 'smart-mode-line-powerline)
;; (add-hook 'after-init-hook 'sml/setup)
;; (require 'vc-mode)
(require 'flycheck)
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
(menu-bar-mode t)
(tool-bar-mode -1)
(set-scroll-bar-mode -1)

(provide 'setup-general)
