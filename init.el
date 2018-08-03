(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-cn" . "http://elpa.emacs-china.org/melpa/") t)
(add-to-list 'package-archives
             '("org-cn"   . "http://elpa.emacs-china.org/org/") t)
(add-to-list 'package-archives
             '("gnu-cn"   . "http://elpa.emacs-china.org/gnu/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)



;; function-args
(require 'function-args)
(fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)


(setq initial-frame-alist (quote ((fullscreen . maximized))))
(set-face-attribute 'default nil :height 140)

(add-hook 'prog-mode-hook
          '(lambda ()
             (smartparens-mode t)
             (aggressive-indent-mode t)
             (linum-mode t)))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; hlt-hlight
(global-set-key [f8] 'hlt-highlight-symbol)
(global-set-key [f9] 'hlt-unhighlight-symbol)

(global-hl-line-mode t)

(define-key global-map (kbd "C-c t") 'helm-tramp)


(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-item 10)

(load-theme 'manoj-dark)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(helm-mode t)
 '(moo-do-includes t)
 '(package-selected-packages
   (quote
    (aggressive-indent smartparens helm-tramp highlight magit function-args monokai-theme zygospore helm-gtags helm yasnippet ws-butler volatile-highlights use-package undo-tree iedit dtrt-indent counsel-projectile company clean-aindent-mode anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
