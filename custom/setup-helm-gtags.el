;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(use-package helm-gtags
  :init
  (progn
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)

    ;; Enable helm-gtags-mode in Dired so you can jump to any tag
    ;; when navigate project tree with Dired
    (add-hook 'dired-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in Eshell for the same reason as above
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)

    ;; Enable helm-gtags-mode in languages that GNU Global supports
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'java-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
      (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

(provide 'setup-helm-gtags)
