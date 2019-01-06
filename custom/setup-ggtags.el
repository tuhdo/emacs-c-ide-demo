(use-package ggtags
  :init
  (progn

    (ggtags-mode 1)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                  (ggtags-mode 1))))

    (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
    (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
    (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
    (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
    (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
    (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
    (define-key ggtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key ggtags-mode-map (kbd "M-.") 'ggtags-find-tag-dwim)
    (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
    (define-key ggtags-mode-map (kbd "C-c <") 'ggtags-prev-mark)
    (define-key ggtags-mode-map (kbd "C-c >") 'ggtags-next-mark)
  ))

(provide 'setup-ggtags)
