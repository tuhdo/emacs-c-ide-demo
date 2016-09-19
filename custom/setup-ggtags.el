(require 'ggtags)

(ggtags-mode 1)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(dolist (map (list ggtags-mode-map dired-mode-map))
  (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key map (kbd "C-c g f") 'ggtags-find-file)
  (define-key map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key map (kbd "M-.") 'ggtags-find-tag-dwim)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map (kbd "C-c <") 'ggtags-prev-mark)
  (define-key map (kbd "C-c >") 'ggtags-next-mark))

(provide 'setup-ggtags)
