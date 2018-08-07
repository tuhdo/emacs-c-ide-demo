(use-package anaconda-mode)

(add-hook 'python-mode-hook
          (lambda ()
			(anaconda-mode t)
            (set (make-local-variable 'company-backends)
				 '((company-anaconda company-dabbrev)))
			))

(provide 'setup-python)
