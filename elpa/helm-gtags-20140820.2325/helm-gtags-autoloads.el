;;; helm-gtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-gtags" "helm-gtags.el" (21494 43913 455053
;;;;;;  726000))
;;; Generated autoloads from helm-gtags.el

(autoload 'helm-gtags-clear-all-cache "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-clear-cache "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-next-history "helm-gtags" "\
Jump to next position on context stack

\(fn)" t nil)

(autoload 'helm-gtags-previous-history "helm-gtags" "\
Jump to previous position on context stack

\(fn)" t nil)

(autoload 'helm-gtags-select "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-select-path "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-find-tag "helm-gtags" "\
Jump to definition

\(fn)" t nil)

(autoload 'helm-gtags-find-tag-other-window "helm-gtags" "\
Jump to definition in other window.

\(fn)" t nil)

(autoload 'helm-gtags-find-rtag "helm-gtags" "\
Jump to referenced point

\(fn)" t nil)

(autoload 'helm-gtags-find-symbol "helm-gtags" "\
Jump to the symbol location

\(fn)" t nil)

(autoload 'helm-gtags-find-pattern "helm-gtags" "\
Jump to pattern

\(fn)" t nil)

(autoload 'helm-gtags-find-files "helm-gtags" "\
Find file with gnu global

\(fn)" t nil)

(autoload 'helm-gtags-find-tag-from-here "helm-gtags" "\
Find from here with gnu global

\(fn)" t nil)

(autoload 'helm-gtags-dwim "helm-gtags" "\
Find by context

\(fn)" t nil)

(autoload 'helm-gtags-parse-file "helm-gtags" "\
Find file with gnu global

\(fn)" t nil)

(autoload 'helm-gtags-pop-stack "helm-gtags" "\
Jump to previous point on the stack

\(fn)" t nil)

(autoload 'helm-gtags-show-stack "helm-gtags" "\
Show context stack

\(fn)" t nil)

(autoload 'helm-gtags-clear-stack "helm-gtags" "\
Clear jumped point stack

\(fn)" t nil)

(autoload 'helm-gtags-clear-all-stacks "helm-gtags" "\
Clear all jumped point stacks

\(fn)" t nil)

(autoload 'helm-gtags-update-tags "helm-gtags" "\
Update TAG file. Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'

\(fn)" t nil)

(autoload 'helm-gtags-resume "helm-gtags" "\
Resurrect previously invoked `helm-gtags` command.

\(fn)" t nil)

(autoload 'helm-gtags-mode "helm-gtags" "\
Toggle Helm-Gtags mode on or off.
With a prefix argument ARG, enable Helm-Gtags mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{helm-gtags-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-gtags-autoloads.el ends here
