

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


(provide 'setup-bugreport)
