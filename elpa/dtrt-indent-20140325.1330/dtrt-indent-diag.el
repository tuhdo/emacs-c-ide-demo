;;; dtrt-indent-diag.el --- Diagnostic functions for dtrt-indent.el

;; Copyright (C) 2003, 2007, 2008 Julian Scheid

;; Author: Julian Scheid <julians37@googlemail.com>
;; Version: 0.2.0
;; Keywords: convenience files languages c

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301 USA

(require 'dtrt-indent)

;;-----------------------------------------------------------------
;; Diagnostic functions

(defun dtrt-indent-diagnosis ()
  "Guess indentation for the current buffer and output diagnostics."
  (interactive)
  (require 'benchmark)
  (let ((language-and-variable
         (cdr (dtrt-indent--search-hook-mapping major-mode))))

    (with-output-to-temp-buffer "*dtrt-indent-debug*"
    (if (null language-and-variable)
        (princ (format "Major mode %s not supported by dtrt-indent" major-mode))
      (let* ((language-and-variable
              (cdr (dtrt-indent--search-hook-mapping major-mode)))
             result
             (time-for-analysis
              (benchmark-elapse
                (setq result
                      (dtrt-indent--analyze
                       (dtrt-indent--calc-histogram
                        (car language-and-variable))))))
             (histogram
              (cdr (assoc :histogram result)))
             (total-lines
              (cdr (assoc :total-lines result)))
             (hard-tab-lines
              (cdr (assoc :hard-tab-lines result)))
             (hard-tab-percentage
              (cdr (assoc :hard-tab-percentage result)))
             (soft-tab-lines
              (cdr (assoc :soft-tab-lines result)))
             (soft-tab-percentage
              (cdr (assoc :soft-tab-percentage result)))
             (change-indent-tabs-mode
              (cdr (assoc :change-indent-tabs-mode result)))
             (indent-tabs-mode-setting
              (cdr (assoc :indent-tabs-mode-setting result)))
             (analysis
              (cdr (assoc :analysis result)))
             (best-guess
              (cdr (assoc :best-guess result)))
             (second-best-guess
              (cdr (assoc :second-best-guess result)))
             (confidence
              (cdr (assoc :confidence result))))

        (princ (format "\nGuessing offset for %s\n\n"
                       (or (buffer-file-name) (buffer-name))))
        (princ (format "Elapsed time for analysis: %.3f seconds\n\n"
                       time-for-analysis))
        (princ (format "Total relevant lines: %d out of %d (limit: %d)\n"
                       total-lines
                       (line-number-at-pos (point-max))
                       dtrt-indent-max-relevant-lines))
        (if (< total-lines
               dtrt-indent-min-relevant-lines)
            (princ
             (format "\
\n\
Analysis cancelled: not enough relevant lines (%d required) - not \
modifying offset or indent-tabs-mode\n\n"
                     dtrt-indent-min-relevant-lines))
          (princ "\nHistogram:\n\n")
          (princ
           (eval
            (append '(concat)
                    (mapcar (lambda (x)
                              (format "  %4dx %3d spaces\n"
                                      (nth 1 x)
                                      (nth 0 x)))
                            (sort
                             histogram
                             (lambda (x y)
                               (or (not y)
                                   (and x (< (car x) (car y))))))))))
          (princ "\nAnalysis:\n\n")
          (princ
           (eval
            (append '(concat)
                    (mapcar
                     (lambda (analysis-entry)
                       (format "\
  offset %d works for %6.2f%% of relevant lines, matching %d \
distinct offsets - %s\n"
                               (nth 0 analysis-entry)
                               (* 100.0 (nth 1 analysis-entry))
                               (nth 2 analysis-entry)
                               (or (nth 3 analysis-entry) "CONSIDERED")))
                     analysis))))
          (princ "\nSummary:\n\n")

          (princ
           (format "\
  Best guess is offset %d with %.2f%% matching lines \(%.2f%% \
required)\n"
                   (nth 0 best-guess)
                   (* 100.0 (nth 1 best-guess))
                   dtrt-indent-min-quality))

          (if second-best-guess
              (progn
                (princ
                 (format "\
  Second best guess is offset %d with %.2f%% matching lines\n"
                         (nth 0 second-best-guess)
                         (* 100.0 (nth 1 second-best-guess))))
                (princ
                 (format "\
  Best guess is %.2f%% better than second best guess (%.2f%% \
required)\n"
                         (- (/ (* 100.0 (nth 1 best-guess))
                               (nth 1 second-best-guess)) 100)
                         dtrt-indent-min-indent-superiority)))
            (princ
             (format "  There is no second best guess\n")))

          (princ (format "  Hard tab percentage: %.2f%% (%d lines), \
%.2f%% superior to soft tabs (threshold %.2f%%)\n"
                         (* 100.0 hard-tab-percentage) 
                         hard-tab-lines 
                         (- (* 100.0 (/ hard-tab-percentage 
                                        soft-tab-percentage))
                            100.0)
                         dtrt-indent-min-hard-tab-superiority
                         ))
          (princ (format "  Soft tab percentage: %.2f%% (%d lines), \
%.2f%% superior to hard tabs (threshold %.2f%%)\n"
                         (* 100.0 soft-tab-percentage) 
                         soft-tab-lines
                         (- (* 100.0 (/ soft-tab-percentage 
                                        hard-tab-percentage))
                            100.0)
                         dtrt-indent-min-soft-tab-superiority))

          (princ "\nConclusion:\n\n")

          (princ (format "\
  Guessed offset %s with %.0f%% confidence.\n"
                         (nth 0 best-guess)
                         (* 100.0 confidence)))
          (princ (format "  Change indent-tab-setting: %s\n"
                         (if change-indent-tabs-mode
                             (format "yes, to %s" indent-tabs-mode-setting)
                           "no")))))))))


;; The following is from font-lock.el
(defmacro save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list t)
                      (inhibit-read-only t)
                      (inhibit-point-motion-hooks t)
                      (inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

(defun dtrt-indent-highlight ()
  "Highlight non-excluded indentation in the current buffer."
  (interactive)
  (let ((language-and-variable
         (cdr (dtrt-indent--search-hook-mapping major-mode))))
    (if (null language-and-variable)
        (message "Major mode %s not supported by dtrt-indent" major-mode)
      (save-buffer-state nil
        (dtrt-indent--for-each-indentation
         (car language-and-variable)
         (lambda (histogram)
           (put-text-property (save-excursion (beginning-of-line) (point))
                              (point)
                              'face '(background-color . "red"))
           t)
         nil)))))

;;; dtrt-indent-diag.el ends here
