;;; dtrt-indent.el --- Adapt to foreign indentation offsets

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

;;; Commentary:

;; A minor mode that guesses the indentation offset originally used
;; for creating source code files and transparently adjusts the
;; corresponding settings in Emacs, making it more convenient to edit
;; foreign files.
;;
;; This hooks into many major modes - c-mode, java-mode, shell-mode
;; and ruby-mode, to name but a few - and makes an educated guess on
;; which offset is appropriate by analyzing indentation levels in the
;; file.
;;
;; Heuristics are used to estimate the proper indentation offset and
;; therefore this system is not infallible, however adjustments will
;; only be made if the guess is considered reliable.  This way it
;; should leave you off no worse than before.
;;
;; To install,
;;   (require 'dtrt-indent)
;;   (dtrt-indent-mode 1)
;;
;; The default settings have been carefully chosen and tested to work
;; reliably on a wide range of source files.  However, if it doesn't
;; work for you they can be fine tuned using M-x customize-group
;; dtrt-indent
;;
;; There is more extensive information in the dtrt-indent info page
;; which you currently need to install manually.
;;
;; Improvements over guess-offset.el:
;;
;; - Whereas guess-offset only worked for C, C++ and Java files,
;;   dtrt-indent supports plenty of major modes (Shell script, Perl
;;   and Ruby are worth mentioning) and is easier to adapt to other
;;   languages.
;;
;; - dtrt-indent is now a minor mode and can be switched on and off,
;;   both globally and locally (the latter using a File Variable).
;;
;; - dtrt-indent is more precise in analyzing the syntax of source
;;   files, making its guess more accurate (it now ignores lines in
;;   comments, multi-line expressions, here documents and the like.)
;;
;; - dtrt-indent stops analyzing a source file after a customizable
;;   amount of lines, making it operate faster on large source files.
;;
;; - dtrt-indent leaves alone files that explicitly set the
;;   indentation offset with a File Variable.
;;
;; - dtrt-indent comes with diagnostic functions to help you
;;   understand what it does behind the scenes, adapt it to new
;;   languages or fine-tune its parameters.
;;
;; - The name of the script has been changed to better reflect its
;;   purpose.
;;
;; - The customization group is now a child of the convenience and
;;   files groups instead of the tools group.
;;
;; - The customization variables are named more sensibly and are
;;   better documented.
;;
;; - Documentation is improved and no longer confusingly refers to
;;   "tab width" instead of "indentation offset".
;;
;; Files not touched by dtrt-indent:
;;
;; - Files that specify the corresponding variable
;;   (e.g. c-basic-offset) as a File Variable.
;;
;; - Files that specify dtrt-indent-mode: 0 as a File Variable.
;;
;; - Files for which dtrt-indent-accept-file-function returns nil.
;;
;; - Files with a major mode that dtrt-indent doesn't hook into.
;;
;; - Files for which the indentation offset cannot be guessed
;;   reliably.
;;
;; Limitations:
;;
;; - dtrt-indent can't deal well with files that use variable
;;   indentation offsets, e.g. files that use varying indentation
;;   based on the outer construct.
;;
;; - dtrt-indent currently only supports a limited number of languages
;;   (major-modes).
;;
;; - dtrt-indent only guesses the indendation offset, not the
;;   indentation style.  For instance, it does not detect whether a
;;   C-like file uses hanging braces or not.
;;
;; - dtrt-indent can't deal well with files that mix hard tabs with
;; - spaces for indentation.
;;
;; TODO:
;;
;; - verbose and diagnostics messages
;; - make sure variable documentation match their function
;; - make sure defaults are sensible
;; - complete info page
;; - bulk (real world) tests
;; - functional tests
;; - unit tests

;;; Change log:

;; Revision 0.2.0 (2008-03-25)
;; Major rewrite
;; Name change from guess-offset.el to dtrt-indent.el
;;
;; Revision 0.1.2 (2007-02-02)
;; Minor documentation cleanups
;; Added link to cc-guess.el
;; Applied two patches courtesy of Michael Ernst <mernst@alum.mit.edu>:
;; (1) The problem is that you wrote
;;     (- 1 bracket-level)
;;     where you probably meant
;;     (- bracket-level 1)
;; (2) The documentation for `beginning-of-buffer' says
;;     Don't use this command in Lisp programs!
;;     (goto-char (point-min)) is faster and avoids clobbering the mark.
;;
;; Revision 0.1.1 (2003-??-??)
;; Initial version

;;; Code:

;;;###autoload
(define-minor-mode dtrt-indent-mode
  "Toggle dtrt-indent mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When dtrt-indent mode is enabled, the proper indentation
offset will be guessed for newly opened files and adjusted
transparently."
  :global t :group 'dtrt-indent)

(defvar dtrt-indent-language-syntax-table
  '((c/c++/java ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("[/][*]"                0   "[*][/]"   nil)
                ("[/][/]"                0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t))

    (perl       ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("[#]"                   0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t))

    (ruby       ("\""                    0   "\""       nil "\\.")
                ("'"                     0   "'"        nil "\\.")
                ("#"                     0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t)
                ("{"                     0   "}"        t))

    (ada        ("\""                    0   "\""       nil "\\.")
                ("--"                    0   "$"        nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t)
                ("{"                     0   "}"        t))

;;  python-mode comes with offset guessing
;;  (python     ("\"\"\""                0   "\"\"\""   nil "\\.")
;;              ("\""                    0   "\""       nil "\\.")
;;              ("'"                     0   "'"        nil "\\.")
;;              ("#"                     0   "$"        nil)
;;              ("("                     0   ")"        t)
;;              ("\\["                   0   "\\]"      t)
;;              ("{"                     0   "}"        t))

    ;; The standard Erlang style is to indent code inside a block
    ;; relative to the token that opened the block.  For example:
    ;;
    ;; bar(X) ->
    ;;   {A, B} = case X of
    ;;              true ->
    ;;                {alpha, [beta,
    ;;                         gamma]}
    ;;            end.
    ;;
    ;; Thus it is best to ignore the code inside these block
    ;; constructs when determining the indent offset.
    (erlang     ("\""                    0   "\""       nil "\\.")
                ("[<][<]\""              0   "\"[>][>]" nil)
                ("%"                     0   "$"        nil)
                ("{"                     0   "}"        t)
		("\\["                   0   "\\]"      t)
                ("("                     0   ")"        t)
		("\\b\\(begin\\|case\\|fun\\|if\\|receive\\|try\\)\\b"
                                         0   "\\bend\\b" t))

    (css        ("\""                    0   "\""       nil "\\\\.")
                ("'"                     0   "'"        nil "\\\\.")
                ("[/][*]"                0   "[*][/]"   nil))

    (shell      ("\""                    0   "\""       nil "\\.")
                ("'"                     0   "'"        nil "\\.")
                ("[<][<]\\\\?\\([^ \t]+\\)"   1   "^\\1"     nil)
                ("("                     0   ")"        t)
                ("\\["                   0   "\\]"      t)))

  "A list of syntax tables for supported languages.

Each entry in this list is of the form

   (SYMBOL SYNTAX-ENTRY [SYNTAX-ENTRY [...]])

where SYMBOL is used to identify the language in
question and SYNTAX-ENTRY is in format

   (BEGIN-REGEXP NUM-GROUPS END-REGEXP RECURSIVE-P SKIP-REGEXP)

BEGIN-REGEXP is a regular expression matching the beginning of
the syntax construct in question.  NUM-GROUPS indicates how many
groups there are in BEGIN-REGEXP to be substituted in END-REGEXP.

END-REGEXP is a regular expression matching the end of the syntax
construct in question.  It can refer back to one group in
BEGIN-REGEXP using \1. Currently only one group is supported (\2
cannot be used.)

RECURSIVE-P indicates whether other syntax constructs can be
nested within this construct.  RECURSIVE-P is usually false for
comment and constant constructs, such as strings, and usually
true for bracketing constructs.

SKIP-REGEXP is a regular expression that, if matches, means that
END-REGEXP is ignored for that location.  This can be used to
prevent an escaped quote from being interpreted as the closing
quote, for example.")

(defvar dtrt-indent-hook-mapping-list
;;   Mode            Hook                  Syntax        Variable
  '((c-mode          c/c++/java    c-basic-offset)       ; C
    (c++-mode        c/c++/java    c-basic-offset)       ; C++
    (java-mode       c/c++/java    c-basic-offset)       ; Java
    (jde-mode        c/c++/java    c-basic-offset)       ; Java (JDE)
    (js-mode         c/c++/java    js-indent-level)      ; JavaScript
    (objc-mode       c/c++/java    c-basic-offset)       ; Objective C
    (php-mode        c/c++/java    c-basic-offset)       ; PHP
    (perl-mode       perl          perl-indent-level)    ; Perl
;;  (python-mode     python        py-indent-offset)     ; Python
    (erlang-mode     erlang        erlang-indent-level)  ; Erlang
    (ruby-mode       ruby          ruby-indent-level)    ; Ruby
    (ada-mode        ada           ada-indent)           ; Ada
    (sh-mode         shell         sh-basic-offset)      ; Shell Script
    (css-mode        css           css-indent-offset)    ; CSS
    (pascal-mode     pascal        pascal-indent-level)) ; Pascal
   "A mapping from hook variables to language types.")

;;-----------------------------------------------------------------
;; Customization Definitions:

(defgroup dtrt-indent nil
  "Transparently adapt to foreign indentation offsets."
  :version "22.0"
  :group 'files
  :group 'convenience)

;;;###autoload
(defcustom dtrt-indent-mode nil
  "Toggle adaptive indentation mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `dtrt-indent-mode'."
  :set #'(lambda (symbol value) (funcall symbol (or value 0)))
  :initialize 'custom-initialize-default
  :version "22.0"
  :type    'boolean
  :group   'dtrt-indent
  :require 'dtrt-indent)

(defcustom dtrt-indent-verbosity 1
  "*Verbosity level.

How much dtrt-indent should tell you about what it's doing.  If
this is 1, the default, dtrt-indent will only let you know when
it adjusts the indentation offset but will be silent otherwise.
By setting this to 2 it will also tell you why it didn't adjust
the offset.  You might want to try this as a first measure if
you're unhappy with dtrt-indent's actions.  A setting of 3 will
output lots of diagnostic information.  Finally, a setting of 0
keeps dtrt-indent of ever outputting anything."
  :type '(choice (const :tag "Silent" 0)
                 (const :tag "Normal" 1)
                 (const :tag "Verbose" 2)
                 (const :tag "Diagnostics" 3))
  :tag "Verbosity"
  :group 'dtrt-indent)

(defcustom dtrt-indent-require-confirmation-flag nil
  "*Non-nil means to ask for confirmation before making adjustments.

Whether dtrt-indent asks for confirmation whenever it is about to
make any adjustments.  By default, adjustments are made without
your explicit consent because dtrt-indent is already quite
conservative and tries to 'do the right thing', adjustments can
be undone easily, and they aren't harmful in the first place.
However, if you feel like it's doing things behind your back
you should enable this setting."
  :type 'boolean
  :tag "Require Confirmation"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-relevant-lines 2
  "*Minimum number of relevant lines required for a guess to be made.

This check is in place because with a very low number of lines
that can be analyzed the chances of a wrong guess are rather
high because the sample size is so small.  If you are getting
false positives with small files - i.e. the wrong offset is guessed
- you might want to increase this setting.  On the other hand, if
you are getting false negatives on small files - i.e. no guess is
made on a small file - you might want to decrease it."
  :type 'integer
  :tag "Minimum Number Of Relevant Lines"
  :group 'dtrt-indent)

(defcustom dtrt-indent-max-relevant-lines 500
  "*Maximum number of relevant lines to be considered in analysis.

This setting is meant to prevent dtrt-indent from spending large
amounts of time on analyzing large source files.  In general, the
higher this setting, the more accurate the guess will be but the
more time dtrt-indent will consume when opening files.  If you
have a fast box you might want to consider increasing this
number.  On the other hand, if you find that dtrt-indent
introduces a noticable delay when opening files you might want
to decrease it."
  :type 'integer
  :tag "Maximum Number Of Relevant Lines"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-quality 80.0
  "*Minimum quality for an indentation offset to be accepted.

Percentage (0-100) of lines that are indented by a non-zero
amount of spaces divisible by a given offset required for that
offset to be eligible for guessing.  A value of 80 means that for
instance, an indentation level of 4 will only be guessed if at
least 80% of all lines are indented by an offset divisible by 4.

This setting essentially controls how lenient or conservative
dtrt-indent operates.  If you are getting false positives -
i.e. guess-offset guesses the wrong offset - you might want to
increase this setting.  On the other hand, if you are getting
false negatives - i.e. guess-offset refuses to adjust the offset
- you might want to decrease it."
  :type 'float
  :tag "Minimum Number Of Matching Lines"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-indent-superiority 100.0
  "*Minimum percentage the best guess needs to be better than second best.

The percentage (0-100, but higher values than 100 are possible)
that the number of lines matching the best guess must be higher
than the number of lines matching the second best guess in order
for dtrt-indent to adjust the offset.  For example, a value of
100 means that there must be twice as many lines matching the
best guess than the number of lines matching the second best
guess.

This check is in place to avoid a good guess to be accepted if
there is another, similarly good guess, because in that situation
there is ambiguity and no single reliable guess.  If you are
getting false positives - i.e. dtrt-indent guesses the wrong
offset - you might want to increase this setting.  On the other
hand, if you are getting false negatives - i.e. dtrt-indent
refuses to adjust the offset - you might want to decrease it."
  :type 'float
  :tag "Minimum Superiority Of Best Guess"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-soft-tab-superiority 300.0
  "*Minimum percentage soft-tab lines need to outnumber hard-tab ones.

TBD"
  :type 'float
  :tag "Minimum Superiority Of Soft Tabs"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-hard-tab-superiority 300.0
  "*Minimum percentage hard-tab lines need to outnumber soft-tab ones.

TBD"
  :type 'float
  :tag "Minimum Superiority Of Hard Tabs"
  :group 'dtrt-indent)

(defcustom dtrt-indent-max-merge-deviation 20.0
  "*Minimum difference between offsets divisible without remainder.

The percentage of difference in the number of lines that are
matched by two guessed offsets where the larger offset is
divisible by the smaller without remainder for the smaller offset
to be discarded.

This setting is in place because without it, in a file with 1000
lines matching an offset of 4, all of those 1000 lines are also
matching an offset of 2 and a number of stray lines whose offset
is divisible by 2 but not by 4 would confuse dtrt-indent to treat
the smaller offset as the better guess.  By discarding the
smaller offset with some tolerance, this problem is avoided.

If you notice that often, sub-offsets are wrongly guessed (e.g. 8
would be the proper offset but 4 is guessed, or 6 would be
correct but 3 is guessed) you might want to decrease this
setting.  On the other hand, if super-offsets are guessed (e.g. 4
would be appropriate, but 8 is guessed) you might want to
increase it."
  :type 'float
  :tag "Maximum Deviation For Sub-Offset Merging"
  :group 'dtrt-indent)

(defcustom dtrt-indent-ignore-single-chars-flag nil
  "*Non-nil means ignore lines containing only a single character.

Whether to treat lines that contain only a single non-whitespace
character as irrelevant for the analysis.  Set this to t in to
prevent hanging braces etc. from skewing the results.  Set it to
nil if you are dealing with source files whose indentation level
isn't reliably guessed without those lines."
  :type 'boolean
  :tag "Ignore Single-Character Lines"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-matching-indentations 1
  "*Minimum number of distinct levels for an offset to be eligible.

The minimum number of distinct, non-zero indentation levels
matching a given offset required to be present in a file for that
offset to be eligible for guessing.  A value of 2 means that for
instance, an indentation level of 4 will only be guessed if some
lines are indented at 4 spaces and some at 8; or if some lines
are indented at 12 spaces and some at 20; but not if some lines
are indented at 4 spaces but there are no other lines indented at
an offset divisible by 4.

The default value of 1 effectively disables any such requirement.
If you are getting false positives, you might want to set this to
a higher value such as 2.  However, a value of 2 means that the
offset won't be guessed for files containing only 'flat'
constructs"
  :type 'integer
  :tag "Minimum Depth"
  :group 'dtrt-indent)

(defcustom dtrt-indent-min-offset 2
  "*Minimum indentation offset that can be guessed.

You usually don't want to tinker with this - setting it to a
higher value than 2 means that the rather common offset of 2 will
no longer be guessed.  Setting it to 1 will likely screw up
dtrt-indent algorithms because every line in every source code
matches that indentation level."
  :type 'integer
  :tag "Minimum Guessed Indentation Offset"
  :group 'dtrt-indent)

(defcustom dtrt-indent-max-offset 8
  "*Maximum indentation offset that can be guessed.

You usually don't want to tinker with this - setting it to a
lower value than 8 means that the (unfortunately) rather common
indentation offset of 8 will no longer be guessed.  Setting it to
a higher value than 8 should not be harmful, but source files
using more than 8 spaces per indentation level are very rare."
  :type 'integer
  :tag "Maximum Guessed Indentation Offset"
  :group 'dtrt-indent)

(defcustom dtrt-indent-accept-file-function (lambda (filename) t)
  "*Acceptor determining which files are analyzed.

This function will be called for every file dtrt-indent would
normally analyze with one argument, the file name.  Only if it
returns a non-nil value analysis will be performed on the file.

By default, all files are analyzed."
  :type 'function
  :tag "Analysed File Inclusion Function"
  :group 'dtrt-indent)

(defvar dtrt-indent-original-indent)
(make-variable-buffer-local
 'dtrt-indent-original-indent)

(defvar dtrt-indent-mode-line-info)
(make-variable-buffer-local
 'dtrt-indent-mode-line-info)

(defvar dtrt-indent-explicit-offset)
(make-variable-buffer-local
 'dtrt-indent-explicit-offset)

(defvar dtrt-indent-explicit-tab-mode)
(make-variable-buffer-local
 'dtrt-indent-explicit-tab-mode)

(defun dtrt-indent--replace-in-string (haystack
                                        needle-regexp
                                        replacement)
  "Replace every match in string by constant replacement.
Returns HAYSTACK with every match of NEEDLE-REGEXP replaced by
REPLACEMENT."
  (if (string-match needle-regexp haystack)
      (concat (substring haystack 0 (match-beginning 0))
              replacement
              (substring haystack (match-end 0)))
    haystack))


(defun dtrt-indent--skip-to-end-of-match (end-regex
                                           skip-regex
                                           syntax-regex-pairs
                                           multi-line)
  "Place point at the end of the current match.
END-REGEX is a regular expression matching the end.  If
SKIP-REGEX matches though, END-REGEX is ignored.
SYNTAX-REGEX-PAIRS is a list of syntax entries as described for
`dtrt-indent-language-syntax-table'.  MULTI-LINE, if false,
constrains the search to the current line."
  (let* ((index-offset 1)
         end-index
         skip-index
         (regexp
          (mapconcat
           (lambda (el) (concat "\\(" el "\\)"))
           (append (when end-regex
                     (setq end-index index-offset)
                     (setq index-offset (1+ index-offset))
                     (list end-regex))
                   (when skip-regex
                     (setq skip-index index-offset)
                     (setq index-offset (1+ index-offset))
                     (list skip-regex))
                   (mapcar (lambda (x) (car x))
                           syntax-regex-pairs))
           "\\|")))
    (while
        (and
         (re-search-forward regexp
                            (unless multi-line
                              (save-excursion (end-of-line) (point))) t)
         (let ((match-index 1)
               (match-count (/ (length (match-data)) 2)))
           (while (and (<= match-index match-count)
                       (null (match-beginning match-index)))
             (setq match-index (1+ match-index)))
           (cond
            ((eq match-index end-index) nil)
            ((eq match-index skip-index) t)
            (t
             (let ((matching-syntax-entry
                    (let ((match-count (- match-index index-offset))
                          (syntax-regex-iterator syntax-regex-pairs))
                      (while (> match-count
                                (nth 1 (car syntax-regex-iterator)))
                        (setq match-count
                              (- match-count
                                 (nth 1 (car syntax-regex-iterator)) 1))
                        (setq syntax-regex-iterator
                              (cdr syntax-regex-iterator)))
                      (car syntax-regex-iterator))))
               (dtrt-indent--skip-to-end-of-match
                (if (> (nth 1 matching-syntax-entry) 0)
                    (dtrt-indent--replace-in-string
                     (nth 2 matching-syntax-entry)
                     "[\\][1]" (regexp-quote
				(match-string-no-properties
				 (1+ match-index))))
                  (nth 2 matching-syntax-entry))
                (nth 4 matching-syntax-entry)
                (when (nth 3 matching-syntax-entry) syntax-regex-pairs)
                t)
               t))))))))

(defun dtrt-indent--for-each-indentation (language func user-data)
  "Call a function for each indentation found.
LANGUAGE is used to lookup a syntax table for excluding lines
from the process.  For each line not excluded, FUNC is called
with USER-DATA as its argument and with point on the first
non-whitespace character of the line."
  (save-excursion
    (goto-char (point-min))
    (while (and (re-search-forward "^[ \t]*" nil t)
                (funcall func user-data)
                (progn
                  (dtrt-indent--skip-to-end-of-match
                   nil
                   nil
                   (cdr
                    (assoc language
                           dtrt-indent-language-syntax-table))
                   nil)
                  (beginning-of-line)
                  (let ((here (point)))
                    (forward-line)
                    (not (eq here (point)))))))))

(defun dtrt-indent--calc-histogram (language)
  "Calculate an indendation histogram.

The histogram is calculated for the current buffer using LANGUAGE
to determine which lines to exclude from the histogram."
  (let ((histogram (make-hash-table))
        (hard-tab-line-count 0)
        (soft-tab-line-count 0))

    (dtrt-indent--for-each-indentation
     language
     (lambda (histogram-and-count)
       (when (and (> (current-column) 0)
                  (not (looking-at "$"))
                  (or (not dtrt-indent-ignore-single-chars-flag)
                      (save-excursion
                        (forward-char)
                        (not (looking-at "[ \t]*$")))))
         (puthash (current-column)
                  (1+ (gethash (current-column)
                               (car histogram-and-count) 0))
                  (car histogram-and-count))
         (beginning-of-line)
         (if (looking-at "[\t]+")
             (setq hard-tab-line-count (1+ hard-tab-line-count))
           (setq soft-tab-line-count (1+ soft-tab-line-count)))
         (setcdr histogram-and-count (1+ (cdr histogram-and-count))))
       (< (cdr histogram-and-count)
          dtrt-indent-max-relevant-lines))
     (cons histogram 0))
    (let ((histogram-list '()) (total-lines 0))
      (maphash (lambda (key value)
                 (setq histogram-list (append histogram-list
                                              (list (list key value))))
                 (setq total-lines (+ total-lines value)))
               histogram)
      (list histogram-list
            total-lines
            hard-tab-line-count
            soft-tab-line-count))))

(defun dtrt-indent--analyze-histogram-try-offset (try-offset
                                                   histogram
                                                   total-lines)
  "Return match information for the given offset.
TRY-OFFSET is the offset to try, HISTOGRAM is the previously
calculated indentation histogram, TOTAL-LINES is the total number
of lines for which the histogram was calculated.

Returns a list in the format (TRY-OFFSET, PERCENTAGE,
MATCHING-INDENTATIONS, REJECT-REASON) where TRY-OFFSET is the
offset that was passed in as the first argument, PERCENTAGE is
the percentage of lines (0..1) with indentation levels that are a
multiple of TRY-OFFSET, MATCHING-INDENTATIONS is the number of
distinct indentation levels found that are a multiple of
TRY-OFFSET, and REJECT-REASON, if non-nil, is a string explaining
why TRY-OFFSET should be rejected."
  (let ((total-matching-lines 0)
        (matching-indentations 0))
    (dolist (histogram-entry histogram)
      (when (eq 0 (mod (nth 0 histogram-entry) try-offset))
        (setq total-matching-lines (+ total-matching-lines
                                      (nth 1 histogram-entry)))
        (setq matching-indentations (1+ matching-indentations))))
    (list try-offset
          (/ (float total-matching-lines) total-lines)
          matching-indentations
          (cond
           ((< matching-indentations
               dtrt-indent-min-matching-indentations)
            (format "\
rejected: too few distinct matching offsets (%d required)"
                    dtrt-indent-min-matching-indentations))
           (t
            nil)))))

(defun dtrt-indent--search-hook-mapping(mode)
  "Search hook-mapping for MODE or its derived-mode-parent."
  (if mode
      (or (assoc mode dtrt-indent-hook-mapping-list)
          (dtrt-indent--search-hook-mapping (get mode 'derived-mode-parent)))))

(defun dtrt-indent--analyze (histogram-and-total-lines)
  "Analyze the histogram.

HISTOGRAM-AND-TOTAL-LINES is a tuple with the first item being
the histogram, the second item being the total number of lines
considered in the histogram.

Returns a map with the following entries:

TBD"
  (let* ((analysis
          (let ((try-offset dtrt-indent-min-offset)
                unsorted-analysis)
            (while (<= try-offset dtrt-indent-max-offset)
              (setq
               unsorted-analysis
               (append unsorted-analysis
                       (list (dtrt-indent--analyze-histogram-try-offset
                              try-offset
                              (nth 0 histogram-and-total-lines)
                              (nth 1 histogram-and-total-lines)))))
              (setq try-offset (1+ try-offset)))
            (sort unsorted-analysis (lambda (x y) (> (nth 1 x)
                                                     (nth 1 y))))))
         (analysis-iterator analysis))

    (while analysis-iterator
      (let ((analysis-entry (car analysis-iterator)))
        (dolist (other-analysis-entry (cdr analysis-iterator))

          (let ((deviation (abs (- (nth 1 other-analysis-entry)
                                   (nth 1 analysis-entry)))))
            (when (and (not (nth 3 analysis-entry))
                       (eq 0 (mod (car other-analysis-entry)
                                  (car analysis-entry)))
                       (> dtrt-indent-max-merge-deviation
                          (* 100.0 deviation)))
              (setcdr
               (cddr analysis-entry)
               (list
                (format "\
merged with offset %s (%.2f%% deviation, limit %.2f%%)"
                        (nth 0 other-analysis-entry)
                        (* 100.0 deviation)
                        dtrt-indent-max-merge-deviation)))))))
      (setq analysis-iterator (cdr analysis-iterator)))

    (let (best-guess second-best-guess)
      (dolist (guess analysis)
        (cond
         ((and (null best-guess)
               (null (nth 3 guess)))
          (setq best-guess guess))
         ((and (null second-best-guess)
               (null (nth 3 guess)))
          (setq second-best-guess guess))))

      (let* ((confidence
      (if best-guess
          (- (nth 1 best-guess)
             (if second-best-guess
                 (* 2.0 (expt (/ (nth 1 second-best-guess) 2.0) 2))
               0))
        0))
             (total-lines (nth 1 histogram-and-total-lines))
             (hard-tab-percentage (if (> total-lines 0)
                                      (/ (float (nth 2 histogram-and-total-lines))
                                         total-lines)
                                    0))
             (soft-tab-percentage (if (> total-lines 0)
                                      (/ (float (nth 3 histogram-and-total-lines))
                                         total-lines)
                                    0))
             (change-indent-tabs-mode)
             (indent-tabs-mode-setting)
             (rejected
             (cond
              ((null best-guess)
               "no best guess")
              ((< (* 100.0 (nth 1 best-guess))
                  dtrt-indent-min-quality)
               (format "best guess below minimum quality (%f < %f)"
                       (* 100.0 (nth 1 best-guess))
                       dtrt-indent-min-quality))
              ((and second-best-guess
                    (< (- (/ (* 100.0 (nth 1 best-guess))
                             (nth 1 second-best-guess))
                          100)
                       dtrt-indent-min-indent-superiority))
               "best guess not much better than second best guess"))))

        (cond
         ((or (= 0 hard-tab-percentage)
              (>= (/ soft-tab-percentage
                     hard-tab-percentage)
                  (+ 1.0 (/ dtrt-indent-min-soft-tab-superiority 100.0))))
         (setq change-indent-tabs-mode t)
         (setq indent-tabs-mode-setting nil))

         ((or (= 0 soft-tab-percentage)
              (>= (/ hard-tab-percentage
                     soft-tab-percentage)
                  (+ 1.0 (/ dtrt-indent-min-hard-tab-superiority 100.0))))
         (setq change-indent-tabs-mode t)
         (setq indent-tabs-mode-setting t)))

        (list (cons :histogram (car histogram-and-total-lines))
              (cons :total-lines total-lines)
              (cons :analysis analysis)
              (cons :best-guess best-guess)
              (cons :second-best-guess second-best-guess)
              (cons :hard-tab-lines (nth 2 histogram-and-total-lines) )
              (cons :hard-tab-percentage hard-tab-percentage)
              (cons :soft-tab-lines (nth 3 histogram-and-total-lines) )
              (cons :soft-tab-percentage soft-tab-percentage)
              (cons :change-indent-tabs-mode change-indent-tabs-mode)
              (cons :indent-tabs-mode-setting indent-tabs-mode-setting)
              (cons :rejected rejected)
              (cons :confidence confidence))))))

(defun dtrt-indent-try-set-offset ()
  "Try adjusting the current buffer's indentation offset."
  (let ((language-and-variable (cdr (dtrt-indent--search-hook-mapping major-mode))))
    (when language-and-variable
      (let* ((result
              (dtrt-indent--analyze
               (dtrt-indent--calc-histogram
                (car language-and-variable))))
             (best-guess
              (cdr (assoc :best-guess result)))
             (rejected
              (cdr (assoc :rejected result)))
             (confidence
              (cdr (assoc :confidence result)))
             (change-indent-tabs-mode
              (cdr (assoc :change-indent-tabs-mode result)))
             (indent-tabs-mode-setting
              (cdr (assoc :indent-tabs-mode-setting result)))
             (best-indent-offset
              (nth 0 best-guess))
             (indent-offset-variable
              (nth 1 language-and-variable)))
        (cond
         ((and best-guess
               (not rejected)
               (or (not (eq (symbol-value indent-offset-variable)
                         best-indent-offset))
                   (not (eq indent-tabs-mode indent-tabs-mode-setting))))

          (if dtrt-indent-explicit-offset
              (message "\
Indentation offset set with file variable; not adjusted")
            (when (or (not dtrt-indent-require-confirmation-flag)
                      (yes-or-no-p
                       (format "Do you want to adjust %s to %s for buffer %s? "
                               indent-offset-variable
                               best-indent-offset
                               (buffer-name))))
              (setq dtrt-indent-original-indent
                    (list indent-offset-variable
                          (eval indent-offset-variable)
                          (local-variable-p indent-offset-variable)
                          indent-tabs-mode
                          (local-variable-p indent-tabs-mode)))
              (when (>= dtrt-indent-verbosity 1)
                (let ((offset-info
                       (format "%s adjusted to %s%s"
                               indent-offset-variable
                               best-indent-offset
                               (if (>= dtrt-indent-verbosity 2)
                                   (format " (%.0f%%%% confidence)"
                                           (* 100 confidence))
                                 "")))
                      (tabs-mode-info
                       (when (and change-indent-tabs-mode
                                  (not (eql indent-tabs-mode-setting
                                            indent-tabs-mode)))
                         (format " and indent-tabs-mode adjusted to %s"
                                 indent-tabs-mode-setting))))
                  (message (concat "Note: " offset-info tabs-mode-info))))
              (set (make-local-variable indent-offset-variable)
                   best-indent-offset)
              (when change-indent-tabs-mode
                (set (make-local-variable 'indent-tabs-mode)
                     indent-tabs-mode-setting))
              (setq dtrt-indent-mode-line-info "  [dtrt-indent adjusted]")
              best-indent-offset)))
         (t
          (when (>= dtrt-indent-verbosity 2)
            (message "Note: %s not adjusted" indent-offset-variable))
          nil))))))

(defun dtrt-indent-find-file-hook ()
  "Try adjusting indentation offset when a file is loaded."
  (when dtrt-indent-mode
    (dtrt-indent-try-set-offset)))

(defun dtrt-indent-adapt ()
  "Try adjusting indentation settings for the current buffer."
  (interactive)
  (if dtrt-indent-original-indent
      (message "dtrt-indent already adjusted this buffer")
    (dtrt-indent-try-set-offset)))

(defun dtrt-indent-undo ()
  "Undo any change dtrt-indent made to the indentation offset."
  (interactive)
  (if (null dtrt-indent-original-indent)
      (message "No dtrt-indent override to undo in this buffer")
    (let ((info
           (concat
            (if (nth 2 dtrt-indent-original-indent)
                (progn
                  (set (nth 0 dtrt-indent-original-indent)
                       (nth 1 dtrt-indent-original-indent))
                  (when (>= dtrt-indent-verbosity 1)
                    (format "\
Note: restored original buffer-local value of %d for %s"
                            (nth 1 dtrt-indent-original-indent)
                            (nth 0 dtrt-indent-original-indent))))
              (kill-local-variable (nth 0 dtrt-indent-original-indent))
              (format "\
Note: killed buffer-local value for %s, restoring to default %d"
                      (nth 0 dtrt-indent-original-indent)
                      (eval (nth 1 dtrt-indent-original-indent))))
            (if (nth 4 dtrt-indent-original-indent)
                (progn
                  (setq indent-tabs-mode
                        (nth 3 dtrt-indent-original-indent))
                  (format "\
 and restored original buffer-local value of %s for indent-tabs-mode"
                          (nth 3 dtrt-indent-original-indent)))
              (kill-local-variable 'indent-tabs-mode)
              (format "\
 and killed buffer-local value for indent-tabs-mode, restoring to default %s"
                      indent-tabs-mode)))))
      (when (>= dtrt-indent-verbosity 1)
        (message info))
      (kill-local-variable 'dtrt-indent-original-indent))))

;;-----------------------------------------------------------------
;; Installation

(defun dtrt-indent-unload-hook ()
  "Unload dtrt-indent."
  (dtrt-indent-mode 0))
(add-hook 'dtrt-indent-unload-hook 'dtrt-indent-unload-hook)

(defadvice hack-one-local-variable
  (before dtrt-indent-advise-hack-one-local-variable activate)
  "Adviced by dtrt-indent.

Disable dtrt-indent if offset explicitly set."
  (cond
   ((eql (nth 2 (dtrt-indent--search-hook-mapping major-mode))
         (ad-get-arg 0))
    (setq dtrt-indent-explicit-offset t))
   ((eql 'indent-tab-mode
         (ad-get-arg 0))
    (setq dtrt-indent-explicit-tab-mode t))))

; Install global find-file-hook
(add-hook 'find-file-hook 'dtrt-indent-find-file-hook)

; Customize mode line
(or global-mode-string (setq global-mode-string '("")))
(or (memq 'dtrt-indent-mode-line-info global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(dtrt-indent-mode-line-info))))

(autoload 'dtrt-indent-diagnosis "dtrt-indent-diag"
  "Guess indentation for the current buffer and output diagnostics."
  t)

(autoload 'dtrt-indent-highlight "dtrt-indent-diag"
  "Highlight non-excluded indentation in the current buffer."
  t)

(provide 'dtrt-indent)

;;; dtrt-indent.el ends here
