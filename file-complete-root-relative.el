;;; file-complete-root-relative.el --- Completion style for files  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2019 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: completion
;; Version: 0
;; package-requires: ((emacs "25.0"))
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary

;; A file completion style in which the root directory is left out of
;; the completion string displayed to the user.
;;
;; Following the Design section in uniquify-files.el, this completion
;; style has the following string formats:
;;
;; - user: file name relative to a root directory
;;
;; - completion table input: same as user
;;
;; - data: absolute file name
;;
;; The completion style requires knowlege of the root directory;
;; currently, this requires use of a completion function to provide a
;; place to store it.

(require 'cl-lib)

(require 'uniquify-files);; FIXME: we share many low-level functions; factor them out.

(defun fc-root-rel--root (table)
  "Return root from TABLE."
  (cdr (assoc 'root (completion-metadata "" table nil))))

(defun fc-root-rel-to-table-input (user-string)
  "Implement `completion-to-table-input' for file-root-rel."
  user-string)

(defun fc-root-rel-to-data (user-string table _pred)
  "Implement `completion-get-data-string' for file-root-rel."
  ;; We assume USER-STRING is complete and unique.
  (let ((root (fc-root-rel--root table)))
    (concat root user-string)))

(defun fc-root-rel-to-user (data-string-list root)
  "Convert DATA-STRING-LIST to list of user format strings."
  ;; Assume they all start with ROOT
  (let ((prefix-length (1+ (length root)))) ;; don't include leading '/'
    (mapcar
     (lambda (abs-file-name)
       (substring abs-file-name prefix-length))
     data-string-list)
  ))

(defun fc-root-rel--pcm-merged-pat (string all point)
  "Return a pcm pattern that is the merged completion of STRING in ALL.
ALL must be a list of table input format strings?
Pattern is in reverse order."
  (let* ((case-fold-search completion-ignore-case)
	 (completion-pcm--delim-wild-regex
	  (concat "[" completion-pcm-word-delimiters "*]"))
	 (pattern (completion-pcm--string->pattern string point)))
    (completion-pcm--merge-completions all pattern)
    ))

(defun fc-root-rel-try-completion (string table pred point)
  "Implement `completion-try-completion' for file-root-rel."
  ;; Returns list of user format strings (uniquified file names), nil, or t.
  (let (result
	rel-all
	done)

    ;; Compute result, set done.
    (cond
     ((functionp table)
      (setq rel-all (fc-root-rel-all-completions string table pred point))

      (cond
       ((null rel-all) ;; No matches.
	(setq result nil)
	(setq done t))

       ((= 1 (length rel-all)) ;; One match; unique.
	(setq done t)

	;; Check for valid completion
	(if (string-equal string (car rel-all))
	    (setq result t)

	  (setq result (car rel-all))
	  (setq result (cons result (length result)))))

       (t ;; Multiple matches
	(setq done nil))
       ))

     ;; The following cases handle being called from
     ;; icomplete-completions with the result of `all-completions'
     ;; instead of the real table function. TABLE is a list of
     ;; relative file names.

     ((null table) ;; No matches.
      (setq result nil)
      (setq done t))

     (t
      (setq rel-all table)
      (setq done nil))
     )

    (if done
	result

      ;; Find merged completion of relative file names
      (let* ((merged-pat (fc-root-rel--pcm-merged-pat string rel-all point))

	     ;; `merged-pat' is in reverse order.  Place new point at:
	     (point-pat (or (memq 'point merged-pat) ;; the old point
			    (memq 'any   merged-pat) ;; a place where there's something to choose
			    (memq 'star  merged-pat) ;; ""
			    merged-pat))             ;; the end

	     ;; `merged-pat' does not contain 'point when the field
	     ;; containing 'point is fully completed.

	     (new-point (length (completion-pcm--pattern->string point-pat)))

	     ;; Compute this after `new-point' because `nreverse'
	     ;; changes `point-pat' by side effect.
	     (merged (completion-pcm--pattern->string (nreverse merged-pat))))

	(cons merged new-point)))
   ))

(defun fc-root-rel--hilit (string all point)
  "Apply face text properties to each element of ALL.
STRING is the current user input.
ALL is a list of strings in user format.
POINT is the position of point in STRING.
Returns new list.

Adds the face `completions-first-difference' to the first
character after each completion field."
  (let* ((merged-pat (nreverse (fc-root-rel--pcm-merged-pat string all point)))
	 (field-count 0)
	 (regex (completion-pcm--pattern->regex merged-pat '(any star any-delim point)))
	 )
    (dolist (x merged-pat)
      (when (not (stringp x))
	(setq field-count (1+ field-count))))

    (mapcar
     (lambda (str)
       (when (string-match regex str)
	 (cl-loop
	  for i from 1 to field-count
	  do
	  (when (and
		 (match-beginning i)
		 (<= (1+ (match-beginning i)) (length str)))
	    (put-text-property (match-beginning i) (1+ (match-beginning i)) 'face 'completions-first-difference str))
	  ))
       str)
     all)))

(defun fc-root-rel-all-completions (user-string table pred point)
  "Implement `completion-all-completions' for uniquify-file."
  ;; Returns list of data format strings (abs file names).

  (let* ((table-string (fc-root-rel-to-table-input user-string))
	 (all (funcall table table-string pred t)))

    (when all
      (setq all (fc-root-rel-to-user all (fc-root-rel--root table)))
      (fc-root-rel--hilit user-string all point))
    ))

(defun fc-root-rel--valid-completion (string all root)
  "Return non-nil if STRING is a valid completion in ALL,
else return nil.  ALL should be the result of `all-completions'.
STRING should be in completion table input format."
  (let* ((abs-string (concat root "/" string))
	 (matched nil)
	 name)

    (while (and all
		(not matched))
      (setq name (pop all))
      (when (string-equal abs-string name)
	(setq matched t)))

    matched))

(defun fc-root-rel--pcm-pattern-iter (string root)
  "Return pcm regexes constructed from STRING (a table format string)."
  ;; In file-name-all-completions, `completion-regexp-list', is
  ;; matched against file names and directories relative to `dir'.
  ;; Thus to handle partial completion delimiters in `string', we
  ;; construct two regexps from `string'; one from the directory
  ;; portion, and one from the non-directory portion.
  (let ((file-name (file-name-nondirectory string))
	(dir-name (directory-file-name (or (file-name-directory string) "")))
	dir-length)

    (setq dir-length (length dir-name))

    (when (and (< 0 (length file-name))
	       (= ?* (aref file-name 0)))
      (setq dir-name (concat dir-name "*")))

    ;; `completion-pcm--string->pattern' assumes its argument is
    ;; anchored at the beginning but not the end; that is true
    ;; for `dir-name' once we prepend ROOT.  file-name must match
    ;; a directory in "root/dir-name".
    (let* ((dir-pattern (completion-pcm--string->pattern dir-name))
	   (file-pattern (completion-pcm--string->pattern string))
	   (dir-regex
	    (cond
	     ((= 0 (length dir-name))
	      (if (= 0 (length file-name))
		  root
		(concat root
			"\\(\\'\\|/"
			(substring (completion-pcm--pattern->regex file-pattern) 2) ;; strip \`
			"\\)")))

	     ((string-equal "*" dir-name)
	      (if (or (= 0 dir-length)
		      (= 0 (length file-name)))
		  (concat root "/?")

		;; else STRING contains an explicit "/"
		(concat root "/")))

	     (t
	      (concat root
		      "/"
		      (substring (completion-pcm--pattern->regex dir-pattern) 2)
		      "\\("
		      (substring (completion-pcm--pattern->regex file-pattern) 2)
		      "\\)?"))
	     ))

	   ;; file-regex is matched against an absolute file name
	   (file-regex
	    (concat root
		    (if (eq 'star (nth 0 file-pattern)) "/?" "/")
		    (substring (completion-pcm--pattern->regex file-pattern) 2)))
	   )
      (list dir-regex file-regex))))

(defun fc-root-rel-completion-table-iter (path-iter string pred action)
  "Implement a completion table for file names in PATH-ITER.

PATH-ITER is a `path-iterator' object; it must have exacly one
recursive root, and no non-recursive roots.

STRING, PRED, ACTION are completion table arguments."

  ;; This completion table function combines iterating on files in
  ;; PATH-ITER with filtering on USER-STRING and PRED. This is an
  ;; optimization that minimizes storage use when USER-STRING is not
  ;; empty and PRED is non-nil.

  (cond
   ((eq (car-safe action) 'boundaries)
    ;; We don't use boundaries; return the default definition.
    (cons 'boundaries
	  (cons 0 (length (cdr action)))))

   ((eq action 'metadata)
    (cons 'metadata
	  (list
	   ;; We specify the category 'project-file here, to match the
	   ;; `completion-category-defaults' setting above.  We use
	   ;; the default sort order, which is shortest first, so
	   ;; "project.el" is easier to complete when it also matches
	   ;; "project-am.el".
	   '(category . project-file)
	   (cons 'root (car (path-iter-path-recursive-init path-iter))))))

   ((null action)
    ;; Called from `try-completion'; should never get here (see
    ;; `fc-root-rel-try-completion').
    nil)

   ((memq action
	  '(lambda ;; Called from `test-completion'
	     t))   ;; Called from all-completions

    ;; In file-name-all-completions, `completion-regexp-list', is
    ;; matched against file names and directories relative to `dir',
    ;; which is useless for this table.

    (pcase-let ((`(,dir-regex ,file-regex)
		 (fc-root-rel--pcm-pattern-iter string (car (path-iter-path-recursive-init path-iter)))))
      (let ((result nil)
	    (case-fold-search completion-ignore-case)
	    dir)

	(path-iter-restart path-iter)
	(while (setq dir (path-iter-next path-iter))
	  (when (string-match dir-regex dir)
	    (cl-mapc
	     (lambda (file-name)
	       (let ((absfile (concat (file-name-as-directory dir) file-name)))
		 (when (and (not (string-equal "." (substring absfile -1)))
			    (not (string-equal ".." (substring absfile -2)))
			    (not (file-directory-p absfile))
			    (string-match file-regex absfile)
			    (or (null pred)
				(funcall pred absfile)))
		   (push absfile result))))
	     (directory-files dir))
	    ))
	(cond
	 ((eq action 'lambda)
	  ;; Called from `test-completion'
	  (fc-root-rel--valid-completion string result (car (path-iter-path-recursive-init path-iter))))

	 ((eq action t)
	  ;; Called from all-completions
	  result)
	 ))
      ))
   ))

(defun fc-root-rel--pcm-pattern-list (string root)
  "Return pcm regex constructed from STRING (a table format string)."
  (let ((pattern (completion-pcm--string->pattern string)))
    (concat "\\`"
	    root
	    (when (< 0 (length string)) "/")
	    (substring (completion-pcm--pattern->regex pattern) 2);; trim \`
	    )))

(defun fc-root-rel-completion-table-list (file-list root string pred action)
  "Implement a completion table for file names in FILE-LIST,
with common prefix ROOT.

STRING, PRED, ACTION are completion table arguments."

  ;; This completion table function is required to provide access to
  ;; ROOT via metadata.

  (cond
   ((eq (car-safe action) 'boundaries)
    ;; We don't use boundaries; return the default definition.
    (cons 'boundaries
	  (cons 0 (length (cdr action)))))

   ((eq action 'metadata)
    (cons 'metadata
	  (list
	   ;; We specify the category 'project-file here, to match the
	   ;; `completion-category-defaults' setting above.  We use
	   ;; the default sort order, which is shortest first, so
	   ;; "project.el" is easier to complete when it also matches
	   ;; "project-am.el".
	   '(category . project-file)
	   (cons 'root root))))

   ((null action)
    ;; Called from `try-completion'; should never get here (see
    ;; `fc-root-rel-try-completion').
    nil)

   ((memq action
	  '(lambda ;; Called from `test-completion'
	     t))   ;; Called from all-completions

    (let ((regex (fc-root-rel--pcm-pattern-list string root))
	  (result nil)
	  (case-fold-search completion-ignore-case))

      (cl-mapc
       (lambda (absfile)
	 (when (and (string-match regex absfile)
		    (or (null pred)
			(funcall pred absfile)))
	   (push absfile result)))
       file-list)

      (cond
       ((eq action 'lambda)
	;; Called from `test-completion'
	(fc-root-rel--valid-completion string result root))

       ((eq action t)
	;; Called from all-completions
	result)
       )))
   ))

(provide 'file-complete-root-relative)
;;; file-complete-root-relative.el ends here
