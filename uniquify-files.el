;;; uniquify-files.el --- Completion style for files in a path  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2017, 2019  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: completion table
;;   uniquify
;; Version: 0
;; package-requires: ((emacs "25.0") (path-iterator "1.0"))
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


;;; Discussion
;;;
;; These are the driving requirements for this completion style:
;;
;; 1. Allow the strings entered by the user and displayed in the
;;    completion list to be rearranged abbreviations of the absolute
;;    file name returned by `completing-read'.
;;
;; 2. Allow partial completion on the directory and filename portions
;;    of the abbreviated strings.
;;
;;    "partial completion" means file names are partitioned at "_-/"
;;    characters, so "fo-ba" completes to "foo-bar".
;;
;; 3. The style should be usable with the completion table function
;;    provided here, or with a list of absolute file names.

;; Requirement 1 has the most effect on the design. There are two
;; common ways to select the result of a completion:
;;
;; - `minibuffer-complete-and-exit' - by default bound to <ret> in the
;;   minibuffer when `icomplete-mode' is enabled.
;;
;; - `minibuffer-force-complete-and-exit' - some users bind this to
;;    <ret> or other keys, so that it is easier to select the first
;;    completion.
;;
;; One possible design is to have `completion-try-completion' return
;; an absolute file name (rather than an abbreviated file name) when
;; the completed string is a valid completion. That sometimes works
;; with `minibuffer-complete-and-exit', but it does not work with
;; `minibuffer-force-complete-and-exit'; details follow.

;; The nominal path thru `minibuffer-complete-and-exit' in effect
;; calls `test-completion'. If that returns nil, it calls
;; `completion-try-completion' with the same string, and then
;; `test-completion' on that result. If that returns non-nil, the
;; completed string is returned as the result of
;; `completing-read'. Thus `test-completion' could return nil for user
;; format strings, and t for data format strings; and `try-completion'
;; could convert user format strings that are valid completions to data
;; format strings. However, the full logic is complex (see the code in
;; minibuffer.el for more details), and often ends up not converting
;; the user string to a data string.
;;
;; `minibuffer-force-complete-and-exit' calls
;; `minibuffer-force-complete', which replaces the buffer text with
;; the first completion. Then it calls `test-completion', but _not_
;; `try-completion' if that fails. So there is no opportunity to
;; convert the user string to a data string.
;;
;; Thus the design we use here adds an explicit conversion from user
;; to data format, via advice on completing-read.
;;
;; We did not meet the third requirement; the completion table
;; implements part of the completion style.

;;; Design
;;
;; There are three string formats involved in completion. For most
;; styles, they are all the same; the following table describes them
;; for the uniquify-file style.
;;
;; - user
;;
;;   The format typed by the user in the minibuffer, and shown in the
;;   displayed completion list.
;;
;;   The user input is passed to `completion-try-completion', so it must
;;   accept this format.
;;
;;   The string returned by `completion-try-completion' when it extends
;;   the string replaces the string typed by the user, so it must be
;;   in this format.
;;
;;   The text displayed by `completing-read' consists of the current
;;   input string followed by a completion list.  The completion list
;;   consists of the strings returned by `completion-all-completions'
;;   with the common prefix deleted (the common prefix is in the
;;   completion string); `completion-all-completions' must return
;;   strings in this format.
;;
;;   When the user selects a displayed completion, the string is
;;   passed to `test-completion'; it must accept strings in this format
;;   and return t.
;;
;;   For the uniquify-file style, this is a partial or complete file
;;   base name with any required uniquifying directories appended.
;;
;; - completion table input
;;
;;   The string input to the completion table function, or, if the
;;   table is a list of absolute filenames, the string matched against
;;   the table.
;;
;;   The `completion-try-completion' and `completion-all-completion'
;;   `test-completion' functions must convert user format strings to
;;   completion table input format strings when calling the
;;   corresponding low-level completion functions that call the
;;   completion table function.
;;
;;   For the uniquify-file style, this contains the complete or
;;   partial directory name or no directory name, followed by the
;;   partial or complete file base name, in normal elisp filename
;;   format.
;;
;;   A completion table input string is a valid completion if the
;;   string equals (respecting `completion-ignore-case') the tail of
;;   an existing file name, starting after a directory separator and
;;   ending at the end of the file name.
;;
;; - data
;;
;;   The string format desired as the result of `completing-read'.
;;
;;   In order to keep style-dependent code out of the completion table
;;   function, the completion table function returns a list of strings
;;   in this format when action is t; `completion-all-completions'
;;   converts them to user format strings.
;;
;;   For the uniquify-file style, this is an absolute file name.
;;
;;
;; As of Emacs 25.1, `completion-try-completion' and
;; `completion-all-completion' support style-specific implementations
;; via `completion-style-alist', but `test-completion' does not. So we
;; advise `test-completion' to convert to the appropriate format first.
;;
;; Similarly, the current completion code does not have a provision
;; for converting from user format to data format after a completion
;; is selected; we add that via advice on `completing-read-default'. A
;; future version may add this conversion in
;; `completion--complete-and-exit' instead.
;;
;; In order to allow other completion styles that have different user
;; and data string formats, we extend `completion-styles-alist' with
;; two entries:
;;
;; - fourth entry contains a function that takes one argument
;; USER-STRING and returns a table input format string. This is used
;; by `completion-to-table-input' - advice for `test-completion'.
;;
;; - fifth entry contains a function that takes three arguments
;; USER-STRING, TABLE, PREDICATE, and returns a list of data string
;; format strings matching USER-STRING. This is used by
;; `completion-get-data-string'.
;;

(require 'cl-lib)
(require 'path-iterator)

(defconst uniq-files--regexp "^\\(.*\\)<\\([^>]*\\)>?$"
  ;; The trailing '>' is optional so the user can type "<dir" in the
  ;; input buffer to complete directories.
  "Regexp matching uniqufied file name.
Match 1 is the filename, match 2 is the relative directory.")

(defun uniq-file--dir-match (partial abs)
  "Return the portion of ABS that matches PARTIAL; both are directories."
  (cond
   ((and partial
	 (< 0 (length partial)))
    (let* ((pattern (completion-pcm--string->pattern partial nil))
	   (regex (completion-pcm--pattern->regex pattern)))

      ;; `regex' is anchored at the beginning; delete the anchor to
      ;; match a directory in the middle of ABS.  Also extend
      ;; the match to the bounding '/'.
      (setq regex (substring regex 2))
      (unless (= ?/ (aref regex 0))
	(setq regex (concat "/" regex)))
      (unless (= ?/ (aref regex (1- (length regex))))
	(setq regex (concat regex "[^/]*/" )))

      (when (string-match regex abs);; Should never fail, but gives obscure error if it does
	;; Drop the leading '/'
	(substring (match-string 0 abs) 1))
      ))

   (t
    ;; no partial; nothing matches
    "")
   ))

(defun uniq-files--conflicts (conflicts dir)
  "Subroutine of `uniq-files-uniquify'."
  (let ((common-root ;; shared prefix of dirs in conflicts - may be nil
	 (fill-common-string-prefix (file-name-directory (nth 0 conflicts)) (file-name-directory (nth 1 conflicts)))))

    (let ((temp (cddr conflicts)))
      (while (and common-root
		  temp)
	(setq common-root (fill-common-string-prefix common-root (file-name-directory (pop temp))))))

    (when common-root
      ;; Trim `common-root' back to last '/'
      (let ((i (1- (length common-root))))
	(while (and (> i 0)
		    (not (= (aref common-root i) ?/)))
	  (setq i (1- i)))
	(setq common-root (substring common-root 0 (1+ i)))))

    (cl-mapcar
     (lambda (name)
       ;; The set of `non-common' is unique, but we also need to
       ;; include all of `completed-dir' in the result.
       ;;
       ;; examples
       ;;   1. uniquify-files-test.el test-uniq-file-uniquify, dir "Al/a-"
       ;;      conflicts:
       ;;         .../Alice/alice-1/bar-file1.text
       ;;         .../Alice/alice-1/bar-file2.text
       ;;         .../Alice/alice-2/bar-file2.text
       ;;      common        : .../Alice/
       ;;      non-common    : alice-1/, alice-2/
       ;;      completed-dir : Alice/alice-1/, Alice/alice-2/
       ;;
       ;;   2. uniquify-files-test.el test-uniq-file-all-completions-noface-1 "f-file4.text<a-3"
       ;;      conflicts:
       ;;         .../uniquify-files-resources/Alice/alice-3/foo-file4.text
       ;;         .../uniquify-files-resources/Bob/alice-3/foo-file4.text
       ;;      common        : .../uniquify-files-resources
       ;;      non-common    : Alice/alice-3/, Bob/alice-3/
       ;;      completed-dir : alice-3/
       ;;
       (let ((completed-dir (and dir (uniq-file--dir-match dir (file-name-directory name))))
	     (non-common (substring (file-name-directory name) (length common-root))))

	 (when (and completed-dir
		    (not (string-match completed-dir non-common)))
	   ;; case 1.
	   (let* ((completed-dirs (and completed-dir (nreverse (split-string completed-dir "/" t))))
		  (first-non-common (substring non-common 0 (string-match "/" non-common))))
	     (while completed-dirs
	       (let ((dir1 (pop completed-dirs)))
		 (when (not (string-equal dir1 first-non-common))
		   (setq non-common (concat dir1 "/" non-common)))))))
	 ;; else case 2; non-common is correct

	 (concat (file-name-nondirectory name) "<" non-common ">")
	 ))
     conflicts)
    ))

(defun uniq-file--uniquify (names dir)
  "Return a uniquified list of names built from NAMES.
NAMES contains absolute file names.

The result contains non-directory filenames with partial
directory paths appended.  The partial directory path will always
include at least the completion of DIR.

If DIR is non-nil, all elements of NAMES must match DIR."
  ;;  AKA uniq-file-to-user; convert list of data format strings to list of user format strings.
  (let ((case-fold-search completion-ignore-case))
    (when names
      (let (result
	    conflicts ;; list of names where all non-directory names are the same.
	    )

	;; Sort names on basename so duplicates are grouped together
	(setq names (sort names (lambda (a b)
				  (string< (file-name-nondirectory a) (file-name-nondirectory b)))))

	(while names
	  (setq conflicts (list (pop names)))
	  (while (and names
		      (string= (file-name-nondirectory (car conflicts)) (file-name-nondirectory (car names))))
	    (push (pop names) conflicts))

	  (if (= 1 (length conflicts))
	      (let ((completed-dir (and dir (uniq-file--dir-match dir (file-name-directory (car conflicts))))))
		(push
		 (if completed-dir
		     (concat (file-name-nondirectory (car conflicts)) "<" completed-dir ">")

		   (concat (file-name-nondirectory (car conflicts))))
		 result))

	    (setq result (append (uniq-files--conflicts conflicts dir) result)))
	  )
	(nreverse result)
	))
    ))

(defun uniq-file-to-table-input (user-string &optional _table _pred)
  "Implement `completion-to-table-input' for uniquify-file."
  (let* ((match (string-match uniq-files--regexp user-string))
	 (dir (and match (match-string 2 user-string))))

    (if match
	(if (= 0 (length dir)) ;; ie "file<"
	    (match-string 1 user-string)
	  (concat (file-name-as-directory dir) (match-string 1 user-string)))

      ;; else not uniquified
      user-string)))

(defun uniq-file--valid-regexp (string)
  "Return a regexp matching STRING (in table input format) to an absolute file name.
Regexp matches if the file name is a valid completion."
  (concat (unless (file-name-absolute-p string) "/") string "\\'"))

(defun uniq-file--valid-completion (string all)
  "Return non-nil if STRING is a valid completion in ALL,
else return nil.  ALL should be the result of `all-completions'.
STRING should be in completion table input format."
  ;; STRING is a valid completion if it is a tail of at least one
  ;; element of ALL, including at least the base name.
  (let* ((regexp (uniq-file--valid-regexp string))
	 (matched nil)
	 name)

    (while (and all
		(not matched))
      (setq name (pop all))
      (when (string-match regexp name)
	(setq matched t)))

    matched))

(defun uniq-file--pcm-pattern (string)
  "Return pcm regexes constructed from STRING (a table format string)."
  ;; In file-name-all-completions, `completion-regexp-list', is
  ;; matched against file names and directories relative to `dir'.
  ;; Thus to handle partial completion delimiters in `string', we
  ;; construct two regexps from `string'; one from the directory
  ;; portion, and one from the non-directory portion.
  (let* ((dir-name (directory-file-name (or (file-name-directory string) "")))
	 (file-name (file-name-nondirectory string))

	 ;; `completion-pcm--string->pattern' assumes its argument
	 ;; is anchored at the beginning but not the end; that is
	 ;; true for `dir-name' only if it is absolute.
	 (dir-pattern (completion-pcm--string->pattern
		       (if (file-name-absolute-p dir-name) dir-name (concat "*/" dir-name))))
	 (dir-regex (completion-pcm--pattern->regex dir-pattern))

	 (file-pattern (completion-pcm--string->pattern file-name))
	 (file-regex (completion-pcm--pattern->regex file-pattern)))
    (list dir-regex file-regex)))

(defun uniq-file--pcm-merged-pat (string all point)
  "Return a pcm pattern that is the merged completion of STRING in ALL.
ALL must be a list of table input format strings?
Pattern is in reverse order."
  (let* ((case-fold-search completion-ignore-case)
	 (completion-pcm--delim-wild-regex
	  (concat "[" completion-pcm-word-delimiters "<>*]"))
	 ;; If STRING ends in an empty directory part, some valid
	 ;; completions won't have any directory part.
	 (trimmed-string
	  (if (and (< 0 (length string))
		   (= (aref string (1- (length string))) ?<))
	      (substring string 0 -1)
	    string))
	 dir-start
	 (pattern (completion-pcm--string->pattern trimmed-string point)))

    ;; If trimmed-string has a directory part, allow uniquifying
    ;; directories.
    (when (and (setq dir-start (string-match "<" trimmed-string))
	       (< dir-start (1- (length trimmed-string))))
      (let (new-pattern
	    item)
	(while pattern
	  (setq item (pop pattern))
	  (push item new-pattern)
	  (when (equal item "<")
	    (setq item (pop pattern))
	    (if (eq item 'any-delim)
		(push 'any new-pattern)
	      (push item new-pattern))))
	(setq pattern (nreverse new-pattern))))

    (completion-pcm--merge-completions all pattern)))

(defun uniq-file-try-completion (user-string table pred point)
  "Implement `completion-try-completion' for uniquify-file."
  ;; Returns common leading substring of completions of USER-STRING in table,
  ;; consed with new point (length of common substring).
  (let (result
	uniq-all
	done)

    ;; Compute result or uniq-all, set done.
    (cond
     ((or
       (functionp table) ;; TABLE is a wrapper function that calls uniq-file-completion-table.
       (and (consp table)
	    (file-name-absolute-p (car table)))) ;; TABLE is the original list of absolute file names.

      (setq uniq-all (uniq-file-all-completions user-string table pred point))

      (cond
       ((null uniq-all) ;; No matches.
	(setq result nil)
	(setq done t))

       ((= 1 (length uniq-all)) ;; One match; unique.
	(setq done t)

	;; Check for valid completion
	(if (string-equal user-string (car uniq-all))
	    (setq result t)

	  (setq result (car uniq-all))
	  (setq result (cons result (length result)))))

       (t ;; Multiple matches
	(setq done nil))
       ))

     ;; The following cases handle being called from
     ;; icomplete-completions with the result of `all-completions'
     ;; instead of the real table function. TABLE is a list of
     ;; uniquified file names.

     ((null table) ;; No matches.
      (setq result nil)
      (setq done t))

     (t ;; TABLE is a list of uniquified file names
      (setq uniq-all table)
      (setq done nil))
     )

    (if done
	result

      ;; Find merged completion of uniqified file names
      (let* ((merged-pat (uniq-file--pcm-merged-pat user-string uniq-all point))

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

(defun uniq-files--hilit (string all point)
  "Apply face text properties to each element of ALL.
STRING is the current user input.
ALL is a list of strings in user format.
POINT is the position of point in STRING.
Returns new list.

Adds the face `completions-first-difference' to the first
character after each completion field."
  (let* ((merged-pat (nreverse (uniq-file--pcm-merged-pat string all point)))
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

(defun uniq-file--match-list (regexp-list file-name)
  "Return non-nil if FILE-NAME matches all regular expressions in REGEXP-LIST,
nil otherwise."
  (let ((result t))
    (dolist (regexp regexp-list)
      (unless (string-match regexp file-name)
	(setq result nil)))
    result))

(defun uniq-file-all-completions (user-string table pred point)
  "Implement `completion-all-completions' for uniquify-file."
  ;; Returns list of data format strings (abs file names).

  (let ((table-string (uniq-file-to-table-input user-string))
	all)

    (cond
     ((functionp table)
      (setq all (funcall table table-string pred t)))

     ((and (consp table)
	   (file-name-absolute-p (car table)))
      ;; TABLE is the original list of absolute file names.

      (pcase-let ((`(,dir-regex ,file-regex)
		   (uniq-file--pcm-pattern table-string)))
	(let ((completion-regexp-list (cons file-regex completion-regexp-list))
	      (case-fold-search completion-ignore-case))
	  (dolist (file-name table)
	    (when (and
		   (string-match dir-regex (directory-file-name file-name))
		   (uniq-file--match-list completion-regexp-list (file-name-nondirectory file-name))
		   (or (null pred)
		       (funcall pred file-name)))
	      (push file-name all)))
	  )))
     )

    (when all
      (setq all (uniq-file--uniquify all (file-name-directory table-string)))
      (uniq-files--hilit user-string all point))
    ))

(defun uniq-file-get-data-string (user-string table pred)
  "Implement `completion-get-data-string' for 'uniqify-file."
  ;; We assume USER-STRING is complete, but it may not be unique, in
  ;; both the file name and the directory; shortest completion of each
  ;; portion is the correct one.
  (let ((table-string (uniq-file-to-table-input user-string))
	 all)
    (cond
     ((functionp table)
      (setq all (all-completions table-string table pred)))

     (t
      ;; TABLE is list of absolute file names. Match table-string
      ;; against tail of table entry.
      (let ((regexp (uniq-file--valid-regexp table-string)))
	(dolist (entry table)
	  (when (string-match regexp entry)
	    (push entry all)))
	))
     )

    (setq
     all
     (sort all
	   (lambda (a b)
	     (let ((lfa (length (file-name-nondirectory a)))
		   (lfb (length (file-name-nondirectory b))))
	       (if (= lfa lfb)
		   (< (length a) (length b))
		 (< lfa lfb))
	       ))
	   ))

    (or (car all)
        "");; must return a string, not nil.
    ))

(defun completion-get-data-string (user-string table pred)
  "Return the data string corresponding to USER-STRING."
  (let* ((styles
	  (or (cdr (assq 'styles (completion-metadata user-string table pred)))
	      (completion--styles (completion-metadata user-string table pred))))

	 (results
	  ;;  FIXME: This is ultimately called from
	  ;;  `completion-try-completion' or `completion-all-completions';
	  ;;  there is only one style currently being used. Need to pass that
	  ;;  style from there to here.
	  (mapcar (lambda (style)
		    (let ((to-data-func (nth 5 (assq style completion-styles-alist))))
		      (if to-data-func
			  (funcall to-data-func user-string table pred)
			user-string)))
		  styles))
	 )
    (car (delete-dups results))
    ))

(defun completion-to-table-input (orig-fun user-string table &optional pred)
  "Advice for `test-completion'; convert user string to table input."
  ;; See FIXME: in completion-get-data-string
  (let* ((styles
	  (or (cdr (assq 'styles (completion-metadata user-string table pred)))
	      (completion--styles (completion-metadata user-string table pred))))
	 (table-strings
	  (mapcar
	   (lambda (style)
	     (let ((to-table-func (if (functionp table)
				      (nth 4 (assq style completion-styles-alist)) ;; user to table

				    ;; TABLE is a list of absolute file names
				    (nth 5 (assq style completion-styles-alist)) ;; user to data
				    )))
	       (if to-table-func
		   (funcall to-table-func user-string table pred)
		 user-string)))
	   styles)))
    (setq table-strings (delete-dups table-strings))
    (funcall orig-fun (car table-strings) table pred)
    ))

(advice-add #'test-completion :around #'completion-to-table-input)

(defun uniq-file-completing-read-default-advice (orig-fun prompt collection &optional predicate
							  require-match initial-input hist def
							  inherit-input-method)
  "Advice for `completing-read-default'; convert user string to data string."
  (let ((user-string (funcall orig-fun prompt collection
			      predicate require-match initial-input hist def
			      inherit-input-method)))
    (completion-get-data-string user-string collection predicate)
    ))

(advice-add #'completing-read-default :around #'uniq-file-completing-read-default-advice)

(add-to-list 'completion-styles-alist
	     '(uniquify-file
	       uniq-file-try-completion
	       uniq-file-all-completions
	       "display uniquified filenames."
	       uniq-file-to-table-input    ;; 4 user to table input format
	       uniq-file-get-data-string)) ;; 5 user to data format

(defun uniq-file-completion-table (path-iter string pred action)
  "Implement a completion table for file names in PATH-ITER.

PATH-ITER is a `path-iterator' object. It will be restarted for
each call to `uniq-file-completion-table'.

STRING, PRED, ACTION are completion table arguments:

STRING is the entire current user input, which is expected to be
a non-directory file name, plus enough directory portions to
identify a unique file.  `*' is treated as a wildcard, as in a
shell glob pattern.

If PRED is nil, it is ignored. If non-nil, it must be a function
that takes one argument; the absolute file name.  The file name
is included in the result if PRED returns non-nil. In either
case, `completion-ignored-extensions', `completion-regexp-list',
`completion-ignore-case' are used as described in
`file-name-all-completions'.

ACTION is the current completion action; one of:

- nil; return common prefix of all completions of STRING, nil or
  t; see `try-completion'. This table always returns nil.

- t; return all completions; see `all-completions'

- lambda; return non-nil if string is a valid completion; see
  `test-completion'.

- '(boundaries . SUFFIX); return the completion region
  '(boundaries START . END) within STRING; see
  `completion-boundaries'.

- 'metadata; return (metadata . ALIST) as defined by
  `completion-metadata'.

Return a list of absolute file names matching STRING."

  ;; This completion table function combines iterating on files in
  ;; PATH-ITER with filtering on USER-STRING and PRED. This is an
  ;; optimization that minimizes storage use when USER-STRING is not
  ;; empty and PRED is non-nil.

  ;; We don't use cl-assert on the path here, because that would be
  ;; called more often than necessary, and because throwing an error
  ;; from inside completing-read and/or icomplete is not helpful.

  (cond
   ((eq (car-safe action) 'boundaries)
    ;; We don't use boundaries; return the default definition.
    (cons 'boundaries
	  (cons 0 (length (cdr action)))))

   ((eq action 'metadata)
    (cons 'metadata
	  (list
	   '(category . project-file)
	   '(styles . (uniquify-file))
	   )))

   ((null action)
    ;; Called from `try-completion'; should never get here (see
    ;; `uniq-file-try-completion').
    nil)

   ((memq action
	  '(lambda ;; Called from `test-completion'
	     t))   ;; Called from all-completions

    ;; In file-name-all-completions, `completion-regexp-list', is
    ;; matched against file names and directories relative to `dir'.
    ;; Thus to handle partial completion delimiters in `string', we
    ;; construct two regexps from `string'; one from the directory
    ;; portion, and one from the non-directory portion.  We use the
    ;; directory regexp here, and pass the non-directory regexp to
    ;; `file-name-all-completions' via `completion-regexp-list'.  The
    ;; `string' input to `file-name-all-completions' is redundant with
    ;; the regexp, so we always build a regexp, and pass an empty
    ;; string.

    (pcase-let ((`(,dir-regex ,file-regex)
		 (uniq-file--pcm-pattern string)))
      (let (;; A project that deals only with C files might set
	    ;; `completion-regexp-list' to match only *.c, *.h, so we
	    ;; preserve that here.
	    (completion-regexp-list (cons file-regex completion-regexp-list))
	    (result nil))

	(path-iter-restart path-iter)

	(let ((case-fold-search completion-ignore-case)
	      dir)
	  (while (setq dir (path-iter-next path-iter))
	    (when (string-match dir-regex dir)
	      (cl-mapc
	       (lambda (file-name)
		 (let ((absfile (concat (file-name-as-directory dir) file-name)))
		   (when (and (not (directory-name-p file-name))
			      (or (null pred)
				  (funcall pred absfile)))
		     (push absfile result))))
	       (file-name-all-completions "" dir))
	      )))
	(cond
         ((eq action 'lambda)
	  ;; Called from `test-completion'
	  (uniq-file--valid-completion string result))

	 ((eq action t)
	  ;; Called from all-completions
	  result)
	 ))
      ))
   ))

(defun locate-uniquified-file (&optional path predicate default prompt)
  "Return an absolute filename, with completion in non-recursive PATH
\(default `load-path').  If PREDICATE is nil, it is ignored. If
non-nil, it must be a function that takes one argument; the
absolute file name.  The file name is included in the result if
PRED returns non-nil. DEFAULT is the default for completion.

In the user input string, `*' is treated as a wildcard."
  (interactive)
  (let ((iter (make-path-iterator :user-path-non-recursive (or path load-path))))
    (completing-read (or prompt "file: ")
		     (apply-partially #'uniq-file-completion-table iter)
		     predicate t nil nil default)
    ))

(defun locate-uniquified-file-iter (iter &optional predicate default prompt)
  "Return an absolute filename, with completion in path-iterator ITER.
If PREDICATE is nil, it is ignored. If non-nil, it must be a
function that takes one argument; the absolute file name.  The
file name is included in the result if PRED returns
non-nil. DEFAULT is the default for completion.

In the user input string, `*' is treated as a wildcard."
  (completing-read (format (concat (or prompt "file") " (%s): ") default)
		   (apply-partially #'uniq-file-completion-table iter)
		   predicate t nil nil default)
  )

(defun locate-uniquified-file-iter-2 (iter &optional predicate default prompt)
  "Same as `locate-uniquified-file-iter', but the internal
completion table is the list returned by `path-iter-all-files'."
  (let ((table (path-iter-all-files iter))
	(completion-styles '(uniquify-file)))
    (completing-read (format (concat (or prompt "file") " (%s): ") default)
		     table
		     predicate t nil nil default)
    ))

(provide 'uniquify-files)
;;; uniquify-files.el ends here
