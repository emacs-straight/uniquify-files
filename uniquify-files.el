;; uniquify-files.el --- Completion style for files in a path  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2017, 2019  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: completion table
;;   uniquify
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


;;; Discussion
;;;
;; These are the driving requirements for this completion style:
;;
;; - Allow the strings entered by the user and displayed in the
;;   completion list to be rearranged abbreviations of the absolute
;;   file name returned by `completing-read'.
;;
;; - Allow partial completion on the directory and filename portions
;;   of the abbreviated strings.
;;
;;   "partial completion" means file names are partitioned at "_-/"
;;   characters, so "fo-ba" completes to "foo-bar".
;;
;; - There should be no style-dependent code in the completion table
;;   function; all code that deals with converting between the
;;   abbreviated strings and the absolute strings should be in
;;   higher-level functions, under the control of
;;   `completion-styles-alist'.

;; The first requirement has the most effect on the design. There are
;; two common ways to select the result of a completion:
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
;;   The displayed completion list consists of the strings returned by
;;   `completion-all-completions' with the common prefix deleted;
;;   `completion-all-completions' must return strings in this format.
;;
;;   When the user selects a displayed completion, the string is
;;   passed to `test-completion'; it must accept strings in this format
;;   and return t.
;;
;;   For the uniquify-file style, this is a partial or complete file
;;   name plus any required uniquifying directories, formatted
;;   according to `uniquify-files-style'.
;;
;; - completion table input
;;
;;   The string input to the completion table function.
;;
;;   The `completion-try-completion' and `completion-all-completion'
;;   `test-completion' functions must convert user format strings to
;;   completion table input format strings when calling the
;;   corresponding low-level completion functions that call the
;;   completion table function.
;;
;;   For the uniquify-file style, this contains the complete or
;;   partial directory name or no directory name, followed by the
;;   partial or complete file name, in normal elisp filename format.
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
;; advise `test-completion' to call `try-completion' first.
;;
;; Similarly, the current completion code does not have a provision
;; for converting from user format to data format after a completion
;; is selected; we add that via advice on `completing-read-default'. A
;; future version may add this conversion in
;; `completion--complete-and-exit' instead.

(require 'cl-lib)
(require 'path-iterator)

(defvar uniquify-files-style 'abbrev
  ;; FIXME: change to defcustom
  "Style used to format uniquifying directories.
One of:
- 'abbrev : minimal directories required to identify a unique file (may be empty)
- 'full   : absolute directory path or empty")

(defconst uniq-files-regexp "^\\(.*\\)<\\([^>]*\\)>?$"
  ;; The trailing '>' is optional so the user can type "<dir" in the
  ;; input buffer to complete directories.
  "Regexp matching uniqufied file name.
Match 1 is the filename, match 2 is the relative directory.")

(defun uniq-file-dir-match (partial abs)
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

(defun uniq-files-conflicts (conflicts dir)
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
       ;; `dir' can match more than one absolute directory, so we
       ;; compute `completed-dir' for each element of conflicts.
       ;;
       ;; `completed-dir' may overlap only `common-root', or both
       ;; `common-root' and `non-common'; eliminate the overlap with
       ;; `non-common'.
       ;;
       ;; We can assume `completed-dir' matches at the end of
       ;; `common-root', not in the middle.
       ;;
       ;; example (see uniquify-files-test.el test-uniq-file-uniquify, dir "Al/a-")
       ;;   common        : c:/tmp/Alice/
       ;;   non-common    : alice-2/
       ;;   completed-dir : Alice/alice-2/
       ;;
       (let* ((completed-dir (and dir (uniq-file-dir-match dir (file-name-directory name))))
	      (completed-dirs (and completed-dir (nreverse (split-string completed-dir "/" t))))
	      (non-common (substring (file-name-directory name) (length common-root)))
	      (first-non-common (substring non-common 0 (string-match "/" non-common))))

	 (while completed-dirs
	   (let ((dir1 (pop completed-dirs)))
	     (when (not (string-equal dir1 first-non-common))
	       (setq non-common (concat dir1 "/" non-common)))))

	 (concat (file-name-nondirectory name)
		 "<"
		 non-common
		 ">")))
     conflicts)
    ))

(defun uniq-file-uniquify (names dir)
  "Return a uniquified list of names built from NAMES.
NAMES contains absolute file names.

The result contains non-directory filenames with partial
directory paths appended.  The partial directory path will always
include at least the completion of DIR.

If DIR is non-nil, all elements of NAMES must match DIR."
  (when names
    (cl-ecase uniquify-files-style
      (abbrev
       (let (result
	     conflicts ;; list of names where all non-directory names are the same.
	     )

	 ;; Sort names so duplicates are grouped together
	 (setq names (sort names (lambda (a b)
				   (string< (file-name-nondirectory a) (file-name-nondirectory b)))))

	 (while names
	   (setq conflicts (list (pop names)))
	   (while (and names
		       (string= (file-name-nondirectory (car conflicts)) (file-name-nondirectory (car names))))
	     (push (pop names) conflicts))

	   (if (= 1 (length conflicts))
	       (let ((completed-dir (and dir (uniq-file-dir-match dir (file-name-directory (car conflicts))))))
		 (push
		  (if completed-dir
		      (concat (file-name-nondirectory (car conflicts)) "<" completed-dir ">")
		    (concat (file-name-nondirectory (car conflicts))))
		  result))

	     (setq result (append (uniq-files-conflicts conflicts dir) result)))
	   )
	 (nreverse result)
	 ))

      (full
       names)
      )))

(defun uniq-file-normalize (user-string)
  "Convert USER-STRING to table input string."
  (let* ((match (string-match uniq-files-regexp user-string))
	 (dir (and match (match-string 2 user-string))))

    (if match
	(if (= 0 (length dir)) ;; ie "file<"
	    (match-string 1 user-string)
	  (concat (file-name-as-directory dir) (match-string 1 user-string)))

      ;; else not uniquified
      user-string)))

(defun uniq-file-valid-completion (string all)
  "Return non-nil if STRING is a valid completion in ALL,
else return nil.  ALL should be the result of `all-completions'.
STRING should be in completion table input format."
  ;; STRING is a valid completion if its normalization is a tail of
  ;; one element of ALL.
  (let* ((regexp (concat (unless (file-name-absolute-p string) "/") string "\\'"))
	 (matched nil)
	 name)

    (while (and all
		(not matched))
      (setq name (pop all))
      (when (string-match regexp name)
	(setq matched t)))

    matched))

(defun completion-uniquify-file-try-completion (string table pred point)
  "Implement `completion-try-completion' for uniquify-file."
  (cond
   ((functionp table) ;; normal case
    (let* ((table-string (uniq-file-normalize string))
	   (abs-all (all-completions table-string table pred)))

      (cond
       ((null abs-all) ;; No matches.
	nil)

       ((= 1 (length abs-all)) ;; One match; unique.

	(if (uniq-file-valid-completion table-string abs-all)
	    t

	  (let ((result (car (uniq-file-uniquify abs-all (file-name-directory table-string)))))
	    (cons result (length result)))))

       (t ;; Multiple matches

	;; Find merged completion of uniqified file names
	(let* ((uniq-all (uniq-file-uniquify abs-all (file-name-directory table-string)))
	       (completion-pcm--delim-wild-regex
		(cl-ecase uniquify-files-style
		  (abbrev
		   (concat "[" completion-pcm-word-delimiters "<>*]"))
		  (full
		   completion-pcm--delim-wild-regex)))
	       (pattern (completion-pcm--string->pattern string point))
	       (merged-pat (completion-pcm--merge-completions uniq-all pattern))

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
       )))

   ;; The following cases handle being called from
   ;; icomplete-completions with result of all-completions instead of
   ;; the real table function.

   ((null table) ;; No matches.
    nil)

   ((consp table)
    (cond
     ((= 1 (length table)) ;; One match; unique.

      (if (string-equal string (car table))
	    t

	  (let ((result (car table)))
	    (cons result (length result)))))

     (t ;; Multiple matches

      ;; Find merged completion of uniqified file names
      (let* ((completion-pcm--delim-wild-regex
	      (cl-ecase uniquify-files-style
		(abbrev
		 (concat "[" completion-pcm-word-delimiters "<>*]"))
		(full
		 completion-pcm--delim-wild-regex)))
	     ;; If STRING ends in an empty directory part, some valid
	     ;; completions won't have any directory part.
	     (trimmed-string
	      (if (and (< 0 (length string))
		       (= (aref string (1- (length string))) ?<))
		  (substring string 0 -1)
		string))
	     (pattern (completion-pcm--string->pattern trimmed-string point))
	     (merged-pat (completion-pcm--merge-completions table pattern))

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
   ))

(defun uniq-files-hilit (string all point)
  "Apply face text properties to each element of ALL.
STRING is the current user input.
ALL is a list of strings in user format.
POINT is the position of point in STRING.
Returns new list.

Adds the face `completions-first-difference' to the first
character after each completion field."
  ;; IMPROVEME: duplicates `completion-uniquify-file-try-completion';
  ;; consider refactor and cache.
  (let* ((completion-pcm--delim-wild-regex
	  (cl-ecase uniquify-files-style
	    (abbrev
	     (concat "[" completion-pcm-word-delimiters "<>*]"))
	    (full
	     completion-pcm--delim-wild-regex)
	    ))
	 ;; If STRING ends in an empty directory part, some valid
	 ;; completions won't have any directory part.
	 (trimmed-string
	  (if (and (< 0 (length string))
		   (= (aref string (1- (length string))) ?<))
	      (substring string 0 -1)
	    string))
	 (pattern (completion-pcm--string->pattern trimmed-string point))
	 (merged-pat (nreverse (completion-pcm--merge-completions all pattern)))
	 (field-count 0)
	 (regex (completion-pcm--pattern->regex merged-pat '(any star any-delim point)))
	 )
    (dolist (x merged-pat)
      (when (not (stringp x))
	(setq field-count (1+ field-count))))

    (mapcar
     (lambda (string)
       (when (string-match regex string)
	 (cl-loop
	  for i from 1 to field-count
	  do
	  (when (and
		 (match-beginning i)
		 (<= (1+ (match-beginning i)) (length string)))
	    (put-text-property (match-beginning i) (1+ (match-beginning i)) 'face 'completions-first-difference string))
	  ))
       string)
     all)))

(defun completion-uniquify-file-all-completions (user-string table pred point)
  "Implement `completion-all-completions' for uniquify-file."

  ;; Convert `user-string' to dir/name format, extract dir for uniquify
  (let* ((table-string (uniq-file-normalize user-string))
	 (all (uniq-file-uniquify (all-completions table-string table pred)
				  (file-name-directory table-string))))

    (when all
      (uniq-files-hilit user-string all point))
    ))

(defun completion-uniquify-file-get-data-string (user-string table pred)
  "Implement `completion-get-data-string' for 'uniq-file."
  ;; We assume USER-STRING is complete, but it may not be unique, in
  ;; both the file name and the directory; shortest completion of each
  ;; portion is the correct one.
  (let ((all (all-completions (uniq-file-normalize user-string) table pred)))
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
    (car all)))

(defun completion-get-data-string (user-string table pred)
  "Return the data string corresponding to USER-STRING."
  ;; IMPROVEME: should use `completion--category-override' and
  ;; `completion-styles-alist' in general, but this is adequate
  ;; for this case.
  (cl-case (completion-metadata-get (completion-metadata user-string table pred) 'category)
    (uniq-file (completion-uniquify-file-get-data-string user-string table pred))
    (t user-string)
  ))

(defun uniq-file-test-completion-advice (orig-fun string table &optional pred)
  "Advice for `test-completion'; convert display string to table input."
  (let ((metadata (completion-metadata string table pred)))
    (cl-case (completion-metadata-get metadata 'category)
      (uniq-file
       ;; IMPROVEME: should use `completion--category-override' and
       ;; `completion-styles-alist' in general, but this is adequate
       ;; for this case.
       (let ((table-string (uniq-file-normalize string)))
	 (uniq-file-valid-completion table-string (all-completions table-string table pred))))

      (t
       (funcall orig-fun string table pred))
      )))

(advice-add #'test-completion :around #'uniq-file-test-completion-advice)

(defun uniq-file-completing-read-default-advice (orig-fun prompt collection &optional predicate
							  require-match initial-input hist def
							  inherit-input-method)
  "Advice for `completing-read-default'; convert display string to data string."
  (let ((user-string (funcall orig-fun prompt collection
			      predicate require-match initial-input hist def
			      inherit-input-method)))
    (completion-get-data-string user-string collection predicate)
    ))

(advice-add #'completing-read-default :around #'uniq-file-completing-read-default-advice)

(add-to-list 'completion-category-defaults '(uniq-file (styles . (uniquify-file))))

(add-to-list 'completion-styles-alist
	     '(uniquify-file
	       completion-uniquify-file-try-completion
	       completion-uniquify-file-all-completions
	       "display uniquified filenames."))

(defun uniq-file-completion-table (path-iter string pred action)
  "Do completion for file names in `locate-uniquified-file'.

PATH-ITER is a `path-iterator' object. It will be restarted for
each call to `uniq-file-completion-table'.

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

Return a list of absolute file names matching STRING, using
`partial-completion' style matching."

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
	   ;; We specify the category 'uniq-file here, because the
	   ;; input STRING is not a prefix of the returned results
	   ;; (absolute file name), which is a requirement of most
	   ;; completion styles.  We use the default sort order, which
	   ;; is shortest first, so "project.el" is easier to complete
	   ;; when it also matches "project-am.el".
	   '(category . uniq-file))))

   ((null action)
    ;; Called from `try-completion'; should never get here (see
    ;; `completion-uniquify-file-try-completion').
    nil)

   ((eq action 'lambda)
    ;; Called from `test-completion'; should never get here (see
    ;; uniq-file-test-completion-advice).
    nil)

   ((eq action t) ;; Called from all-completions

    ;; `completion-regexp-list', is matched against file names and
    ;; directories relative to `dir'.  Thus to handle partial
    ;; completion delimiters in `string', we construct two regexps
    ;; from `string'; one from the directory portion, and one from the
    ;; non-directory portion.  We use the directory regexp here, and
    ;; pass the non-directory regexp to `file-name-all-completions'
    ;; via `completion-regexp-list'.  The `string' input to
    ;; `file-name-all-completions' is redundant with the regexp, so we
    ;; always build a regexp, and pass an empty string.

    (let* ((dir-name (directory-file-name (or (file-name-directory string) "")))
	   (file-name (file-name-nondirectory string))

	   ;; `completion-pcm--string->pattern' assumes its argument
	   ;; is anchored at the beginning but not the end; that is
	   ;; true for `dir-name' only if it is absolute.
	   (dir-pattern (completion-pcm--string->pattern
			 (if (file-name-absolute-p dir-name) dir-name (concat "*/" dir-name))))
	   (dir-regex (completion-pcm--pattern->regex dir-pattern))

	   ;; Child directories of `dir' are not valid completions
	   ;; (`path-iterator' handles recursion).
	   ;; `file-name-all-completions' returns child directories
	   ;; with a trailing '/', but that is added _after_ they are
	   ;; matched against `completion-regexp-list'. So we exclude
	   ;; them below.
	   (file-pattern (completion-pcm--string->pattern file-name))
	   (file-regex (completion-pcm--pattern->regex file-pattern))

	   ;; A project that deals only with C files might set
	   ;; `completion-regexp-list' to match only *.c, *.h, so we
	   ;; preserve that here.
	   (completion-regexp-list (cons file-regex completion-regexp-list))
	   (result nil))

      (path-iter-restart path-iter)

      (let (dir)
	(while (setq dir (path-iter-next path-iter))
	  (when (string-match dir-regex dir)
	    (cl-mapc
	     (lambda (filename)
	       (let ((absfile (concat (file-name-as-directory dir) filename)))
		 (when (and (not (directory-name-p filename))
			    (or (null pred)
				(funcall pred absfile)))
		   (push absfile result))))
	     (file-name-all-completions "" dir))
	    )))
      result))
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

(provide 'uniquify-files)
