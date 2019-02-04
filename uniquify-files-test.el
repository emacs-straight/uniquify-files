;;; uniquify-files-test.el - Test functions in uniquify-files.el -*- lexical-binding:t no-byte-compile:t -*-
;;
;; Copyright (C) 2017, 2019  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
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

;;; Commentary:
;;;
;; This is not a complete test of the completion style; the way the
;; completion functions interact with completing-read is not fully
;; tested. The following table gives useful test cases for a manual
;; interactive test (copy it to an org-mode buffer).
;;
;; | input                                         | display                                                                  | result                              | works/comment                                 |
;; |-----------------------------------------------+--------------------------------------------------------------------------+-------------------------------------+-----------------------------------------------|
;; | "f-file1" <ret>                               | f-file1(foo-file1.text<){*>*, *A*lice/alice-1/>, *A*lice/alice-2>, ... } | <root>/foo-file1.text               | works                                         |
;; | "f-file1" <right> <ret>                       | f-file1(foo-file1.text<){*Alice/alice-1/>*. *A*lice/alice-2/>, ... }     | <root>/Alice/alice-1/foo-file1.text | works                                         |
;; | "f-file1" <right> <tab> <ret>                 | foo-file1.text<{*>*. *A*lice/alice-1/>. *A*lice/alice-2/>, ... }         | <root>/foo-file1.text               | works                                         |
;; | "f-file1" <tab> <tab> <ret>                   | shows *Completion* buffer                                                | <root>/foo-file1.text               | works                                         |
;; | "f-file1" <C-tab> <C-tab> <ret>               | cycles foo-file1.text<> [Matched]                                        | <root>/Alice/alice-1foo-file1.text  | works                                         |
;; | "f-file1<a-2" <ret>                           | f-file1<a-2 [Matched]                                                    | <root>/Alice/alice-2/foo-file1.text | works                                         |
;; | "b-file2" <tab> <ret>                         | bar-file2.text<./alice-{*1*/> *2*/>}                                     | <root>/Alice/alice-1/bar-file2.text | works except display has bad glyph (./alice-) |
;; | "b-file2" <tab> <tab> <ret>                   | bar-file2.text<Alice/alice-{1/> 2/>}                                     | ""                                  | works                                         |
;; | "b-file2" <tab> <tab> <tab> <ret>             | shows *Completion* buffer                                                | ""                                  | works                                         |
;; | "f-file3" <ret>                               | f-file3(foo-file3.text) [Matched]                                        | <root>/Alice/alice-2/foo-file3.text | works                                         |
;; | "f-file3" <tab> <ret>                         | foo-file3.text [Matched]                                                 | <root>/Alice/alice-2/foo-file3.text | works                                         |
;; | "fil"                                         | fil (No matches)                                                         | -                                   | works                                         |
;; | "*-file1" <tab> <ret>                         | *-file1.text<{*f*oo-file1.text<*A*lice/alice-2/> ... }                   | <root>/Alice/alice-2/foo-file1.text | works                                         |
;; | "*-file1" <tab> A <tab> 1 <tab> <ret>         | *^-file1.text<Alice/alice-1/>{*bar-file1.text<Alice/alice-1/>*, ...}     | <root>/Alice/alice-1/bar-file1.text | works                                         |
;; | "*-file1" <tab> A <tab> 1 <tab> <del> f <tab> | foo-file1.text<Alice/alice-1/> [Matched]                                 | <root>/Alice/alice-1/foo-file1.text | works                                         |
;; | "foo-file1.text<Alice/alice-1> <ret>          | foo-file1.text<Alice/alice-1(/>) [Matched]                               | ""                                  | works                                         |


;; See `test-uniquify-file-all-completions-face' below for an explanation of `no-byte-compile'.

(require 'ert)
(require 'uniquify-files)

(defconst uft-root
  (concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "uniquify-files-resources"))

(defconst uft-alice1 (concat uft-root "/Alice/alice-1"))
(defconst uft-alice2 (concat uft-root "/Alice/alice-2"))
(defconst uft-Alice-alice3 (concat uft-root "/Alice/alice-3"))
(defconst uft-Bob-alice3 (concat uft-root "/Bob/alice-3"))
(defconst uft-bob1 (concat uft-root "/Bob/bob-1"))
(defconst uft-bob2 (concat uft-root "/Bob/bob-2"))

(defconst uft-iter
  (make-path-iterator
   :user-path-non-recursive
   (list uft-root
	 uft-alice1
	 uft-alice2
	 uft-Alice-alice3
	 uft-Bob-alice3
	 uft-bob1
	 uft-bob2)))

(ert-deftest test-uniq-file-completion-table ()
  "Test basic functions of table."
  ;; grouped by action
  (should (equal (uniq-file-completion-table uft-iter "fi" nil '(boundaries . ".text"))
		   '(boundaries . (0 . 5))))

  (should (equal (uniq-file-completion-table uft-iter "fi" nil 'metadata)
		 (cons 'metadata
		       (list
			'(category . project-file)
			'(styles   . (uniquify-file))))))

  ;; all-completions. We sort the results here to make the test stable
  (should (equal (sort (uniq-file-completion-table uft-iter "-fi" nil t) #'string-lessp)
		 (list
		  (concat uft-alice1 "/bar-file1.text")
		  (concat uft-alice1 "/bar-file2.text")
		  (concat uft-alice1 "/foo-file1.text")
		  (concat uft-alice1 "/foo-file2.text")
		  (concat uft-alice2 "/bar-file1.text")
		  (concat uft-alice2 "/bar-file2.text")
		  (concat uft-alice2 "/foo-file1.text")
		  (concat uft-alice2 "/foo-file3.text")
 		  (concat uft-alice2 "/foo-file3.texts")
 		  (concat uft-Alice-alice3 "/foo-file4.text")
 		  (concat uft-Bob-alice3   "/foo-file4.text")
		  (concat uft-bob1 "/foo-file1.text")
		  (concat uft-bob1 "/foo-file2.text")
		  (concat uft-bob2 "/foo-file1.text")
		  (concat uft-bob2 "/foo-file5.text")
		  (concat uft-root "/foo-file1.text")
		  (concat uft-root "/foo-file3.texts2")
		  )))

  (should (equal (sort (uniq-file-completion-table uft-iter "a-1/f-fi" nil t) #'string-lessp)
		 (list
		  (concat uft-alice1 "/foo-file1.text")
		  (concat uft-alice1 "/foo-file2.text")
		  )))

  (should (equal (uniq-file-completion-table uft-iter "file1.text<uft-alice1/>" nil t)
		 ;; some caller did not deuniquify; treated as misspelled; no match
		 nil))


  ;; This table does not implement try-completion
  (should (equal (uniq-file-completion-table uft-iter "fi" nil nil)
		 nil))

  ;; test-completion
  (should (equal (uniq-file-completion-table uft-iter (uniq-file-to-table-input "foo-file1.text<alice-1>") nil 'lambda)
		 t))

  )

(ert-deftest test-uniq-file-path-completion-table-pred ()
  "Test table with predicate."
  (should (equal (sort (uniq-file-completion-table
			uft-iter
			"-fi"
			(lambda (absfile) (string= (file-name-directory absfile) (file-name-as-directory uft-alice1)))
			t)
		       #'string-lessp)
		 (list
		  (concat uft-alice1 "/bar-file1.text")
		  (concat uft-alice1 "/bar-file2.text")
		  (concat uft-alice1 "/foo-file1.text")
		  (concat uft-alice1 "/foo-file2.text")
		  )))

  (should (equal (sort (uniq-file-completion-table
			uft-iter
			"-fi"
			(lambda (absfile) (string= (file-name-nondirectory absfile) "bar-file1.text"))
			t)
		       #'string-lessp)
		 (list
		  (concat uft-alice1 "/bar-file1.text")
		  (concat uft-alice2 "/bar-file1.text")
		  )))

  )

(defun test-uniq-file-test-completion-1 (table)
  (should (equal (test-completion "foo-fi" table)
		 nil))

  (should (equal (test-completion "f-fi<dir" table)
		 nil))

  (should (equal (test-completion "foo-file1.text<>" table)
		 t))

  (should (equal (test-completion "foo-file1.text" table)
		 t))

  (should (equal (test-completion "foo-file1.text<alice-1/>" table)
		 t))

  (should (equal (test-completion "foo-file3.tex" table) ;; partial file name
		 nil))

  (should (equal (test-completion "foo-file3.texts2" table)
		 t))

  (should (equal (test-completion "bar-file2.text<Alice/alice-" table)
		 nil))
  )

(ert-deftest test-uniq-file-test-completion-func ()
  (let ((table (apply-partially 'uniq-file-completion-table uft-iter)))
    (test-uniq-file-test-completion-1 table)))

(ert-deftest test-uniq-file-test-completion-list ()
  (let ((table (path-iter-all-files uft-iter))
	(completion-styles '(uniquify-file))) ;; FIXME: need a way to specify category
    (test-uniq-file-test-completion-1 table)))

(defun test-uniq-file-all-completions-noface-1 (table)
  (should (equal
	   (sort (uniq-file-all-completions "" table nil nil) #'string-lessp)
	   (list
	    "bar-file1.text<alice-1/>"
	    "bar-file1.text<alice-2/>"
	    "bar-file2.text<alice-1/>"
	    "bar-file2.text<alice-2/>"
	    "foo-file1.text<>"
	    "foo-file1.text<Alice/alice-1/>"
	    "foo-file1.text<Alice/alice-2/>"
	    "foo-file1.text<Bob/bob-1/>"
	    "foo-file1.text<Bob/bob-2/>"
	    "foo-file2.text<Alice/alice-1/>"
	    "foo-file2.text<Bob/bob-1/>"
	    "foo-file3.text"
	    "foo-file3.texts"
	    "foo-file3.texts2"
	    "foo-file4.text<Alice/alice-3/>"
	    "foo-file4.text<Bob/alice-3/>"
	    "foo-file5.text"
	    )))

  (should (equal
	   (sort (uniq-file-all-completions "*-fi" table nil nil) #'string-lessp)
	   (list
	    "bar-file1.text<alice-1/>"
	    "bar-file1.text<alice-2/>"
	    "bar-file2.text<alice-1/>"
	    "bar-file2.text<alice-2/>"
	    "foo-file1.text<>"
	    "foo-file1.text<Alice/alice-1/>"
	    "foo-file1.text<Alice/alice-2/>"
	    "foo-file1.text<Bob/bob-1/>"
	    "foo-file1.text<Bob/bob-2/>"
	    "foo-file2.text<Alice/alice-1/>"
	    "foo-file2.text<Bob/bob-1/>"
	    "foo-file3.text"
	    "foo-file3.texts"
	    "foo-file3.texts2"
	    "foo-file4.text<Alice/alice-3/>"
	    "foo-file4.text<Bob/alice-3/>"
	    "foo-file5.text"
	    )))

  (should (equal
	   (sort (uniq-file-all-completions "b" table nil nil) #'string-lessp)
	   (list
	    "bar-file1.text<alice-1/>"
	    "bar-file1.text<alice-2/>"
	    "bar-file2.text<alice-1/>"
	    "bar-file2.text<alice-2/>"
	    )))

  (should (equal
	   (sort (uniq-file-all-completions "foo" table nil nil) #'string-lessp)
	   (list
	    "foo-file1.text<>"
	    "foo-file1.text<Alice/alice-1/>"
	    "foo-file1.text<Alice/alice-2/>"
	    "foo-file1.text<Bob/bob-1/>"
	    "foo-file1.text<Bob/bob-2/>"
	    "foo-file2.text<Alice/alice-1/>"
	    "foo-file2.text<Bob/bob-1/>"
	    "foo-file3.text"
	    "foo-file3.texts"
	    "foo-file3.texts2"
	    "foo-file4.text<Alice/alice-3/>"
	    "foo-file4.text<Bob/alice-3/>"
	    "foo-file5.text"
	    )))

  (should (equal
	   (sort (uniq-file-all-completions "f-file2" table nil nil) #'string-lessp)
	   (list
	    "foo-file2.text<Alice/alice-1/>"
	    "foo-file2.text<Bob/bob-1/>"
	    )))

  (should (equal
	   (sort (uniq-file-all-completions "b-fi<" table nil nil) #'string-lessp)
	   (list
	    "bar-file1.text<alice-1/>"
	    "bar-file1.text<alice-2/>"
	    "bar-file2.text<alice-1/>"
	    "bar-file2.text<alice-2/>"
	    )))

  (should (equal
	   (sort (uniq-file-all-completions "f-file<" table nil nil) #'string-lessp)
	   (list
	    "foo-file1.text<>"
	    "foo-file1.text<Alice/alice-1/>"
	    "foo-file1.text<Alice/alice-2/>"
	    "foo-file1.text<Bob/bob-1/>"
	    "foo-file1.text<Bob/bob-2/>"
	    "foo-file2.text<Alice/alice-1/>"
	    "foo-file2.text<Bob/bob-1/>"
	    "foo-file3.text"
	    "foo-file3.texts"
	    "foo-file3.texts2"
	    "foo-file4.text<Alice/alice-3/>"
	    "foo-file4.text<Bob/alice-3/>"
	    "foo-file5.text"
	    )))

  (should (equal
	   (sort (uniq-file-all-completions "b-fi<a-" table nil nil) #'string-lessp)
	   (list
	    "bar-file1.text<alice-1/>"
	    "bar-file1.text<alice-2/>"
	    "bar-file2.text<alice-1/>"
	    "bar-file2.text<alice-2/>"
	    )))

  (let ((completion-ignore-case t))
    (should (equal
	     (sort (uniq-file-all-completions "b-fi<a-" table nil nil) #'string-lessp)
	     (list
	      "bar-file1.text<Alice/alice-1/>"
	      "bar-file1.text<Alice/alice-2/>"
	      "bar-file2.text<Alice/alice-1/>"
	      "bar-file2.text<Alice/alice-2/>"
	      )))
    )

  (should (equal
	   (sort (uniq-file-all-completions "b-fi<a-1" table nil nil) #'string-lessp)
	   (list "bar-file1.text<alice-1/>"
		 "bar-file2.text<alice-1/>")))

  (let ((completion-ignore-case t))
    (should (equal
	     (sort (uniq-file-all-completions "b-fi<a-1" table nil nil) #'string-lessp)
	     (list "bar-file1.text<Alice/alice-1/>"
		   "bar-file2.text<Alice/alice-1/>")))
    )

  (should (equal (uniq-file-all-completions "f-file1.text<a-1" table nil nil)
		 (list "foo-file1.text<alice-1/>")))

  (let ((completion-ignore-case t))
    (should (equal (uniq-file-all-completions "f-file1.text<a-1" table nil nil)
		   (list "foo-file1.text<Alice/alice-1/>")))
    )

  (should (equal (sort (uniq-file-all-completions "f-file1.text<al" table nil nil) #'string-lessp)
		 (list
		  "foo-file1.text<alice-1/>"
		  "foo-file1.text<alice-2/>")))

  (let ((completion-ignore-case t))
    (should (equal (sort (uniq-file-all-completions "f-file1.text<al" table nil nil) #'string-lessp)
		   (list
		    "foo-file1.text<Alice/alice-1/>"
		    "foo-file1.text<Alice/alice-2/>")))
    )

  (should (equal (sort (uniq-file-all-completions "f-file4.text<a-3" table nil nil) #'string-lessp)
		 (list
		  "foo-file4.text<Alice/alice-3/>"
		  "foo-file4.text<Bob/alice-3/>")))

  (should (equal (uniq-file-all-completions "f-file5" table nil nil)
		 (list "foo-file5.text")))

  (should (equal (uniq-file-all-completions "foo-file1.text<Alice/alice-1/>" table nil nil)
		 (list "foo-file1.text<Alice/alice-1/>")))

  (should (equal
	   (sort (uniq-file-all-completions "b-fi<a>" table nil nil) #'string-lessp)
	   (list
	    "bar-file1.text<alice-1/>"
	    "bar-file1.text<alice-2/>"
	    "bar-file2.text<alice-1/>"
	    "bar-file2.text<alice-2/>"
	    )))

  (let ((completion-ignore-case t))
    (should (equal
	     (sort (uniq-file-all-completions "b-fi<a>" table nil nil) #'string-lessp)
	     (list
	      "bar-file1.text<Alice/alice-1/>"
	      "bar-file1.text<Alice/alice-2/>"
	      "bar-file2.text<Alice/alice-1/>"
	      "bar-file2.text<Alice/alice-2/>"
	      )))
    )

  (should (equal
	   (sort (uniq-file-all-completions "foo-file1.text<>" table nil nil) #'string-lessp)
	   ;; This is complete but not unique, because the directory part matches multiple directories.
	   (list
	    "foo-file1.text<>"
	    "foo-file1.text<Alice/alice-1/>"
	    "foo-file1.text<Alice/alice-2/>"
	    "foo-file1.text<Bob/bob-1/>"
	    "foo-file1.text<Bob/bob-2/>"
	    )))
  )

(ert-deftest test-uniq-file-all-completions-noface-func ()
  (let ((table (apply-partially 'uniq-file-completion-table uft-iter))
	(completion-ignore-case nil))
    (test-uniq-file-all-completions-noface-1 table)))

(ert-deftest test-uniq-file-all-completions-noface-list ()
  (let ((table (path-iter-all-files uft-iter))
	(completion-ignore-case nil)
	(completion-styles '(uniquify-file))) ;; FIXME: need a way to specify category
    (test-uniq-file-all-completions-noface-1 table)))

(defun test-uniq-file-hilit (pos-list string)
  "Set 'face text property to 'completions-first-difference at
all positions in POS-LIST in STRING; return new string."
  (while pos-list
    (let ((pos (pop pos-list)))
      (put-text-property pos (1+ pos) 'face 'completions-first-difference string)))
  string)

(ert-deftest test-uniq-file-all-completions-face ()
  ;; all-completions tested above without considering face text
  ;; properties; here we test just those properties. Test cases are
  ;; the same as above.
  ;;
  ;; WORKAROUND: byte-compiling this test makes it fail; it appears to be
  ;; sharing strings that should not be shared because they have
  ;; different text properties.
  (let ((table (apply-partially 'uniq-file-completion-table uft-iter))
	(completion-ignore-case nil))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '(0) "bar-file1.text<alice-1/>")
	      (test-uniq-file-hilit '(0) "bar-file1.text<alice-2/>")
	      (test-uniq-file-hilit '(0) "bar-file2.text<alice-1/>")
	      (test-uniq-file-hilit '(0) "bar-file2.text<alice-2/>")
	      (test-uniq-file-hilit '(0) "foo-file1.text<>")
	      (test-uniq-file-hilit '(0) "foo-file1.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(0) "foo-file1.text<Alice/alice-2/>")
	      (test-uniq-file-hilit '(0) "foo-file1.text<Bob/bob-1/>")
	      (test-uniq-file-hilit '(0) "foo-file1.text<Bob/bob-2/>")
	      (test-uniq-file-hilit '(0) "foo-file2.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(0) "foo-file2.text<Bob/bob-1/>")
	      (test-uniq-file-hilit '(0) "foo-file3.text")
	      (test-uniq-file-hilit '(0) "foo-file3.texts")
	      (test-uniq-file-hilit '(0) "foo-file3.texts2")
	      (test-uniq-file-hilit '(0) "foo-file4.text<Alice/alice-3/>")
	      (test-uniq-file-hilit '(0) "foo-file4.text<Bob/alice-3/>")
	      (test-uniq-file-hilit '(0) "foo-file5.text")
	      )))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "*-fi" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '(0 8) "bar-file1.text<alice-1/>")
	      (test-uniq-file-hilit '(0 8) "bar-file1.text<alice-2/>")
	      (test-uniq-file-hilit '(0 8) "bar-file2.text<alice-1/>")
	      (test-uniq-file-hilit '(0 8) "bar-file2.text<alice-2/>")
	      (test-uniq-file-hilit '(0 8) "foo-file1.text<>")
	      (test-uniq-file-hilit '(0 8) "foo-file1.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(0 8) "foo-file1.text<Alice/alice-2/>")
	      (test-uniq-file-hilit '(0 8) "foo-file1.text<Bob/bob-1/>")
	      (test-uniq-file-hilit '(0 8) "foo-file1.text<Bob/bob-2/>")
	      (test-uniq-file-hilit '(0 8) "foo-file2.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(0 8) "foo-file2.text<Bob/bob-1/>")
	      (test-uniq-file-hilit '(0 8) "foo-file3.text")
	      (test-uniq-file-hilit '(0 8) "foo-file3.texts")
	      (test-uniq-file-hilit '(0 8) "foo-file3.texts2")
	      (test-uniq-file-hilit '(0 8) "foo-file4.text<Alice/alice-3/>")
	      (test-uniq-file-hilit '(0 8) "foo-file4.text<Bob/alice-3/>")
	      (test-uniq-file-hilit '(0 8) "foo-file5.text")
	      )))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "b" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '(8) "bar-file1.text<alice-1/>")
	      (test-uniq-file-hilit '(8) "bar-file1.text<alice-2/>")
	      (test-uniq-file-hilit '(8) "bar-file2.text<alice-1/>")
	      (test-uniq-file-hilit '(8) "bar-file2.text<alice-2/>")
	      )))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "foo" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '(8) "foo-file1.text<>")
	      (test-uniq-file-hilit '(8) "foo-file1.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(8) "foo-file1.text<Alice/alice-2/>")
	      (test-uniq-file-hilit '(8) "foo-file1.text<Bob/bob-1/>")
	      (test-uniq-file-hilit '(8) "foo-file1.text<Bob/bob-2/>")
	      (test-uniq-file-hilit '(8) "foo-file2.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(8) "foo-file2.text<Bob/bob-1/>")
	      (test-uniq-file-hilit '(8) "foo-file3.text")
	      (test-uniq-file-hilit '(8) "foo-file3.texts")
	      (test-uniq-file-hilit '(8) "foo-file3.texts2")
	      (test-uniq-file-hilit '(8) "foo-file4.text<Alice/alice-3/>")
	      (test-uniq-file-hilit '(8) "foo-file4.text<Bob/alice-3/>")
	      (test-uniq-file-hilit '(8) "foo-file5.text")
	      )))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "f-file2" table nil nil) #'string-lessp)
	     (list
	      (test-uniq-file-hilit '(15) "foo-file2.text<Alice/alice-1/>")
	      (test-uniq-file-hilit '(15) "foo-file2.text<Bob/bob-1/>")
	      )))

    (should (equal-including-properties
	     (sort (uniq-file-all-completions "foo-file3.text" table nil nil) #'string-lessp)
	     (list
	      "foo-file3.text"
	      (test-uniq-file-hilit '(14) "foo-file3.texts")
	      (test-uniq-file-hilit '(14) "foo-file3.texts2")
	      )))

    ))

(defun test-uniq-file-try-completion-1 (table)
  (let (string)

    (setq string "fo")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("foo-file" . 8)))

    (setq string "b")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("bar-file" . 8)))

    (setq string "fo<al")
    (should (equal (uniq-file-try-completion string table nil 2)
		   '("foo-file<alice-" . 8)))
    (should (equal (uniq-file-try-completion string table nil 5)
		   '("foo-file<alice-" . 15)))

    (let ((completion-ignore-case t))
      (setq string "fo<al")
      (should (equal (uniq-file-try-completion string table nil 2)
		     '("foo-file<alice" . 8)))
      (should (equal (uniq-file-try-completion string table nil 5)
		     '("foo-file<alice" . 14)))
      )

    (setq string "foo-file3") ;; not unique, not valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("foo-file3.text" . 14)))

    (setq string "f-file1.text<a-1") ;; unique but not valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("foo-file1.text<alice-1/>" . 24)))

    (let ((completion-ignore-case t))
      (setq string "f-file1.text<a-1") ;; unique but not valid
      (should (equal (uniq-file-try-completion string table nil (length string))
		     '("foo-file1.text<Alice/alice-1/>" . 30)))
      )

    (setq string "foo-file1.text") ;; valid but not unique
    (should (equal (uniq-file-try-completion string table nil (length string))
		   (cons "foo-file1.text<" 15)))

    (setq string "foo-file1<") ;; not valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   (cons "foo-file1.text<" 15)))

    (setq string "foo-file1.text<>") ;; valid but not unique
    (should (equal (uniq-file-try-completion string table nil (length string))
		   (cons "foo-file1.text<>" 15)))

    (setq string "foo-file1.text<alice-1/>") ;; valid and unique
    (should (equal (uniq-file-try-completion string table nil (length string))
		   t))

    (let ((completion-ignore-case t))
      (setq string "foo-file1.text<alice-1/>") ;; valid and unique, but accidental match on Alice
      (should (equal (uniq-file-try-completion string table nil (length string))
		     '("foo-file1.text<Alice/alice-1/>" . 30)))
      )

    (setq string "foo-file3.texts") ;; not unique, valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("foo-file3.texts" . 15)))

    (setq string "foo-file3.texts2") ;; unique and valid
    (should (equal (uniq-file-try-completion string table nil (length string))
		   t))

    (setq string "fil2") ;; misspelled
    (should (equal (uniq-file-try-completion string table nil (length string))
		   nil))

    (setq string "b-file2")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("bar-file2.text<alice-" . 21)))

    ;; prev + <tab>; input is prev output
    (setq string "bar-file2.text<alice-")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("bar-file2.text<alice-" . 21)))

    ;; prev + <tab>; input is prev output
    (setq string "bar-file2.text<alice-")
    (should (equal (uniq-file-try-completion string table nil (length string))
		   '("bar-file2.text<alice-" . 21)))

    ;; completion-try-completion called from icomplete-completions with
    ;; result of all-completions instead of table function.
    (setq string "f-file<")
    (let ((comps (uniq-file-all-completions string table nil nil)))
      (should (equal (uniq-file-try-completion string comps nil (length string))
		     (cons "foo-file" 8))))
    ))

(ert-deftest test-uniq-file-try-completion-func ()
  (let ((table (apply-partially 'uniq-file-completion-table uft-iter))
	(completion-ignore-case nil))
    (test-uniq-file-try-completion-1 table)))

(ert-deftest test-uniq-file-try-completion-list ()
  (let ((table (path-iter-all-files uft-iter))
	(completion-ignore-case nil)
	(completion-styles '(uniquify-file))) ;; FIXME: need a way to specify category
    (test-uniq-file-try-completion-1 table)))

(ert-deftest test-uniq-file-get-data-string ()
  (let ((table (apply-partially 'uniq-file-completion-table uft-iter)))

    (should (equal (uniq-file-get-data-string "foo-file1.text<alice-1>" table nil)
		   (concat uft-alice1 "/foo-file1.text")))

    (should (equal (uniq-file-get-data-string "foo-file3.text" table nil)
		   (concat uft-alice2 "/foo-file3.text")))

    (should (equal (uniq-file-get-data-string "foo-file3.texts" table nil)
		   (concat uft-alice2 "/foo-file3.texts")))

    (should (equal (uniq-file-get-data-string "foo-file3.texts2" table nil)
		   (concat uft-root "/foo-file3.texts2")))
    ))

(ert-deftest test-uniq-file-to-table-input ()
  (should (equal (uniq-file-to-table-input "fi")
		 "fi"))

  (should (equal (uniq-file-to-table-input "fi<di")
		 "di/fi"))

  (should (equal (uniq-file-to-table-input "foo-file1.text")
		 "foo-file1.text"))

  (should (equal (uniq-file-to-table-input "file1<Alice/alice-2/>")
		 "Alice/alice-2/file1"))

  (should (equal (uniq-file-to-table-input "file1<>")
		 "file1"))

  (should (equal (uniq-file-to-table-input "file1.text<Alice/alice-2/>")
		 "Alice/alice-2/file1.text"))

  (should (equal (uniq-file-to-table-input "bar-file2.text<Alice/alice-")
		 "Alice/alice-/bar-file2.text"))

  )

(ert-deftest test-uniq-file-uniquify ()
  (should (equal (uniq-file--uniquify
		  '("/Alice/alice1/file1.text" "/Alice/alice1/file2.text"
		    "/Alice/alice2/file1.text" "/Alice/alice2/file3.text"
		    "/Bob/bob1/file1.text")
		  nil)
		 (list "file1.text<Alice/alice1/>"
		       "file1.text<Alice/alice2/>"
		       "file1.text<Bob/bob1/>"
		       "file2.text"
		       "file3.text")))

  (should (equal (uniq-file--uniquify '("/Alice/alice1/file1.text" "/Alice/alice2/file1.text") nil)
		 (list "file1.text<alice1/>" "file1.text<alice2/>")))

  (should (equal (uniq-file--uniquify '("/alice1/file2.text") nil)
		 (list "file2.text")))

  (should (equal (uniq-file--uniquify
		  '("c:/tmp/test/alice-1/bar-file1.text"
		    "c:/tmp/test/alice-1/bar-file2.text")
		  "a-1")
		 (list "bar-file1.text<alice-1/>" "bar-file2.text<alice-1/>")))

  (should (equal (uniq-file--uniquify
		  '("c:/tmp/Alice/alice-1/bar-file1.text"
		    "c:/tmp/Alice/alice-1/bar-file2.text"
		    "c:/tmp/Alice/alice-2/bar-file2.text")
		  "a-")

		 ;; FIXME: This result reflects a bug in
		 ;; `completion-pcm--pattern->regex'; "a-" becomes
		 ;; "a.*?-", but it should be (concat "a[^"
		 ;; wildcards "]*-".

		 (list "bar-file1.text<Alice/alice-1/>"
		       "bar-file2.text<Alice/alice-1/>"
		       "bar-file2.text<Alice/alice-2/>")))

  (should (equal (uniq-file--uniquify
		  '("c:/tmp/Alice/alice-1/bar-file1.text"
		    "c:/tmp/Alice/alice-1/bar-file2.text"
		    "c:/tmp/Alice/alice-2/bar-file2.text")
		  "Al/a-")
		 (list "bar-file1.text<Alice/alice-1/>"
		       "bar-file2.text<Alice/alice-1/>"
		       "bar-file2.text<Alice/alice-2/>")))

  ;; From "foo-file1.text<>"
  (should (equal (uniq-file--uniquify
		  (list
		   (concat uft-alice1 "/foo-file1.text")
		   (concat uft-alice2 "/foo-file1.text")
		   (concat uft-bob1 "/foo-file1.text")
		   (concat uft-bob2 "/foo-file1.text")
		   (concat uft-root "/foo-file1.text")
		   )
		  "")
		 '(
		   "foo-file1.text<Alice/alice-1/>"
		   "foo-file1.text<Alice/alice-2/>"
		   "foo-file1.text<Bob/bob-1/>"
		   "foo-file1.text<Bob/bob-2/>"
		   "foo-file1.text<>"
		   )))

  ;; from cedet-global-test
  (should (equal (uniq-file--uniquify
		  (list
		   (concat uft-alice1 "/bar-file1.c")
		   (concat uft-alice1 "/bar-file2.c")
		   (concat uft-alice2 "/bar-file1.c")
		   (concat uft-alice2 "/bar-file2.c")
		   (concat uft-bob1 "/foo-file1.c") ;; 'b' in directory part; accidental match
		   (concat uft-bob1 "/foo-file2.c")
		   (concat uft-bob2 "/foo-file1.c")
		   (concat uft-bob2 "/foo-file5.c")
		   )
		  nil)
		 '(
		   "bar-file1.c<alice-1/>"
		   "bar-file1.c<alice-2/>"
		   "bar-file2.c<alice-1/>"
		   "bar-file2.c<alice-2/>"
		   "foo-file1.c<bob-1/>"
		   "foo-file1.c<bob-2/>"
		   "foo-file2.c"
		   "foo-file5.c"
		   )))
  )

(provide 'uniquify-files-test)
;;; uniquify-files-test.el ends here
