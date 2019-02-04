;;; file-complete-root-relative-test.el - Test for file-complete-root-relative.el -*- lexical-binding:t no-byte-compile:t -*-
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

(require 'ert)
(require 'uniquify-files-test) ;; We share the test directory tree.
(require 'file-complete-root-relative)

(defconst fc-root-rel-iter (make-path-iterator :user-path-recursive (list uft-root)))

(defconst fc-root-rel-file-list
  (list
    (concat uft-root "/foo-file1.text")
    (concat uft-root "/foo-file3.texts2")
    (concat uft-root "/Alice/alice-1/bar-file1.text")
    (concat uft-root "/Alice/alice-1/bar-file2.text")
    (concat uft-root "/Alice/alice-1/foo-file1.text")
    (concat uft-root "/Alice/alice-1/foo-file2.text")
    (concat uft-root "/Alice/alice-2/bar-file1.text")
    (concat uft-root "/Alice/alice-2/bar-file2.text")
    (concat uft-root "/Alice/alice-2/foo-file1.text")
    (concat uft-root "/Alice/alice-2/foo-file3.text")
    (concat uft-root "/Alice/alice-2/foo-file3.texts")
    (concat uft-root "/Alice/alice-3/foo-file4.text")
    (concat uft-root "/Bob/alice-3/foo-file4.text")
    (concat uft-root "/Bob/bob-1/foo-file1.text")
    (concat uft-root "/Bob/bob-1/foo-file2.text")
    (concat uft-root "/Bob/bob-2/foo-file1.text")
    (concat uft-root "/Bob/bob-2/foo-file5.text")
    ))

(ert-deftest test-fc-root-rel-completion-table-iter ()
  "Test basic functions of table."
  ;; grouped by action
  (should (equal (fc-root-rel-completion-table-iter fc-root-rel-iter "fi" nil '(boundaries . ".text"))
		   '(boundaries . (0 . 5))))

  (should (equal (fc-root-rel-completion-table-iter fc-root-rel-iter "fi" nil 'metadata)
		 (cons 'metadata
		       (list
			'(category . project-file)
			'(styles   . (file-root-rel))
			(cons 'root uft-root)))))

  ;; all-completions. We sort the results here to make the test stable
  (should (equal (sort (fc-root-rel-completion-table-iter fc-root-rel-iter "" nil t) #'string-lessp)
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
 		  (concat uft-Bob-alice3 "/foo-file4.text")
		  (concat uft-bob1 "/foo-file1.text")
		  (concat uft-bob1 "/foo-file2.text")
		  (concat uft-bob2 "/foo-file1.text")
		  (concat uft-bob2 "/foo-file5.text")
		  (concat uft-root "/foo-file1.text")
		  (concat uft-root "/foo-file3.texts2")
		  )))

  (should (equal (sort (fc-root-rel-completion-table-iter fc-root-rel-iter "a-1/f-fi" nil t) #'string-lessp)
		 (list
		  (concat uft-alice1 "/foo-file1.text")
		  (concat uft-alice1 "/foo-file2.text")
		  )))

  (should (equal (fc-root-rel-completion-table-iter fc-root-rel-iter "file1.text<uft-alice1/>" nil t)
		 ;; some caller did not deuniquify; treated as misspelled; no match
		 nil))


  ;; This table does not implement try-completion
  (should (equal (fc-root-rel-completion-table-iter fc-root-rel-iter "fi" nil nil)
		 nil))

  ;; test-completion
  (should (equal (fc-root-rel-completion-table-iter
		  fc-root-rel-iter
		  (fc-root-rel-to-table-input "alice-1/foo-file1.text") nil 'lambda)
		 nil)) ;; not at root

  (should (equal (fc-root-rel-completion-table-iter
		  fc-root-rel-iter
		  (fc-root-rel-to-table-input "Alice/alice-1/foo-file1.text") nil 'lambda)
		 t)) ;; at root

  )

(ert-deftest test-fc-root-rel-completion-table-list ()
  "Test basic functions of table."
  ;; grouped by action
  (should (equal (fc-root-rel-completion-table-list fc-root-rel-file-list uft-root "fi" nil '(boundaries . ".text"))
		   '(boundaries . (0 . 5))))

  (should (equal (fc-root-rel-completion-table-list fc-root-rel-file-list uft-root "fi" nil 'metadata)
		 (cons 'metadata
		       (list
			'(category . project-file)
			'(styles   . (file-root-rel))
			(cons 'root uft-root)))))

  ;; all-completions. We sort the results here to make the test stable
  (should (equal (sort (fc-root-rel-completion-table-list fc-root-rel-file-list uft-root "" nil t) #'string-lessp)
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
 		  (concat uft-Bob-alice3 "/foo-file4.text")
		  (concat uft-bob1 "/foo-file1.text")
		  (concat uft-bob1 "/foo-file2.text")
		  (concat uft-bob2 "/foo-file1.text")
		  (concat uft-bob2 "/foo-file5.text")
		  (concat uft-root "/foo-file1.text")
		  (concat uft-root "/foo-file3.texts2")
		  )))

  (should (equal (sort (fc-root-rel-completion-table-list
			fc-root-rel-file-list uft-root "a-1/f-fi" nil t)
		       #'string-lessp)
		 (list
		  (concat uft-alice1 "/foo-file1.text")
		  (concat uft-alice1 "/foo-file2.text")
		  )))

  (should (equal (fc-root-rel-completion-table-list fc-root-rel-file-list uft-root "uft-alice1/file1.text" nil t)
		 ;; misspelled; no match
		 nil))

  ;; This table does not implement try-completion
  (should (equal (fc-root-rel-completion-table-list fc-root-rel-file-list uft-root "fi" nil nil)
		 nil))

  ;; test-completion
  (should (equal (fc-root-rel-completion-table-list
		  fc-root-rel-file-list uft-root
		  (fc-root-rel-to-table-input "alice-1/foo-file1.text") nil 'lambda)
		 nil)) ;; not at root

  (should (equal (fc-root-rel-completion-table-iter
		  fc-root-rel-iter
		  (fc-root-rel-to-table-input "Alice/alice-1/foo-file1.text") nil 'lambda)
		 t)) ;; at root
  )

(defun test-fc-root-rel-test-completion-1 (table)
  (should (equal (test-completion "foo-fi" table)
		 nil))

  (should (equal (test-completion "dir/f-fi" table)
		 nil))

  (should (equal (test-completion "foo-file1.text" table)
		 t)) ;; starts at root

  (should (equal (test-completion "alice-1/foo-file1.text" table)
		 nil)) ;; does not start at root

  (should (equal (test-completion "Alice/alice-1/foo-file1.text" table)
		 t)) ;; starts at root

  (should (equal (test-completion "foo-file3.text" table)
		 nil))

  (should (equal (test-completion "foo-file3.texts2" table)
		 t))

  (should (equal (test-completion "Alice/alice-/bar-file2.text" table)
		 nil))

  (should (equal (test-completion "Alice/alice-1/bar-file2.text" table)
		 t))
  )

(ert-deftest test-fc-root-rel-test-completion-iter ()
  (let ((table (apply-partially 'fc-root-rel-completion-table-iter fc-root-rel-iter))
	(completion-category-overrides '(project-file (styles . file-root-rel))))
    (test-fc-root-rel-test-completion-1 table)))

(ert-deftest test-fc-root-rel-test-completion-list ()
  (let ((table (apply-partially 'fc-root-rel-completion-table-list fc-root-rel-file-list uft-root))
	(completion-category-overrides '(project-file (styles . file-root-rel))))
    (test-fc-root-rel-test-completion-1 table)))

(defun test-fc-root-rel-all-completions-noface-1 (table)
  (should (equal
	   (sort (fc-root-rel-all-completions "" table nil nil) #'string-lessp)
	   (list
	    "Alice/alice-1/bar-file1.text"
	    "Alice/alice-1/bar-file2.text"
	    "Alice/alice-1/foo-file1.text"
	    "Alice/alice-1/foo-file2.text"
	    "Alice/alice-2/bar-file1.text"
	    "Alice/alice-2/bar-file2.text"
	    "Alice/alice-2/foo-file1.text"
	    "Alice/alice-2/foo-file3.text"
	    "Alice/alice-2/foo-file3.texts"
 	    "Alice/alice-3/foo-file4.text"
 	    "Bob/alice-3/foo-file4.text"
	    "Bob/bob-1/foo-file1.text"
	    "Bob/bob-1/foo-file2.text"
	    "Bob/bob-2/foo-file1.text"
	    "Bob/bob-2/foo-file5.text"
	    "foo-file1.text"
	    "foo-file3.texts2"
	    )))

  (should (equal
	   (sort (fc-root-rel-all-completions "*-fi" table nil nil) #'string-lessp)
	   (list
	    "Alice/alice-1/bar-file1.text"
	    "Alice/alice-1/bar-file2.text"
	    "Alice/alice-1/foo-file1.text"
	    "Alice/alice-1/foo-file2.text"
	    "Alice/alice-2/bar-file1.text"
	    "Alice/alice-2/bar-file2.text"
	    "Alice/alice-2/foo-file1.text"
	    "Alice/alice-2/foo-file3.text"
	    "Alice/alice-2/foo-file3.texts"
 	    "Alice/alice-3/foo-file4.text"
 	    "Bob/alice-3/foo-file4.text"
	    "Bob/bob-1/foo-file1.text"
	    "Bob/bob-1/foo-file2.text"
	    "Bob/bob-2/foo-file1.text"
	    "Bob/bob-2/foo-file5.text"
	    "foo-file1.text"
	    "foo-file3.texts2"
	    )))

  (should (equal
	   (sort (fc-root-rel-all-completions "b" table nil nil) #'string-lessp)
	   nil))

  (let ((completion-ignore-case t))
    (should (equal
	     (sort (fc-root-rel-all-completions "b" table nil nil) #'string-lessp)
	     	   (list
 		    "Bob/alice-3/foo-file4.text"
		    "Bob/bob-1/foo-file1.text"
		    "Bob/bob-1/foo-file2.text"
		    "Bob/bob-2/foo-file1.text"
		    "Bob/bob-2/foo-file5.text"
		    )))
    )

  (should (equal
	   (sort (fc-root-rel-all-completions "*/foo" table nil nil) #'string-lessp)
	   (list
	    "Alice/alice-1/foo-file1.text"
	    "Alice/alice-1/foo-file2.text"
	    "Alice/alice-2/foo-file1.text"
	    "Alice/alice-2/foo-file3.text"
	    "Alice/alice-2/foo-file3.texts"
 	    "Alice/alice-3/foo-file4.text"
 	    "Bob/alice-3/foo-file4.text"
	    "Bob/bob-1/foo-file1.text"
	    "Bob/bob-1/foo-file2.text"
	    "Bob/bob-2/foo-file1.text"
	    "Bob/bob-2/foo-file5.text"
	    )))

  (should (equal
	   (sort (fc-root-rel-all-completions "Alice/alice-1/" table nil nil) #'string-lessp)
	   (list
	    "Alice/alice-1/bar-file1.text"
	    "Alice/alice-1/bar-file2.text"
	    "Alice/alice-1/foo-file1.text"
	    "Alice/alice-1/foo-file2.text"
	    )))

  (should (equal
	   (sort (fc-root-rel-all-completions "Alice/alice-1/f-file2" table nil nil) #'string-lessp)
	   (list
	    "Alice/alice-1/foo-file2.text"
	    )))
  )

(ert-deftest test-fc-root-rel-all-completions-noface-iter ()
  (let ((table (apply-partially 'fc-root-rel-completion-table-iter fc-root-rel-iter))
	(completion-category-overrides '(project-file (styles . file-root-rel)))
	(completion-ignore-case nil))
    (test-fc-root-rel-all-completions-noface-1 table)))

(ert-deftest test-fc-root-rel-all-completions-noface-list ()
  (let ((table (apply-partially 'fc-root-rel-completion-table-list fc-root-rel-file-list uft-root))
	(completion-category-overrides '(project-file (styles . file-root-rel)))
	(completion-ignore-case nil))
    (test-fc-root-rel-all-completions-noface-1 table)))

;; FIXME: more tests

(provide 'file-complete-root-relative-test)
;;; file-complete-root-relative-test.el ends here
