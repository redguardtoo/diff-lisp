;; diff-lisp-tests.el --- unit tests for diff-lisp -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'diff-lisp)

(defun test-create-v (n m)
  "Create vector from N and M."
  (make-vector (+ n m 1) nil))

(defun test-read-file-into-string (file)
  "Read FILE's content into string."
  (let* ((files (directory-files-recursively default-directory file)))
    (with-temp-buffer
      (when files
        (insert-file-contents (car files)))
      (buffer-string))))

(defun test-eq (l1 l2)
  "Two list L1 and L2 have same content."
  (let* (rlt
         (i 0)
         (len (length l1))
         stop)
     (when (eq len (length l2))
       (while (and (not stop) (< i len))
         (unless (eq (nth i l1) (nth i l2))
           (setq stop t))
         (setq i (1+ i)))
       (unless stop
         (setq rlt t)))
    rlt))

(ert-deftest test-find-middle-snake ()
  (let* ((a (string-to-list "d2a1c3b"))
         (b (string-to-list "D2A1C3B"))
         (s (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b))))
    (should (test-eq (plist-get s :snake) '(3 3 4 4)))
    (should (> (plist-get s :difference) 1))

    (setq s (diff-lisp-myers-find-middle-snake a 0 3 b 0 3))
    (should (test-eq (plist-get s :snake) '(1 1 2 2)))

    (setq s (diff-lisp-myers-find-middle-snake a 4 (- 7 4) b 4 (- 7 4)))
    (should (test-eq (plist-get s :snake) '(1 1 2 2)))))

(ert-deftest test-compare-basic-string ()
  (let* ((a (string-to-list "ABCABBA"))
         (b (string-to-list "CBABAC"))
         (rlt (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b))))
    (should (eq 5 (plist-get rlt :difference)))
    (setq rlt (diff-lisp-myers-find-all-snakes a 0 (length a) b 0 (length b)))
    (message "basic rlt=%s" rlt)
    (should (eq (length rlt) 4))
    (setq rlt (diff-lisp-myers-do-diff a (length a) b (length b)))
    (should (eq (length rlt) 3))))

(ert-deftest test-compare-files ()
  (let* ((s1 (test-read-file-into-string "git-send-email-v1.perl"))
         (s2 (test-read-file-into-string "git-send-email-v2.perl")))
    (diff-lisp-diff-strings s1 s2)
    (should t)))

(ert-deftest test-b-is-longer-than-a ()
  (let* ((a (string-to-list "abcd"))
         (b (string-to-list "abczacd"))
         rlt)
    (setq rlt (diff-lisp-myers-find-all-snakes a 0 (length a) b 0 (length b)))
    (should (eq (length rlt) 4))
    (setq rlt (diff-lisp-myers-do-diff a  (length a) b (length b)))
    (should (eq (length rlt) 2))
    (should (test-eq (nth 0 rlt) '(0 0 3 3)))
    (should (test-eq (nth 1 rlt) '(3 6 4 7)))))

(ert-run-tests-batch-and-exit)
;;; diff-lisp-tests.el ends here
