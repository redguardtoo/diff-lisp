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
(require 'subr-x)
(require 'diff-lisp)


(defun test-create-v (n m)
  "Create vector from N and M."
  (make-vector (+ n m 1) nil))

(defun test-read-file-into-string (file)
  "Read FILE's content into string."
  (let* ((files (directory-files-recursively default-directory file)))
    (diff-lisp-file-to-string (car files))))

(defun test-strip-diff-header (diff)
  "Strip DIFF header."
  (let* ((a (split-string diff "\n")))
    (setq a (cddr a))
    (string-trim (mapconcat 'identity a "\n"))))

(defun test-eq (l1 l2)
  "Two list L1 and L2 have same content."
  (let* (rlt
         (i 0)
         (len (length l1))
         stop)
     (when (= len (length l2))
       (while (and (not stop) (< i len))
         (unless (= (nth i l1) (nth i l2))
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
    (should (= 5 (plist-get rlt :difference)))
    (setq rlt (diff-lisp-myers-find-all-snakes a 0 (length a) b 0 (length b)))
    (should (= (length rlt) 4))
    (setq rlt (diff-lisp-myers-do-diff a (length a) b (length b)))
    (should (= (length rlt) 3))))

(ert-deftest test-compare-files ()
  (let* ((f1 "git-send-email-v1.perl")
         (f2 "git-send-email-v2.perl")
         (fp1 (concat "tests/" f1))
         (fp2 (concat "tests/" f2))
         (s1 (test-read-file-into-string f1))
         (s2 (test-read-file-into-string f2))
         (output (shell-command-to-string (format "diff -u %s %s" fp1 fp2))))
    (should (string= (test-strip-diff-header output)
                     (test-strip-diff-header (diff-lisp-diff-strings s1 s2))))

    (should (string= (test-strip-diff-header (diff-lisp-diff-files fp1 fp2))
                     (test-strip-diff-header output)))))

(ert-deftest test-b-is-longer-than-a ()
  (let* ((a (string-to-list "abcd"))
         (b (string-to-list "abczacd"))
         rlt)
    (setq rlt (diff-lisp-myers-find-all-snakes a 0 (length a) b 0 (length b)))
    (should (= (length rlt) 4))
    (setq rlt (diff-lisp-myers-do-diff a  (length a) b (length b)))
    (should (= (length rlt) 2))
    (should (test-eq (nth 0 rlt) '(0 0 3 3)))
    (should (test-eq (nth 1 rlt) '(3 6 4 7)))))

(ert-deftest test-no-difference ()
  (let* ((a (string-to-list "abcd"))
         (rlt (diff-lisp-myers-do-diff a  (length a) a (length a))))
    (should (= (length rlt) 1))
    (should (test-eq (nth 0 rlt) (list 0 0 (length a) (length a))))))

(ert-deftest test-middle-snake-odd-sum-no-crash-and-snake ()
  "MRE: When n+m is odd, original code may crash due to float indices or reading nil from v2[k].
After fix, middle snake should be returned correctly."
  (let* ((a (string-to-list "ab"))   ;; n=2
         (b (string-to-list "a"))    ;; m=1, n+m=3 (odd), delta=1 (odd)
         signaled
         s)
    ;; In buggy version: this likely signals due to float index or (>= x nil)
    (setq signaled
          (condition-case _
              (progn
                (setq s (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b)))
                nil)
            (error t)))
    ;; After fix: should NOT signal
    (should (not signaled))
    ;; Expected middle snake for common subsequence 'a'
    (should (test-eq (plist-get s :snake) '(2 1 2 1)))
    ;; Edit distance should be 1
    (should (= (plist-get s :difference) 1))))

(ert-deftest test-middle-snake-odd-sum-longer ()
  "MRE: Another odd n+m case to exercise forward/backward overlap logic."
  (let* ((a (string-to-list "abcd"))  ;; n=4
         (b (string-to-list "abc"))   ;; m=3, n+m=7 (odd), delta=1 (odd)
         signaled
         s)
    (setq signaled
          (condition-case _
              (progn
                (setq s (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b)))
                nil)
            (error t)))
    ;; After fix: should NOT signal
    (should (not signaled))
    ;; Conservative invariants on snake coordinates
    (let* ((snake (plist-get s :snake))
           (x (nth 0 snake)) (y (nth 1 snake))
           (u (nth 2 snake)) (v (nth 3 snake)))
      (should (and (<= 0 x u) (<= 0 y v)))
      (should (and (<= u (length a)) (<= v (length b)))))))

(ert-deftest test-forward-overlap-reads-wrong-v2-index ()
  "MRE: Delta is odd; forward overlap must read v2[k+delta], not v2[k].
Buggy code reading v2[k] can return nil and crash on comparison."
  (let* ((a (string-to-list "cab"))   ;; n=3
         (b (string-to-list "ab"))    ;; m=2, n+m=5 (odd), delta=1 (odd)
         signaled
         s)
    (setq signaled
          (condition-case _
              (progn
                (setq s (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b)))
                nil)
            (error t)))
    ;; After fix: should NOT signal and return a valid 4-tuple
    (should (not signaled))
    (let ((snake (plist-get s :snake)))
      (should (consp snake))
      (should (= (length snake) 4)))))

(ert-deftest test-do-diff-odd-delta-basic ()
  "End-to-end MRE: odd delta case should not crash inside middle-snake and should yield a single common snake."
  (let* ((a (string-to-list "ab"))
         (b (string-to-list "a"))
         signaled
         rlt)
    (setq signaled
          (condition-case _
              (progn
                (setq rlt (diff-lisp-myers-do-diff a (length a) b (length b)))
                nil)
            (error t)))
    ;; After fix: should NOT signal
    (should (not signaled))
    ;; Only the common 'a' snake should be present
    (should (= (length rlt) 1))
    (should (test-eq (car rlt) '(0 0 1 1)))))

(ert-deftest test-even-delta-stability ()
  "Regression guard: even delta should produce stable path selection (tie-breaking uses strict <)."
  (let* ((a (string-to-list "axbxc"))  ;; LCS is 'abc'
         (b (string-to-list "aybzc"))
         (rlt (diff-lisp-myers-do-diff a (length a) b (length b))))
    ;; We expect at least two snakes (sequence may split LCS into multiple snakes)
    (should (>= (length rlt) 2))
    ;; Each snake must be non-decreasing and within bounds
    (dolist (s rlt)
      (let ((x (nth 0 s)) (y (nth 1 s)) (u (nth 2 s)) (v (nth 3 s)))
        (should (and (<= 0 x u) (<= 0 y v)))
        (should (and (<= u (length a)) (<= v (length b))))))))

(ert-run-tests-batch-and-exit)
;;; diff-lisp-tests.el ends here
