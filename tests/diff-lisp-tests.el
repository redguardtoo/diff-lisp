;; diff-lisp-tests.el --- unit tests for diff-lisp -*- coding: utf-8 -*-

;; Author: Chen Bin

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

(defun test-read-file-into-string (file)
  "Read FILE's content into string."
  (let* ((files (directory-files-recursively default-directory file)))
    (diff-lisp-file-to-string (car files))))

(defun test-strip-diff-header (diff)
  "Strip DIFF header."
  (let* ((a (split-string diff "\n")))
    (setq a (cddr a))
    (string-trim (mapconcat 'identity a "\n"))))

(ert-deftest test-find-middle-snake ()
  (let* ((a (string-to-vector "d2a1c3b"))
         (b (string-to-vector "D2A1C3B"))
         (snake (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b))))

    (pcase-let ((`(,x ,y ,u ,v ,d) snake))
      (should (= x 3))
      (should (= y 3))
      (should (= u 4))
      (should (= v 4))
      (should (> d 1)))

    (setq snake (diff-lisp-myers-find-middle-snake a 0 3 b 0 3))
    (pcase-let ((`(,x ,y ,u ,v) snake))
      (should (= x 1))
      (should (= y 1))
      (should (= u 2))
      (should (= v 2)))

    (setq snake (diff-lisp-myers-find-middle-snake a 4 (- 7 4) b 4 (- 7 4)))
    (pcase-let ((`(,x ,y ,u ,v) snake))
      (should (= x 1))
      (should (= y 1))
      (should (= u 2))
      (should (= v 2)))))

(ert-deftest test-line-to-hash ()
  (let* ((diff-lisp-ignore-whitespace nil)
         (diff-lisp-ignore-whitespace-change nil))

    ;; can produce normal hash
    (should (> (diff-lisp-hash-record "abcd") 0))
    (should (/= (diff-lisp-hash-record "abcd") (diff-lisp-hash-record "abce")))
    (should (/= (diff-lisp-hash-record "abcd") (diff-lisp-hash-record "ab cd")))
    (should (/= (diff-lisp-hash-record "ab cd") (diff-lisp-hash-record "ab  cd")))

    (should (/= (diff-lisp-hash-record-with-whitespace "abcd")
                (diff-lisp-hash-record-with-whitespace "ab cd")))
    (should (/= (diff-lisp-hash-record-with-whitespace "a b c d")
                (diff-lisp-hash-record-with-whitespace "abcd ")))
    (should (/= (diff-lisp-hash-record-with-whitespace "ab cd")
                (diff-lisp-hash-record-with-whitespace "ab  cd"))))

  (let* ((diff-lisp-ignore-whitespace nil)
         (diff-lisp-ignore-whitespace-change nil))
    (should (/= (diff-lisp-hash-record-with-whitespace "abcd")
                (diff-lisp-hash-record-with-whitespace "abcd  "))))

  (let* ((diff-lisp-ignore-whitespace nil)
         (diff-lisp-ignore-whitespace-change t))
    (should (= (diff-lisp-hash-record-with-whitespace "ab cd")
               (diff-lisp-hash-record-with-whitespace "ab  cd"))))

  (let* ((diff-lisp-ignore-whitespace t)
         (diff-lisp-ignore-whitespace-change nil))
    (should (= (diff-lisp-hash-record-with-whitespace "abcd")
               (diff-lisp-hash-record-with-whitespace "ab cd")))
    (should (= (diff-lisp-hash-record-with-whitespace "a b c d")
               (diff-lisp-hash-record-with-whitespace "abcd ")))))

(ert-deftest test-compare-basic-string ()
  (let* ((a (string-to-vector "ABCABBA"))
         (b (string-to-vector "CBABAC"))
         (snake (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b))))

    (pcase-let ((`(,x ,y ,u ,v ,d) snake))
      (should (= d 5)))

    (setq snake (diff-lisp-myers-find-all-snakes a 0 (length a) b 0 (length b)))
    (should (= (length snake) 3))
    (setq snake (diff-lisp-myers-do-diff a (length a) b (length b)))
    (should (= (length snake) 3))))

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

(ert-deftest test-empty-sequences ()
  "Boundary cases when one or both sequences are empty (n = 0 or m = 0)."
  (let ((non-empty (string-to-vector "abc"))
        (empty []))
    ;; Both empty
    (should (null (diff-lisp-myers-find-all-snakes empty 0 0 empty 0 0)))
    (should (null (diff-lisp-myers-do-diff empty 0 empty 0)))

    ;; A is empty, B is non-empty (pure insertion)
    (should (null (diff-lisp-myers-find-all-snakes empty 0 0 non-empty 0 3)))
    (should (null (diff-lisp-myers-do-diff empty 0 non-empty 3)))

    ;; A is non-empty, B is empty (pure deletion)
    (should (null (diff-lisp-myers-find-all-snakes non-empty 0 3 empty 0 0)))
    (should (null (diff-lisp-myers-do-diff non-empty 3 empty 0)))))

(ert-deftest test-no-common-elements-insert-and-delete ()
  "Completely disjoint sequences should produce no non-empty matching snakes."
  (let* ((a (string-to-vector "abc"))
         (b (string-to-vector "xyz"))
         (raw-snakes (diff-lisp-myers-find-all-snakes a 0 (length a) b 0 (length b))))
    (should (null (delete nil (mapcar #'diff-lisp--not-empty-snake-p raw-snakes))))
    (should (null (diff-lisp-myers-do-diff a (length a) b (length b))))))

(ert-deftest test-b-is-longer-than-a ()
  (let* ((a (string-to-vector "abcd"))
         (b (string-to-vector "abczacd"))
         (rlt (diff-lisp-myers-find-all-snakes a 0 (length a) b 0 (length b))))
    (should (= (length rlt) 2))
    (setq rlt (diff-lisp-myers-do-diff a (length a) b (length b)))
    (should (= (length rlt) 2))
    (should (equal (nth 0 rlt) '(0 0 3 3)))
    (should (equal (nth 1 rlt) '(3 6 4 7)))))

(ert-deftest test-a-b-share-common-prefix ()
  "A prefix of B should produce a single snake covering the shared prefix without empty snakes."
  (let* ((a (string-to-vector "abc"))
         (b (string-to-vector "abcdefgh"))
         (snakes (diff-lisp-myers-find-all-snakes a 0 (length a) b 0 (length b)))
         (sorted-snakes (diff-lisp-myers-do-diff a (length a) b (length b)))
         (hunks (diff-lisp-snakes-to-hunks sorted-snakes (length a) (length b))))
    ;; Only the non-empty common-prefix snake [0..3) in A and [0..3) in B is kept
    (should (equal snakes '((0 0 3 3))))
    (should (equal sorted-snakes '((0 0 3 3))))
    ;; Remaining suffix "defgh" in B becomes a single trailing insertion hunk
    (should (equal hunks '((3 3 3 8))))))

(ert-deftest test-no-difference ()
  (let* ((a (string-to-vector "abcd"))
         (rlt (diff-lisp-myers-do-diff a  (length a) a (length a))))
    (should (= (length rlt) 1))
    (should (equal (nth 0 rlt) (list 0 0 (length a) (length a))))))

(ert-deftest test-middle-snake-odd-sum-no-crash-and-snake ()
  "When n+m is odd, original code may crash reading nil from v2[k].
After fix, middle snake should be returned correctly."
  (let* ((a (string-to-vector "ab"))   ;; n=2
         (b (string-to-vector "a"))    ;; m=1, n+m=3 (odd), delta=1 (odd)
         signaled
         snake)
    ;; In buggy version: this likely signals due to float index or (>= x nil)
    (setq signaled
          (condition-case _
              (progn
                (setq snake (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b)))
                nil)
            (error t)))
    ;; After fix: should NOT signal
    (should (not signaled))
    (pcase-let ((`(,x ,y ,u ,v ,d) snake))
      ;; Expected middle snake for common subsequence 'a'
      (should (= x 2))
      (should (= y 1))
      (should (= u 2))
      (should (= v 1))
      ;; Edit distance should be 1
      (should (= d 1)))))

(ert-deftest test-middle-snake-odd-sum-longer ()
  "Another odd n+m case to exercise forward/backward overlap logic."
  (let* ((a (string-to-vector "abcd"))  ;; n=4
         (b (string-to-vector "abc"))   ;; m=3, n+m=7 (odd), delta=1 (odd)
         signaled
         snake)
    (setq signaled
          (condition-case _
              (progn
                (setq snake (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b)))
                nil)
            (error t)))
    ;; After fix: should NOT signal
    (should (not signaled))
    ;; Conservative invariants on snake coordinates
    (pcase-let ((`(,x ,y ,u ,v) snake))
      (should (and (<= 0 x u) (<= 0 y v)))
      (should (and (<= u (length a)) (<= v (length b)))))))

(ert-deftest test-forward-overlap-reads-wrong-v2-index ()
  "MRE: Delta is odd; forward overlap must read v2[k+delta], not v2[k].
Buggy code reading v2[k] can return nil and crash on comparison."
  (let* ((a (string-to-vector "cab"))   ;; n=3
         (b (string-to-vector "ab"))    ;; m=2, n+m=5 (odd), delta=1 (odd)
         signaled
         snake)
    (setq signaled
          (condition-case _
              (progn
                (setq snake (diff-lisp-myers-find-middle-snake a 0 (length a) b 0 (length b)))
                nil)
            (error t)))
    (should (not signaled))
    (should (= (length snake) 5))))


(ert-deftest test-do-diff-odd-delta-basic ()
  "End-to-end MRE: odd delta case should not crash inside middle-snake
and should yield a single common snake."
  (let* ((a (string-to-vector "ab"))
         (b (string-to-vector "a"))
         signaled
         rlt)
    (setq signaled
          (condition-case _
              (progn
                (setq rlt (diff-lisp-myers-do-diff a (length a) b (length b)))
                nil)
            (error t)))
    (should (not signaled))
    (should (= (length rlt) 1))
    (should (equal (car rlt) '(0 0 1 1)))))


(ert-deftest test-even-delta-stability ()
  "Regression guard: even delta should produce stable path selection (tie-breaking uses strict <)."
  (let* ((a (string-to-vector "axbxc"))  ;; LCS is 'abc'
         (b (string-to-vector "aybzc"))
         (snakes (diff-lisp-myers-do-diff a (length a) b (length b))))
    ;; We expect at least two snakes (sequence may split LCS into multiple snakes)
    (should (>= (length snakes) 2))
    ;; Each snake must be non-decreasing and within bounds
    (dolist (snake snakes)
      (pcase-let ((`(,x ,y ,u ,v ,d) snake))
        (should (and (<= 0 x u) (<= 0 y v)))
        (should (and (<= u (length a)) (<= v (length b))))))))

(ert-deftest test-myers-find-all-snakes-but-no-middle-snake ()
  "No middle snake."
  (diff-lisp-myers-do-diff [1 2 3 4] 4 [5 6] 2))

(ert-deftest test-d1-edit-before-common-snake ()
  "When d = 1 and middle snake degenerates, edit before common elements
must not treat mismatched leading elements as a snake."
  (let* ((a (string-to-vector "XAB")) ;; n=3, delete 'X', common 'AB' at [1..3]
         (b (string-to-vector "AB"))  ;; m=2, common 'AB' at [0..2]
         (rlt (diff-lisp-myers-do-diff a (length a) b (length b))))
    (should (= (length rlt) 1))
    (should (equal (car rlt) '(1 0 3 2)))))

(ert-deftest test-merge-adjacent-hunks-both-sides ()
  "Adjacent hunks within unified context on both A and B should merge into one @@ block."
  (let* ((diff-lisp-output-unified-context 3)
         (s1 (mapconcat #'identity
                        '("a1" "a2" "a3" "a4" "a5"
                          "a6" "a7" "a8" "a9" "a10")
                        "\n"))
         (s2 (mapconcat #'identity
                        '("a1" "a2" "b3" "a4" "a5"
                          "a6" "b7" "a8" "a9" "a10")
                        "\n"))
         (diff (diff-lisp-diff-strings s1 s2)))
    ;; Only one "@@ ... @@" hunk header is emitted when two nearby edits merge
    (should (= (length (split-string diff "^@@.*@@$" t)) 2))
    (should (string-match-p "@@ -1,10 \\+1,10 @@" diff))))


(ert-deftest diff-lisp-test-ignore-final-newline ()
  (should
   (equal (diff-lisp-diff-strings "abc\ndef" "abc\ndef\n") ""))
  (should
   (equal (diff-lisp-diff-strings "abc" "abc\n") "")))

(ert-deftest diff-lisp-myers-test-replace-one-character ()
  (should
   (equal
    (diff-lisp-myers-do-diff
     (string-to-vector "abc") 3
     (string-to-vector "axc") 3)
    '((0 0 1 1)
      (2 2 3 3)))))

(ert-deftest diff-lisp-myers-test-insert-in-middle ()
  (should
   (equal
    (diff-lisp-myers-do-diff
     (string-to-vector "abc") 3
     (string-to-vector "axbc") 4)
    '((0 0 1 1)
      (1 2 3 4)))))

(ert-deftest diff-lisp-myers-test-delete-in-middle ()
  (should
   (equal
    (diff-lisp-myers-do-diff
     (string-to-vector "abc") 3
     (string-to-vector "ac") 2)
    '((0 0 1 1)
      (2 1 3 2)))))

(ert-run-tests-batch-and-exit)
;;; diff-lisp-tests.el ends here
