;;; diff-lisp.el --- Create diff in purse lisp -*- lexical-binding: t -*-


;; Copyright (C) 2021 Chen Bin
;;
;; Version: 0.0.1
;; Keywords: convenience patch diff vc
;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
;; URL: https://github.com/redguardtoo/diff-lisp
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run "git help diff" to see summary of four diff algorithms,
;; default, myers
;;     The basic greedy diff algorithm. Currently, this is the default.

;; minimal
;;     Spend extra time to make sure the smallest possible diff is produced.

;; patience
;;     Use "patience diff" algorithm when generating patches.

;; histogram
;;     This algorithm extends the patience algorithm to "support
;;     low-occurrence common elements".
;; @see xdl_change_compact in xdiffi.c from git for pretty diff output
;; @see xdl_emit_diff, it print out hunks, it's assigned to "ef" in xdl_diff
;; in gdb, use "run --no-pager diff --no-index --diff-algorithm=myers ~/projs/diff-lisp/tests/v1.txt ~/projs/diff-lisp/tests/v2.txt" to debug git diff

;;; Code:
(require 'files)
(require 'diff-lisp-sdk)
(require 'diff-lisp-myers)

(defgroup diff-lisp nil
  "Create diff in purse lisp."
  :group 'convenience)

(defcustom diff-lisp-output-unified-context 3
  "Output NUM (default 3) lines of unified context."
  :group 'diff-lisp
  :type 'integer)

;; {{ make linter happy
(defvar evil-state)
;; }}
;; Step 1: Select a region and `M-x diff-lisp-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-lisp-compare-with-b'
;; Press "q" in evil-mode or "C-c C-c" to exit the diff output buffer
(defun diff-lisp-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E."
  (if (> b e) (cl-rotatef b e))

  ;; select lines
  (save-excursion
    ;; Another workaround for evil-visual-line bug:
    ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
    ;; the (line-beginning-position) of the line which is after the last selected
    ;; line is always (region-end)! Don't know why.
    (when (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
      (setq e (1- e)))
    (goto-char b)
    (setq b (line-beginning-position))
    (goto-char e)
    (setq e (line-end-position)))
  (list b e))

(defmacro diff-lisp-open-diff-output (content buffer-name)
  "Insert CONTENT into a buffer named BUFFER-NAME."
  `(let ((rlt-buf (get-buffer-create ,buffer-name)))
    (save-current-buffer
      (switch-to-buffer-other-window rlt-buf)
      (set-buffer rlt-buf)
      (erase-buffer)
      (insert ,content)
      ;; `ffip-diff-mode' is more powerful than `diff-mode'
      (if (fboundp 'ffip-diff-mode) (ffip-diff-mode) (diff-mode))
      (goto-char (point-min)))))

(defun diff-lisp-tag-selected-as-a ()
  "Select a region to compare."
  (interactive)
  (cond
   ((region-active-p)
    (let* ((tmp (diff-lisp-format-region-boundary (region-beginning) (region-end)))
           (buf (get-buffer-create "*diff-lispA*")))
      ;; select lines
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp)))
    (message "Now select the other text to compare and run `diff-lisp-compare-with-b'"))

   (t
    (message "Please select the text first."))))

(defun diff-lisp-compare-with-b ()
  "Compare current region with region from `diff-lisp-tag-selected-as-a'.
If no region is selected, `kill-ring' or clipboard is used instead."
  (interactive)
  (let* (rlt-buf
         cmd
         diff-output
         tmp
         ;; file A
         (fa (make-temp-file (expand-file-name "diff-lisp"
                                               (or small-temporary-file-directory
                                                   temporary-file-directory))))
         ;; file B
         (fb (make-temp-file (expand-file-name "diff-lisp"
                                               (or small-temporary-file-directory
                                                   temporary-file-directory)))))
    (when (and fa (file-exists-p fa) fb (file-exists-p fb))
      (cond
       ((region-active-p)
        ;; text from selected region
        (setq tmp (diff-lisp-format-region-boundary (region-beginning) (region-end)))
        (write-region (car tmp) (cadr tmp) fb))

       (t
        ;; text from `kill-ring' or clipboard
        (let* ((choice (completing-read "Since no region selected, compare text in:"
                                        '("kill-ring" "clipboard")))
               (txt (cond
                     ((string= choice "kill-ring")
                      (car kill-ring))
                     ((string= choice "clipboard")
                      (my-gclip)))))
          (with-temp-file fb
            (insert txt)))))

      ;; save region A as file A
      (save-current-buffer
        (set-buffer (get-buffer-create "*diff-lispA*"))
        (write-region (point-min) (point-max) fa))


      ;; diff NOW!
      ;; prepare diff command, I prefer "git diff" because it has more features
      (cond
       ((executable-find "git")
        (setq cmd (format "git diff --no-index --histogram --ignore-cr-at-eol -b -w -B \"%s\" \"%s\"" fa fb)))

       ((executable-find diff-command)
        (setq cmd (format "%s -u -b -w -B \"%s\" \"%s\"" diff-command fa fb))))

      (cond
       ((not cmd)
        (message "Please add git or diff into environment variable PATH first!"))

       ((string= (setq diff-output (shell-command-to-string cmd)) "")
        (message "Two regions are SAME!"))

       (t
        ;; show the diff output
        (diff-lisp-open-diff-output diff-output "*diff-lisp-output*")))

      ;; clean the temporary files
      (if (and fa (file-exists-p fa)) (delete-file fa))
      (if (and fb (file-exists-p fb)) (delete-file fb)))))

(defun diff-lisp-get-hunk-start (snake beginning end start-line)
  "Get hunk start from SNAKE which has BEGINNING and END.
START-LINE is possibly the line number before current hunk."
  (let* ((hunk-start (cond
                      ((<= start-line (nth beginning snake))
                       (- start-line diff-lisp-output-unified-context))
                      (t
                       (- (1+ (nth end snake)) diff-lisp-output-unified-context)))))
    (if (< hunk-start start-line) start-line hunk-start)))

(defun diff-lisp-get-hunk-end (snake beginning end end-line)
  "Get hunk end from SNAKE which has BEGINNING and END.
END-LINE is possibly the line number after current hunk."
  (let* ((hunk-end (cond
                      ((> end-line (nth end snake))
                       end-line)
                      (t
                       (+ (nth beginning snake) diff-lisp-output-unified-context)))))
    (1+ (if (> hunk-end end-line) end-line hunk-end))))

(defun diff-lisp-snakes-to-hunks (snakes n m)
  "Convert SNAKES to hunks.  M and N are the length of sequences to compare.
Numbers are zero-originated in the change."
  (let* (rlt
         (i 0)
         (a-start 0)
         (b-start 0)
         current
         change
         (snakes-length (length snakes)))
    (while (< i snakes-length)
      (setq current (nth i snakes))
      (setq change (list a-start
                         b-start
                         (nth 0 current)
                         (nth 1 current)))
      (push change rlt)
      ;; the next change start from the end of current snake
      (setq a-start (nth 2 current))
      (setq b-start (nth 3 current))
      (setq i (1+ i)))

    ;; manually add last change
    (when (or (> n a-start)
              (> m b-start))
      (push (list a-start b-start n m) rlt))

    (setq rlt (nreverse rlt))

    ;; first hunk could be empty
    (when (and (> (length rlt) 0))
      (let ((first-hunk (car rlt)))
        (when (and (eq (nth 2 first-hunk) 0)
                   (eq (nth 3 first-hunk) 0))
          (setq rlt (cdr rlt)))))

    rlt))

(defun diff-lisp-change-compact (hunks a b)
  "Similar to xdl_change_compact in git."
  hunks)

(defun diff-lisp-emit-diff (hunks a n b m)
  "Similar to xdl_emit_diff in git."
  (let* ((rlt (format "--- s1\n+++ s2\n"))
         (chg-header (format "@@ -%s,%s +%s,%s @@\n"  1 n 1 m))
         i
         first-hunk)

    (setq rlt (concat rlt chg-header))
    (when hunks
      ;; output the context before the first hunk
      (setq i 0)
      (setq first-hunk (car hunks))
      (while (< i (car first-hunk))
        ;; a/b is sequence, but possibly not list
        (setq rlt (concat rlt (elt a i) "\n"))
        (setq i (1+ i)))

      (dolist (hunk hunks)
        ;; print out a hunk
        (setq i (nth 0 hunk))
        (while (< i (nth 2 hunk))
          (setq rlt (concat rlt "-" (elt a i) "\n"))
          (setq i (1+ i)))

        ;; print out b hunk
        (setq i (nth 1 hunk))
        (while (< i (nth 3 hunk))
          (setq rlt (concat rlt "+" (elt b i) "\n"))
          (setq i (1+ i)))

        ;; todo, output some context after last hunk
        ))
    rlt))

(defun diff-lisp-diff-strings (s1 s2)
  "Diff string S1 and string S2."
  (let* ((a (split-string s1 "\n"))
         (b (split-string s2 "\n"))
         (a-length (length a))
         (b-length (length b))
         (snakes (diff-lisp-myers-do-diff a a-length b b-length))
         hunks)

    (setq hunks
          (diff-lisp-change-compact (diff-lisp-snakes-to-hunks snakes a-length b-length)
                                    a
                                    b))

    ;; (message "---hunks=%s" hunks)

    ;; every snake is a list like "(x y u v)"
    ;; x,y is the start point of snake; u,y is the end point of snake
    (message "rlt=%s" (diff-lisp-emit-diff hunks a a-length b b-length))
    ))

(provide 'diff-lisp)
;;; diff-lisp.el ends here
