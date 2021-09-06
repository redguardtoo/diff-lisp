;;; diff-lisp-sdk.el --- sdk -*- lexical-binding: t -*-

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

;; 

;;; Code:
(defvar diff-lisp-debug nil "Debug flag.")

(defmacro diff-lisp-string-equal (a b x y)
  "Test if string A[X] equals string B[Y]."
  `(equal (nth ,x ,a) (nth ,y ,b)))

(defun diff-lisp-show-v (v half-v-size)
  "Show content of V.  HALF-V-SIZE is used to get k diagonal."
  (let* ((i 0)
         (len (length v))
         k
         x
         rlt)
    (while (< i len)
      (setq x (aref v i))
      (when x
        (setq k (- i half-v-size))
        (push (format "k=%s end=(%s,%s)" k x (- x k)) rlt))
      (setq i (1+ i)))
    (nreverse rlt)))

(defun diff-lisp-file-to-string (file)
  "Read FILE into string."
  (cond
   ((and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))
   (t
    "")))

(provide 'diff-lisp-sdk)
;;; diff-lisp-sdk.el ends here
