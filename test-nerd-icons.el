;;; test-nerd-icons.el<emacs-nerd-icons> --- Test nerd icons -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'nerd-icons)
(require 'ert)

;; (let ((ver-list (version-to-list emacs-version)))
;;   (setq user-emacs-directory (expand-file-name "./"))
;;   (setq package-user-dir (expand-file-name (format "./.cask/%s.%s/elpa/" (car ver-list) (car (cdr ver-list)))))
;;   (package-initialize))

(defvar test-expr nil
  "Holds a test expression to evaluate with `test-eval'.")

(defvar test-result nil
  "Holds the eval result of `test-expr' by `test-eval'.")

(defun test-eval ()
  "Evaluate `test-expr'."
  (interactive)
  (setq test-result (eval test-expr)))

(global-set-key (kbd "C-c C-c e") 'test-eval)

(defun test-with (expr keys)
  "Evaluate EXPR followed by KEYS."
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (with-current-buffer buf
        (erase-buffer))))
  (let ((test-expr expr))
    (execute-kbd-macro
     (vconcat (kbd "C-c C-c e")
              (kbd keys)))
    test-result))

(ert-deftest test-nerd-insert ()
  (should
   (equal (cdr (assoc "javascript" nerd-icons-alist/fileicon))
          (with-temp-buffer
            (test-with '(call-interactively 'nerd-icons-insert 'any) "javascript RET")
            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-nerd-icons-icon-for-file ()
  (should
   (equal (cdr (assoc "typescript" nerd-icons-alist/fileicon))
          (nerd-icons-icon-for-file "foo.ts"))))

(ert-deftest test-nerd-icons-icon-for-mode ()
  (should
   (equal (cdr (assoc "python" nerd-icons-alist/fileicon))
          (nerd-icons-icon-for-mode 'python-mode))))

(ert-deftest test-nerd-icons-icon-for-buffer ()
  (should
   (equal (cdr (assoc "elisp" nerd-icons-alist/fileicon))
          (with-tem-buffer
           (emacs-lisp-mode)
           (nerd-icons-icon-for-buffer)))))

(ert-deftest test-nerd-icons-icon-for-buffer ()
  (should
   (equal (cdr (assoc "elisp" nerd-icons-alist/fileicon))
          (with-temp-buffer
           (emacs-lisp-mode)
           (nerd-icons-icon-for-buffer)))))

(ert-deftest test-nerd-icons-icon-for-dir ()
  (should
   (equal
    (cdr (assoc "trash-o" nerd-icons-alist/faicon))
    (replace-regexp-in-string
     "\\`\t+\\([^\t]+\\)\t*\\'" "\\1"
     (substring-no-properties
      (nerd-icons-icon-for-dir "trash"))))))

(ert-deftest test-nerd-icons-icon-for-url ()
  (should
   (equal (cdr (assoc "google" nerd-icons-alist/faicon))
          (nerd-icons-icon-for-url "https://google.com"))))

(ert-deftest test-nerd-icons-icon-for-weather ()
  (should
   (equal (cdr (assoc "tornado" nerd-icons-alist/weather))
          (nerd-icons-icon-for-weather "tornado"))))

;;; test-nerd-icons.el ends here
