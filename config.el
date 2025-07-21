;;; config.el --- shrface configs -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/shrface
;; Keywords: faces
;; Created: 10 April 2020
;; Version: 2.6.4
;; Package-Requires: ((emacs "29.1") (org "9.0") (language-detection "0.1.0"))

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

;;; Commentary:

;; This file is my personal configuration for shrface.
;; You can use it as a reference to configure your own shrface.

(use-package shrface
  :defer t
  :config

  (defvar shrface-general-rendering-functions
    (append '((title . eww-tag-title)
              (form . eww-tag-form)
              (input . eww-tag-input)
              (button . eww-form-submit)
              (textarea . eww-tag-textarea)
              (select . eww-tag-select)
              (link . eww-tag-link)
              (meta . eww-tag-meta)
              (code . shrface-tag-code)
              (pre . shrface-shr-tag-pre-highlight))
            shrface-supported-faces-alist))

  (defvar shrface-nov-rendering-functions
    (append '((img . nov-render-img)
              (svg . nov-render-svg)
              (title . nov-render-title)
              (pre . shrface-shr-tag-pre-highlight)
              (code . shrface-tag-code)
              (form . eww-tag-form)
              (input . eww-tag-input)
              (button . eww-form-submit)
              (textarea . eww-tag-textarea)
              (select . eww-tag-select)
              (link . eww-tag-link)
              (meta . eww-tag-meta))
            shrface-supported-faces-alist))

  (defvar shrface-anki-rendering-functions
    (append '((img . anki-render-img)
              (pre . shrface-shr-tag-pre-highlight)
              (code . shrface-tag-code)
              (form . eww-tag-form)
              (input . eww-tag-input)
              (button . eww-form-submit)
              (textarea . eww-tag-textarea)
              (select . eww-tag-select)
              (link . eww-tag-link)
              (meta . eww-tag-meta))
            shrface-supported-faces-alist))

  (setq shr-cookie-policy nil)
  (if (string-equal system-type "android")
      (setq shrface-bullets-bullet-list '("▼" "▽" "▿" "▾"))
    (setq shrface-bullets-bullet-list '("▼" "▽" "▿" "▾")))
  (add-hook 'outline-view-change-hook 'shrface-outline-visibility-changed)
  (require 'shr-tag-pre-highlight)
  (setq shr-tag-pre-highlight-lang-modes
        '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
          ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql)
          ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("C++" . c++)
          ("screen" . shell-script) ("shell" . sh) ("bash" . sh)
          ("rust" . rustic)
          ("rust" . rustic)
          ("awk" . bash)
          ("json" . "js")
          ;; Used by language-detection.el
          ("emacslisp" . emacs-lisp)
          ;; Used by Google Code Prettify
          ("el" . emacs-lisp))))

(use-package calibredb
  :defer t
  :config
  (advice-add 'calibredb-show-entry :around #'shrface-render-advice))

(use-package eww
  :defer t
  :config
  (require 'shrface)
  (advice-add 'eww-display-html :around #'shrface-render-advice)
  (add-hook 'eww-after-render-hook #'org-indent-mode)
  (add-hook 'eww-after-render-hook #'eldoc-mode)
  (add-hook 'eww-after-render-hook #'eldoc-box-hover-mode)
  (add-hook 'eww-after-render-hook #'shrface-eww-setup))

(use-package anki
  :defer t
  :config
  (add-hook 'anki-mode-hook #'org-indent-mode)
  (advice-add 'anki-render-html :around #'shrface-anki-advice)
  (advice-add 'anki-render-region :around #'shrface-anki-advice)
  (add-hook 'anki-mode-hook #'eldoc-mode)
  (add-hook 'anki-mode-hook #'eldoc-box-hover-mode)
  (add-hook 'anki-mode-hook #'shrface-anki-setup))

(use-package nov
  :defer t
  :config
  (add-hook 'nov-mode-hook #'eldoc-mode)
  (add-hook 'nov-mode-hook #'org-indent-mode)
  (add-hook 'nov-mode-hook #'eldoc-box-hover-mode)
  (add-hook 'nov-mode-hook #'shrface-nov-setup)
  (require 'shrface)
  (setq nov-render-html-function #'shrface-nov-render-html)
  (advice-add 'shr--remove-blank-lines-at-the-end :override #'shrface-remove-blank-lines-at-the-end))

(use-package wallabag
  :defer t
  :config
  (add-hook 'wallabag-entry-mode-hook #'org-indent-mode)
  (add-hook 'wallabag-entry-mode-hook #'eldoc-mode)
  (add-hook 'wallabag-entry-mode-hook #'eldoc-box-hover-mode)
  (add-hook 'wallabag-entry-mode-hook #'shrface-wallabag-setup)
  (advice-add 'wallabag-entry-quit :after #'(lambda (&rest args)
                                              (interactive)
                                              (if (get-buffer "*Ilist*")
                                                  (kill-buffer "*Ilist*"))))
  (require 'shrface)
  (setq wallabag-render-html-function #'shrface-wallabag-render-html))

(use-package mu4e
  :defer t
  :config
  (advice-add 'mu4e-shr2text :around #'shrface-render-advice))

(use-package elfeed
  :defer t
  :config
  (add-hook 'elfeed-show-mode-hook #'org-indent-mode)
  (add-hook 'elfeed-show-mode-hook #'eldoc-mode)
  (add-hook 'elfeed-show-mode-hook #'eldoc-box-hover-mode)
  (add-hook 'elfeed-show-mode-hook #'shrface-wallabag-setup)
  (advice-add 'elfeed-insert-html :around #'shrface-elfeed-advice)
  (require 'shrface))


(defun shrface-eww-setup ()
  (unless shrface-toggle-bullets
    (shrface-regexp)
    (setq-local imenu-create-index-function #'shrface-imenu-get-tree))
  ;; workaround to show annotations in eww
  (shrface-show-all-annotations))

(defun shrface-anki-setup ()
  (unless shrface-toggle-bullets
    (shrface-regexp)
    (setq-local imenu-create-index-function #'shrface-imenu-get-tree)))

(defun shrface-anki-advice (orig-fun &rest args)
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        (shr-table-vertical-line "")
        (shr-width 90)
        (shr-indentation 3)
        (anki-shr-rendering-functions shrface-anki-rendering-functions)
        (shrface-toggle-bullets nil)
        (shrface-href-versatile t))
    (apply orig-fun args)))

(defun shrface-shr-tag-pre-highlight (pre)
  "Highlighting code in PRE."
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (code (with-temp-buffer
                 (shr-generic pre)
                 ;; (indent-rigidly (point-min) (point-max) 2)
                 (buffer-string)))
         (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                   (let ((sym (language-detection-string code)))
                     (and sym (symbol-name sym)))))
         (mode (and lang
                    (shr-tag-pre-highlight--get-lang-mode lang))))
    (shr-ensure-newline)
    (shr-ensure-newline)
    (setq start (point))
    (insert
     ;; (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
     (or (and (fboundp mode)
              (with-demoted-errors "Error while fontifying: %S"
                (shr-tag-pre-highlight-fontify code mode)))
         code)
     ;; (propertize "#+END_SRC" 'face 'org-block-end-line )
     )
    (shr-ensure-newline)
    (setq end (point))
    (pcase (frame-parameter nil 'background-mode)
      ('light
       (add-face-text-property start end '(:background "#D8DEE9" :extend t)))
      ('dark
       (add-face-text-property start end '(:background "#292b2e" :extend t))))
    (shr-ensure-newline)
    (insert "\n")))

(defun shrface-nov-render-html ()
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        (shr-table-vertical-line "|")
        (shr-width 7000) ;; make it large enough, it would not fill the column (use visual-line-mode/writeroom-mode instead)
        (shr-indentation 0) ;; remove all unnecessary indentation
        (tab-width 8)
        (shr-external-rendering-functions shrface-nov-rendering-functions)
        (shrface-toggle-bullets nil)
        (shrface-href-versatile t)
        (shr-use-fonts nil)           ; nil to use default font
        (shr-map nov-mode-map))

    ;; HACK: `shr-external-rendering-functions' doesn't cover
    ;; every usage of `shr-tag-img'
    (cl-letf (((symbol-function 'shr-tag-img) 'nov-render-img))
      (shr-render-region (point-min) (point-max)))
    ;; workaround, need a delay to update the header line
    (run-with-timer 0.01 nil 'shrface-update-header-line)
    ;; workaround, show annotations when document updates
    (shrface-show-all-annotations)))

(defun shrface-remove-blank-lines-at-the-end (start end)
  "A fix for `shr--remove-blank-lines-at-the-end' which will remove image at the end of the document."
  (save-restriction
    (save-excursion
      (narrow-to-region start end)
      (goto-char end)
      (when (and (re-search-backward "[^ \n]" nil t)
                 (not (eobp)))
        (forward-line 1)
        (delete-region (point) (min (1+ (point)) (point-max)))))))

(defun shrface-nov-setup ()
  (unless shrface-toggle-bullets
    (shrface-regexp))
  (set-visited-file-name nil t)
  (setq tab-width 8)
  (if (string-equal system-type "android")
      (setq-local touch-screen-enable-hscroll nil)))

(defun shrface-wallabag-setup ()
  (unless shrface-toggle-bullets
    (shrface-regexp)
    (setq-local imenu-create-index-function #'shrface-imenu-get-tree))
  (if (string-equal system-type "android")
      (setq-local touch-screen-enable-hscroll nil)))

(defun shrface-wallabag-render-html (beg end)
  ;; workaround, show annotations when document updates
  (shrface-render-region beg end)
  (shrface-update-header-line)
  (paw-annotation-mode 1)
  (visual-line-mode 1))

(defun shrface-render-region (beg end)
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        ;; make it large enough, it would not fill the column
        ;; I uses visual-line-mode, writeroom-mode for improving the reading experience instead
        (shr-width 7000)
        (shr-indentation (if (string-equal system-type "android") 0 0))
        (shr-table-vertical-line "|")
        (shr-external-rendering-functions shrface-general-rendering-functions)
        (shrface-toggle-bullets nil)
        (shrface-href-versatile t)
        (shr-use-fonts nil))
    (shr-render-region beg end)))

(defun shrface-render-advice (orig-fun &rest args)
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        (shr-table-vertical-line "|")
        (shr-width 65)
        (shr-indentation 0)
        (shr-external-rendering-functions shrface-general-rendering-functions)
        (shrface-toggle-bullets nil)
        (shrface-href-versatile t)
        (shr-use-fonts nil))
    ;; workaround, need a delay to update the header line
    (run-with-timer 0.01 nil 'shrface-update-header-line)
    (apply orig-fun args)))

(defun shrface-elfeed-advice (orig-fun &rest args)
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        ;; make it large enough, it would not fill the column
        ;; I uses visual-line-mode, writeroom-mode for improving the reading experience instead
        (shr-width 7000)
        (shr-indentation 0)
        (shr-table-vertical-line "|")
        (shr-external-rendering-functions shrface-general-rendering-functions)
        (shrface-toggle-bullets nil)
        (shrface-href-versatile t)
        (shr-use-fonts nil))
    (apply orig-fun args)
    (with-current-buffer "*elfeed-entry*"
      (shrface-show-all-annotations))))

(defun shrface-show-all-annotations()
  (when (bound-and-true-p paw-annotation-mode)
    (paw-clear-annotation-overlay)
    (paw-show-all-annotations)
    (if paw-annotation-show-wordlists-words-p
        (paw-focus-find-words :wordlist t))
    (if paw-annotation-show-unknown-words-p
        (paw-focus-find-words))

    ;; enable kinlde-like word wise feature by default
    (setq-local paw-enable-inline-annotations-p nil)
    (paw-toggle-inline-annotations)))
