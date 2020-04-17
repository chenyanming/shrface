;;; shrface.el --- Apply Org Faces for shr -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/shrface
;; Keywords: shr face
;; Created: 10 April 2020
;; Version: 1.7
;; Package-Requires: ((org "9.0"))

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

;; It is a shr faces package that helps you apply your favourite org faces to
;; shr

;;; Code:

(require 'shr)
(require 'org-faces)
(require 'outline)
(require 'org-indent)

(ignore-errors
;; in case the users lazy load org-mode before require shrface
;; require org-superstar and org-bullets
  (require 'org-superstar)
  (require 'org-bullets))

;;; shrface

(defgroup shrface nil
  "Org-like faces setting for shr"
  :group 'shr)

(defgroup shrface-faces nil
  "Org-like faces for shr"
  :group 'shrface
  :group 'faces)

(defcustom shrface-bullets-bullet-list
  (or (bound-and-true-p org-bullets-bullet-list)
      (bound-and-true-p org-superstar-headline-bullets-list)
      '("◉"
        "○"
        "✸"
        "✿"))
  "Bullets for headings"
  :group 'shrface
  :type '(repeat (string :tag "Bullet character")))

(defcustom shrface-paragraph-indentation 0
  "Indentation for paragraph"
  :group 'shrface
  :type 'integer)

(defcustom  shrface-paragraph-fill-column 120
  "Fill columns for paragraph"
  :group 'shrface
  :type 'integer)

(defcustom shrface-item-bullet "➤"
  "Bullet used for unordered lists."
  :group 'shrface
  :type 'character)

(defvar shrface-href-face 'shrface-href-face
  "Face name to use for href.")

(defvar shrface-outline-regexp (eval-when-compile
                                 (concat " ?+"
                                         (regexp-opt
                                          shrface-bullets-bullet-list
                                          t) " +"))
  "TODO: Regexp to match shrface headlines.")

(defvar shrface-outline-regexp-bol (eval-when-compile
                                     (concat " ?+"
                                             (regexp-opt
                                              shrface-bullets-bullet-list
                                              t) "\\( +\\)"))
  "TODO: Regexp to match shrface headlines.
This is similar to `shrface-outline-regexp' but additionally makes
sure that we are at the beginning of the line.")

(defvar shrface-imenu-regexp-bol (eval-when-compile
                                   (concat "^\\(?: ?+\\)"
                                           (regexp-opt
                                            shrface-bullets-bullet-list
                                            t) "\\( .*\\)$"))
  "TODO: Regexp to match shrface headlines.
This is similar to `shrface-outline-regexp' but additionally makes
sure that we are at the beginning of the line.")

(defvar shrface-level 'shrface-level
  "Compute the header's nesting level in an outline.")

(defface shrface-href-face '((t :inherit org-link))
  "Face used for href"
  :group 'shrface-faces)

(defface shrface-h1-face '((t :inherit org-level-1))
  "Face used for h1 headlines."
  :group 'shrface-faces)

(defface shrface-h2-face '((t :inherit org-level-2))
  "Face used for h2 headlines."
  :group 'shrface-faces)

(defface shrface-h3-face '((t :inherit org-level-3))
  "Face used for h3 headlines."
  :group 'shrface-faces)

(defface shrface-h4-face  '((t :inherit org-level-4))
  "Face used for h4 headlines."
  :group 'shrface-faces)

(defface shrface-h5-face  '((t :inherit org-level-5))
  "Face used for h5 headlines."
  :group 'shrface-faces)

(defface shrface-h6-face '((t :inherit org-level-6))
  "Face used for h6 headlines."
  :group 'shrface-faces)

(defface shrface-highlight '((t :inherit highlight))
  ";;TODO Face used for highlight."
  :group 'shrface-faces)

(defface shrface-verbatim '((t :inherit org-verbatim))
  "Face used for verbatim/emphasis."
  :group 'shrface-faces)

(defface shrface-code '((t :inherit org-code))
  "TODO Face used for inline code"
  :group 'shrface-faces)

(defface shrface-item-bullet-face '((t :inherit org-list-dt))
  "Face used for unordered list bullet"
  :group 'shrface-faces)

;;; Utility

;;;###autoload
(defsubst shrface-shr-generic (dom)
  "TODO: Improved shr-generic: fontize the sub dom"
  (dolist (sub (dom-children dom))
    (cond ((stringp sub) (shr-insert sub)) ; insert the string dom
          ((not (equal "" (dom-text (dom-by-tag sub 'code))))
           (shrface-shr-fontize-dom-child sub '(comment t face shrface-code))) ; insert the fontized <code> dom
          (t (shr-descend sub)))))      ;insert other sub dom

;;;###autoload
(defun shrface-shr-fontize-dom (dom &rest types)
  (let ((start (point))) ;; remember start of inserted region
    (shr-generic dom) ;; inserts the contents of the tag
    (dolist (type types)
      (shrface-shr-add-font start (point) type)) ;; puts text properties of TYPES on the inserted contents
    ))

;;;###autoload
(defun shrface-shr-fontize-dom-child (dom &rest types)
  "TODO: fontize the sub dom"
  (let ((start (point))) ;; remember start of inserted region
    (shr-descend dom) ;; inserts the contents of the tag
    (dolist (type types)
      (shrface-shr-add-font start (point) type)) ;; puts text properties of TYPES on the inserted contents
    ))

;;;###autoload
(defun shrface-shr-add-font (start end type)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (bolp)
        (skip-chars-forward " "))
      (add-text-properties (point) (min (line-end-position) end) type)
      (if (< (line-end-position) end)
          (forward-line 1)
        (goto-char end)))))

;;;###autoload
(defun shrface-shr-urlify (start url &optional title)
  (shr-add-font start (point) 'shr-link)
  (add-text-properties
   start (point)
   (list 'shr-url url
         'help-echo (let ((iri (or (ignore-errors
                                     (decode-coding-string
                                      (url-unhex-string url)
                                      'utf-8 t))
                                   url)))
                      (if title (format "%s (%s)" iri title) iri))
         'follow-link t
         'face shrface-href-face
         'mouse-face 'highlight))
  (while (and start
              (< start (point)))
    (let ((next (next-single-property-change start 'keymap nil (point))))
      (if (get-text-property start 'keymap)
          (setq start next)
        (put-text-property start (or next (point)) 'keymap shr-map)))))


;;;###autoload
(defun shrface-bullets-level-string (level)
  (nth (mod (1- level)
             (length shrface-bullets-bullet-list))
        shrface-bullets-bullet-list))

(defun shrface-tag-h1 (dom)
  "Fontize tag h1"
  (shrface-shr-h1 dom '(comment t face shrface-h1-face)))

(defun shrface-tag-h2 (dom)
  "Fontize tag h2"
  (shrface-shr-h2 dom '(comment t face shrface-h2-face)))

(defun shrface-tag-h3 (dom)
  "Fontize tag h3"
  (shrface-shr-h3 dom '(comment t face shrface-h3-face)))

(defun shrface-tag-h4 (dom)
  "Fontize tag h4"
  (shrface-shr-h4 dom '(comment t face shrface-h4-face)))

(defun shrface-tag-h5 (dom)
  "Fontize tag h5"
  (shrface-shr-h5 dom '(comment t face shrface-h5-face)))

(defun shrface-tag-h6 (dom)
  "Fontize tag h6"
  (shrface-shr-h6 dom '(comment t face shrface-h6-face)))

(defun shrface-shr-item-bullet ()
  "Fontize shr-bullet"
  (setq shr-bullet (propertize (concat shrface-item-bullet " ") 'face 'shrface-item-bullet-face)))

(defun shrface-shr-h1 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat (shrface-bullets-level-string 1) " ") 'face 'shrface-h1-face))
  ;; (insert (propertize  "* " 'face 'shrface-h1-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h2 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat " " (shrface-bullets-level-string 2) " ") 'face 'shrface-h2-face))
  ;; (insert (propertize  "** " 'face 'shrface-h2-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h3 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat "  " (shrface-bullets-level-string 3) " ") 'face 'shrface-h3-face))
  ;; (insert (propertize  "*** " 'face 'shrface-h3-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h4 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat "   " (shrface-bullets-level-string 4) " ") 'face 'shrface-h4-face))
  ;; (insert (propertize  "**** " 'face 'shrface-h4-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h5 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat "    " (shrface-bullets-level-string 5) " ") 'face 'shrface-h5-face))
  ;; (insert (propertize  "***** " 'face 'shrface-h5-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h6 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat "     " (shrface-bullets-level-string 6) " ") 'face 'shrface-h6-face))
  ;; (insert (propertize  "****** " 'face 'shrface-h6-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-tag-code (dom)
  "Fontize tag code"
  (shrface-shr-fontize-dom dom '(comment t face shrface-code)))

(defun shrface-tag-p (dom)
  "Fontize tag p"
  (let* ((code (with-temp-buffer
                 (shr-ensure-paragraph)
                 (shr-generic dom)
                 (shr-ensure-paragraph)
                 ;; indent and fill text node
                 ;; Temporary solution, not the best
                 (when (not (equal "" (dom-text dom)) )
                   (setq-local fill-column shrface-paragraph-fill-column)
                   (fill-region (point-min) (point-max) nil nil nil)
                   (if (not (equal 0 shrface-paragraph-indentation))
                       (indent-rigidly (point-min) (point-max) shrface-paragraph-indentation)))
                 (buffer-string))))
    (insert code)))

(defun shrface-tag-em (dom)
  "Fontize tag em"
  (shrface-shr-fontize-dom dom '(comment t face shrface-verbatim)))

(defun shrface-tag-a (dom)
  "Fontize tag a"
  (let ((url (dom-attr dom 'href))
        (title (dom-attr dom 'title))
        (start (point))
        shr-start)
    (shr-generic dom)
    (when (and shr-target-id
               (equal (dom-attr dom 'name) shr-target-id))
      ;; We have a zero-length <a name="foo"> element, so just
      ;; insert...  something.
      (when (= start (point))
        (shr-ensure-newline)
        (insert " "))
      (put-text-property start (1+ start) 'shr-target-id shr-target-id))
    (when url
      (shrface-shr-urlify (or shr-start start) (shr-expand-url url) title))))

;;;###autoload
(defun shrface-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (org-with-wide-buffer
   (goto-char (point-max))
   (let* ((re shrface-imenu-regexp-bol)
          (subs (make-vector (1+ org-imenu-depth) nil))
          (last-level 0))
     (while (re-search-backward re nil t)
       ;; (message (int-to-string (shrface-level (match-string 1))))
       (let ((level (1- (funcall shrface-level)))
             (headline (match-string 2)))
         (message (int-to-string level ))
         (message headline)
         ;; (when  (<= level org-imenu-depth)
         (when (and (<= level org-imenu-depth) (org-string-nw-p headline))
           (let* ((m (point-marker))
                  (item (propertize headline 'org-imenu-marker m 'org-imenu t)))
             (message item)
             (push m org-imenu-markers)
             (if (>= level last-level)
                 (push (cons item m) (aref subs level))
               (push (cons item
                           (cl-mapcan #'identity (cl-subseq subs (1+ level))))
                     (aref subs level))
               (cl-loop for i from (1+ level) to org-imenu-depth
                        do (aset subs i nil)))
             (setq last-level level)))))
     (aref subs 0))))

(defun shrface-level ()
  "Function of no args to compute a header's nesting level in an outline."
  (1+ (cl-position (match-string 1) shrface-bullets-bullet-list :test 'equal)))

(defun shrface-regexp ()
  "set regexp for outline minior mode"
  (setq-local outline-regexp shrface-outline-regexp)
  (setq-local org-outline-regexp-bol outline-regexp) ; for org-cycle, org-shifttab
  (setq-local org-outline-regexp outline-regexp) ; for org-cycle, org-shifttab
  (setq-local org-complex-heading-regexp outline-regexp) ; for org-cycle, org-shifttab
  (setq-local outline-level shrface-level))

;;; load general settings for shr
(with-eval-after-load 'shr
  (shrface-shr-item-bullet))

;;; load the settings for nov
(with-eval-after-load 'nov
  (add-hook 'nov-mode-hook
            (lambda ()
              (setq imenu-create-index-function 'shrface-imenu-get-tree)
              (shrface-regexp)
              (outline-minor-mode)
              (org-indent-mode))))

;;; load the settings for eww
(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook
            (lambda ()
              (setq imenu-create-index-function 'shrface-imenu-get-tree)
              (shrface-regexp)))

  (add-hook 'eww-after-render-hook
            (lambda ()
              (outline-minor-mode)
              (org-indent-mode))))

;;; enable the faces

(add-to-list 'shr-external-rendering-functions '(em  . shrface-tag-em))
(add-to-list 'shr-external-rendering-functions '(h1  . shrface-tag-h1))
(add-to-list 'shr-external-rendering-functions '(h2  . shrface-tag-h2))
(add-to-list 'shr-external-rendering-functions '(h3  . shrface-tag-h3))
(add-to-list 'shr-external-rendering-functions '(h4  . shrface-tag-h4))
(add-to-list 'shr-external-rendering-functions '(h5  . shrface-tag-h5))
(add-to-list 'shr-external-rendering-functions '(h6  . shrface-tag-h6))
(add-to-list 'shr-external-rendering-functions '(a   . shrface-tag-a))
(add-to-list 'shr-external-rendering-functions '(p   . shrface-tag-p))

;;;
;;; experimental features
;;;
;;; shrface-tag-code is experimental, sometimes eww will hangup.
;; (add-to-list 'shr-external-rendering-functions '(code   . shrface-tag-code))

(provide 'shrface)
;;; shrface.el ends here
