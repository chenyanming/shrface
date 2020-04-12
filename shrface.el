;;; shrface.el --- Org-like Faces for shr -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan
;; URL: https://github.com/chenyanming/shrface
;; Keywords: shr face
;; Version: 1.2

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

(require 'shr)
(require 'org-faces)
(require 'org-bullets)

;;; shrface

(defgroup shrface nil
  "Org-like faces setting for shr"
  :group 'shr)

(defgroup shrface-faces nil
  "Org-like faces for shr"
  :group 'shrface
  :group 'faces)

(defcustom shrface-bullets-bullet-list org-bullets-bullet-list
  "Bullets for headings"
  :group 'shrface
  :type '(repeat (string :tag "Bullet character")))

(defcustom shrface-paragraph-indentation 2
  "Indentation for paragraph"
  :group 'shrface
  :type 'integer)

(defcustom  shrface-paragraph-fill-column 120
  "Fill columns for paragraph"
  :group 'shrface
  :type 'integer)

(defvar shrface-href-face 'shrface-href-face
  "Face name to use for href.")

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
  "Face used for highlight."
  :group 'shrface-faces)

(defface shrface-verbatim '((t :inherit org-verbatim))
  "Face used for verbatim."
  :group 'shrface-faces)

;;; Utility

;;;###autoload
(defsubst shrface-shr-generic (dom)
  "Improved shr-generic: fontize the sub dom"
  (dolist (sub (dom-children dom))
    (cond ((stringp sub) (shr-insert sub)) ; insert the string dom
          ((not (equal "" (dom-text (dom-by-tag sub 'code))))
           (shrface-shr-fontize-dom-child sub '(comment t face shrface-verbatim))) ; insert the fontized <code> dom
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

(defun shrface-tag-h1 (dom)
  (shrface-shr-h1 dom '(comment t face shrface-h1-face)))

(defun shrface-tag-h2 (dom)
  (shrface-shr-h2 dom '(comment t face shrface-h2-face)))

(defun shrface-tag-h3 (dom)
  (shrface-shr-h3 dom '(comment t face shrface-h3-face)))

(defun shrface-tag-h4 (dom)
  (shrface-shr-h4 dom '(comment t face shrface-h4-face)))

(defun shrface-tag-h5 (dom)
  (shrface-shr-h5 dom '(comment t face shrface-h5-face)))

(defun shrface-tag-h6 (dom)
  (shrface-shr-h6 dom '(comment t face shrface-h6-face)))

(defun shrface-shr-h1 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat (nth 0 shrface-bullets-bullet-list) " ") 'face 'shrface-h1-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h2 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat (nth 1 shrface-bullets-bullet-list) " ") 'face 'shrface-h2-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h3 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat (nth 2 shrface-bullets-bullet-list) " ") 'face 'shrface-h3-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h4 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat (nth 3 shrface-bullets-bullet-list) " ") 'face 'shrface-h4-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h5 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat (nth 4 shrface-bullets-bullet-list) " ") 'face 'shrface-h5-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h6 (dom &rest types)
  (shr-ensure-paragraph)
  (insert (propertize (concat (nth 5 shrface-bullets-bullet-list) " ") 'face 'shrface-h6-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-tag-p (dom)
  (let* ((code (with-temp-buffer
                 (shr-ensure-paragraph)
                 (shrface-shr-generic dom)
                 (shr-ensure-paragraph)
                 ;; indent and fill text node
                 (if (eq "" (dom-text dom))
                     nil
                   (progn
                     (setq-local fill-column shrface-paragraph-fill-column)
                     (fill-region (point-min) (point-max) nil nil nil)
                     (indent-rigidly (point-min) (point-max) shrface-paragraph-indentation)))
                 ;; add verbatim face to inline codes in paragraph
                 (buffer-string))))
    (insert code)))

(defun shrface-tag-em (dom)
  (shrface-shr-fontize-dom dom '(comment t face shrface-highlight)))

(defun shrface-tag-a (dom)
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

(provide 'shrface)
