;;; shrface.el --- Apply Org Faces for shr -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/shrface
;; Keywords: faces
;; Created: 10 April 2020
;; Version: 2.1
;; Package-Requires: ((emacs "25.1") (org "9.0"))

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
  "Bullets for headings."
  :group 'shrface
  :type '(repeat (string :tag "Bullet character")))

(defcustom shrface-paragraph-indentation 0
  "Indentation for paragraph."
  :group 'shrface
  :type 'integer)

(defcustom  shrface-paragraph-fill-column 120
  "Fill columns for paragraph."
  :group 'shrface
  :type 'integer)

(defcustom shrface-item-bullet "➤"
  "Bullet used for unordered lists."
  :group 'shrface
  :type 'character)

(defcustom shrface-mode-hook nil
  "Hook run after enable shrface-mode buffers.
This hook is evaluated when enable `shrface-mode'."
  :group 'shrface
  :type 'hook)

(defcustom shrface-href-versatile nil
  "NON-nil to enable versatile href faces"
  :group 'shrface
  :type 'boolean)

(defvar shrface-href-face 'shrface-href-face
  "Face name to use for href if `shrface-href-versatile' is nil")

(defvar shrface-href-http-face 'shrface-href-http-face
  "Face name to use for href http://.")

(defvar shrface-href-https-face 'shrface-href-https-face
  "Face name to use for href https://.")

(defvar shrface-href-ftp-face 'shrface-href-ftp-face
  "Face name to use for href ftp://.")

(defvar shrface-href-file-face 'shrface-href-file-face
  "Face name to use for href file://.")

(defvar shrface-href-mailto-face 'shrface-href-mailto-face
  "Face name to use for href mailto://.")

(defvar shrface-href-other-face 'shrface-href-other-face
  "Face name to use for other href.")

(defvar shrface-level 'shrface-level
  "Compute the header's nesting level in an outline.")

(defvar shrface-supported-faces-alist
  '((em  . shrface-tag-em)
    (h1  . shrface-tag-h1)
    (h2  . shrface-tag-h2)
    (h3  . shrface-tag-h3)
    (h4  . shrface-tag-h4)
    (h5  . shrface-tag-h5)
    (h6  . shrface-tag-h6)
    (a   . shrface-tag-a)
    (p   . shrface-tag-p)
    (li   . shrface-tag-li)
    (dt   . shrface-tag-dt)
    ;; (code   . shrface-tag-code)
    )
  "Alist of shrface supported faces except experimental faces")

(defface shrface-href-face '((t :inherit org-link))
  "Default <href> face if `shrface-href-versatile' is nil"
  :group 'shrface-faces)

(defface shrface-href-other-face '((t :inherit org-link :foreground "#81A1C1"))
  "Face used for <href> other than http:// https:// ftp://
file:// mailto:// if `shrface-href-versatile' is NON-nil. For
example, it can be used for fontifying charter links with epub
files when using nov.el."
  :group 'shrface-faces)

(defface shrface-href-http-face '((t :inherit org-link :foreground "#39CCCC"))
  "Face used for <href>, http:// if `shrface-href-versatile' is
NON-nil"
  :group 'shrface-faces)

(defface shrface-href-https-face '((t :inherit org-link :foreground "#7FDBFF"))
  "Face used for <href>, https:// if `shrface-href-versatile' is
NON-nil"
  :group 'shrface-faces)

(defface shrface-href-ftp-face '((t :inherit org-link :foreground "#5E81AC"))
  "Face used for <href>, ftp:// if `shrface-href-versatile' is
NON-nil"
  :group 'shrface-faces)

(defface shrface-href-file-face '((t :inherit org-link :foreground "#87cefa"))
  "Face used for <href>, file:// if `shrface-href-versatile' is
NON-nil"
  :group 'shrface-faces)

(defface shrface-href-mailto-face '((t :inherit org-link :foreground "#8FBCBB"))
  "Face used for <href>, mailto:// if `shrface-href-versatile' is
NON-nil"
  :group 'shrface-faces)

(defface shrface-h1-face '((t :inherit org-level-1))
  "Face used for <h1> headlines."
  :group 'shrface-faces)

(defface shrface-h2-face '((t :inherit org-level-2))
  "Face used for <h2> headlines."
  :group 'shrface-faces)

(defface shrface-h3-face '((t :inherit org-level-3))
  "Face used for <h3> headlines."
  :group 'shrface-faces)

(defface shrface-h4-face  '((t :inherit org-level-4))
  "Face used for <h4> headlines."
  :group 'shrface-faces)

(defface shrface-h5-face  '((t :inherit org-level-5))
  "Face used for <h5> headlines."
  :group 'shrface-faces)

(defface shrface-h6-face '((t :inherit org-level-6))
  "Face used for <h6> headlines."
  :group 'shrface-faces)

(defface shrface-highlight '((t :inherit highlight))
  ";;TODO Face used for highlight."
  :group 'shrface-faces)

(defface shrface-verbatim '((t :inherit org-verbatim))
  "Face used for verbatim/emphasis - <em>."
  :group 'shrface-faces)

(defface shrface-code '((t :inherit org-code))
  "TODO Face used for inline <code>"
  :group 'shrface-faces)

(defface shrface-item-bullet-face '((t :inherit org-list-dt))
  "Face used for unordered list bullet"
  :group 'shrface-faces)

(defface shrface-item-number-face '((t :inherit org-list-dt))
  "Face used for ordered list numbers"
  :group 'shrface-faces)

(defface shrface-description-list-term-face '((t :inherit org-list-dt))
  "Face used for description list terms <dt>"
  :group 'shrface-faces)

;;; Utility

;;;###autoload
(defsubst shrface-shr-generic (dom)
  "TODO: Improved shr-generic: fontize the sub DOM."
  (dolist (sub (dom-children dom))
    (cond ((stringp sub) (shr-insert sub)) ; insert the string dom
          ((not (equal "" (dom-text (dom-by-tag sub 'code))))
           (shrface-shr-fontize-dom-child sub '(comment t face shrface-code))) ; insert the fontized <code> dom
          (t (shr-descend sub)))))      ;insert other sub dom

;;;###autoload
(defun shrface-shr-fontize-dom (dom &rest types)
  "Fontize the sub Optional argument TYPES  DOM."
  (let ((start (point))) ;; remember start of inserted region
    (shr-generic dom) ;; inserts the contents of the tag
    (dolist (type types)
      (shrface-shr-add-font start (point) type)) ;; puts text properties of TYPES on the inserted contents
    ))

;;;###autoload
(defun shrface-shr-fontize-dom-child (dom &rest types)
  "TODO: fontize the sub DOM.
Optional argument TYPES face attributes."
  (let ((start (point))) ;; remember start of inserted region
    (shr-descend dom) ;; inserts the contents of the tag
    (dolist (type types)
      (shrface-shr-add-font start (point) type)) ;; puts text properties of TYPES on the inserted contents
    ))

;;;###autoload
(defun shrface-shr-add-font (start end type)
  "Fontize the string.
Argument START start point.
Argument END end point.
Argument TYPE face attributes."
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
  "Fontize the URL.
Argument START start point.
Argument END the url."
  (shr-add-font start (point) 'shr-link)
  (if shrface-href-versatile
    (let* ((extract (let ((string url)
                          (regexp "\\(https\\)\\|\\(http\\)\\|\\(ftp\\)\\|\\(file\\)\\|\\(mailto\\):"))
                      (ignore-errors    ; in case of the url is not string
                        (when (string-match regexp string)
                          (match-string 0 string)))))
           (face (cond
                  ((equal extract "http")  shrface-href-http-face)
                  ((equal extract "https") shrface-href-https-face)
                  ((equal extract "ftp") shrface-href-ftp-face)
                  ((equal extract "file") shrface-href-file-face)
                  ((equal extract "mailto") shrface-href-mailto-face)
                  (t  shrface-href-other-face))))
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
             'face face
             'mouse-face 'highlight)))
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
           'mouse-face 'highlight)))
  (while (and start
              (< start (point)))
    (let ((next (next-single-property-change start 'keymap nil (point))))
      (if (get-text-property start 'keymap)
          (setq start next)
        (put-text-property start (or next (point)) 'keymap shr-map)))))

;;;###autoload
(defun shrface-bullets-level-string (level)
  "Return the bullets in cycle way.
Argument LEVEL the headline level."
  (nth (mod (1- level)
             (length shrface-bullets-bullet-list))
        shrface-bullets-bullet-list))

(defun shrface-outline-regexp ()
  "TODO: Regexp to match shrface headlines."
  (concat " ?+"
          (regexp-opt
           shrface-bullets-bullet-list
           t) " +"))

(defun shrface-outline-regexp-bol ()
  "TODO: Regexp to match shrface headlines.
This is similar to `shrface-outline-regexp' but additionally makes
sure that we are at the beginning of the line."
  (concat " ?+"
          (regexp-opt
           shrface-bullets-bullet-list
           t) "\\( +\\)"))

(defun shrface-imenu-regexp-bol ()
  "TODO: Regexp to match shrface headlines.
This is similar to `shrface-outline-regexp' but additionally makes
sure that we are at the beginning of the line."
  (concat "^\\(?: ?+\\)"
          (regexp-opt
           shrface-bullets-bullet-list
           t) "\\( .*\\)$"))

(defun shrface-tag-h1 (dom)
  "Fontize tag h1.
Argument DOM dom."
  (shrface-shr-h1 dom '(comment t face shrface-h1-face)))

(defun shrface-tag-h2 (dom)
  "Fontize tag h2.
Argument DOM dom."
  (shrface-shr-h2 dom '(comment t face shrface-h2-face)))

(defun shrface-tag-h3 (dom)
  "Fontize tag h3.
Argument DOM dom."
  (shrface-shr-h3 dom '(comment t face shrface-h3-face)))

(defun shrface-tag-h4 (dom)
  "Fontize tag h4.
Argument DOM dom."
  (shrface-shr-h4 dom '(comment t face shrface-h4-face)))

(defun shrface-tag-h5 (dom)
  "Fontize tag h5.
Argument DOM dom."
  (shrface-shr-h5 dom '(comment t face shrface-h5-face)))

(defun shrface-tag-h6 (dom)
  "Fontize tag h6.
Argument DOM dom."
  (shrface-shr-h6 dom '(comment t face shrface-h6-face)))

(defun shrface-shr-item-bullet ()
  "Build a `shr-bullet' based on `shrface-item-bullet'."
  (setq shr-bullet (concat shrface-item-bullet " ")))

(defun shrface-shr-h1 (dom &rest types)
  "Insert the Fontized tag h1.
Argument DOM dom.
Optional argument TYPES face attributes."
  (shr-ensure-paragraph)
  (insert (propertize (concat (shrface-bullets-level-string 1) " ") 'face 'shrface-h1-face))
  ;; (insert (propertize  "* " 'face 'shrface-h1-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h2 (dom &rest types)
  "Insert the Fontized tag h2.
Argument DOM dom.
Optional argument TYPES face attributes."
  (shr-ensure-paragraph)
  (insert (propertize (concat " " (shrface-bullets-level-string 2) " ") 'face 'shrface-h2-face))
  ;; (insert (propertize  "** " 'face 'shrface-h2-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h3 (dom &rest types)
  "Insert the Fontized tag h3.
Argument DOM dom.
Optional argument TYPES face attributes."
  (shr-ensure-paragraph)
  (insert (propertize (concat "  " (shrface-bullets-level-string 3) " ") 'face 'shrface-h3-face))
  ;; (insert (propertize  "*** " 'face 'shrface-h3-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h4 (dom &rest types)
  "Insert the Fontized tag h4.
Argument DOM dom.
Optional argument TYPES face attributes."
  (shr-ensure-paragraph)
  (insert (propertize (concat "   " (shrface-bullets-level-string 4) " ") 'face 'shrface-h4-face))
  ;; (insert (propertize  "**** " 'face 'shrface-h4-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h5 (dom &rest types)
  "Insert the Fontized tag h5.
Argument DOM dom.
Optional argument TYPES face attributes."
  (shr-ensure-paragraph)
  (insert (propertize (concat "    " (shrface-bullets-level-string 5) " ") 'face 'shrface-h5-face))
  ;; (insert (propertize  "***** " 'face 'shrface-h5-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-shr-h6 (dom &rest types)
  "Insert the Fontized tag h6.
Argument DOM .
Optional argument TYPES face attributes."
  (shr-ensure-paragraph)
  (insert (propertize (concat "     " (shrface-bullets-level-string 6) " ") 'face 'shrface-h6-face))
  ;; (insert (propertize  "****** " 'face 'shrface-h6-face))
  (apply #'shrface-shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shrface-tag-code (dom)
  "Fontize tag code.
Argument DOM dom."
  (shrface-shr-fontize-dom dom '(comment t face shrface-code)))

(defun shrface-tag-p (dom)
  "Fontize tag p.
Argument DOM dom."
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
  "Fontize tag em.
Argument DOM dom."
  (shrface-shr-fontize-dom dom '(comment t face shrface-verbatim)))

(defun shrface-tag-a (dom)
  "Fontize tag a.
Argument DOM dom."
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

(defun shrface-tag-li (dom)
  "Fontize tag li.
Argument DOM dom."
  (shr-ensure-newline)
  ;; (setq shr-indentation 40)
  (let ((start (point)))
    (let* ((bullet
            (if (numberp shr-list-mode)
                (prog1
                    (format "%d " shr-list-mode)
                  (setq shr-list-mode (1+ shr-list-mode)))
              (car shr-internal-bullet)))
           (width (if (numberp shr-list-mode)
                      (shr-string-pixel-width bullet)
                    (cdr shr-internal-bullet))))
      (ignore-errors
        (if (numberp shr-list-mode)
            (insert (propertize bullet 'face 'shrface-item-number-face))
          (insert (propertize bullet 'face 'shrface-item-bullet-face))))
      (shr-mark-fill start)
      (let ((shr-indentation (+ shr-indentation width)))
        (put-text-property start (1+ start)
                           'shr-continuation-indentation shr-indentation)
        (put-text-property start (1+ start) 'shr-prefix-length (length bullet))
        (shr-generic dom))))
  (unless (bolp)
    (insert "\n")))

(defun shrface-tag-dt (dom)
  "Fontize tag dt.
Argument DOM dom."
  (shr-ensure-newline)
  (shrface-shr-fontize-dom dom '(comment t face shrface-description-list-term-face))
  (shr-ensure-newline))

;;;###autoload
(defun shrface-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (org-with-wide-buffer
   (goto-char (point-max))
   (let* ((re (shrface-imenu-regexp-bol))
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
  "Set regexp for outline minior mode."
  (setq-local outline-regexp (shrface-outline-regexp))
  (setq-local org-outline-regexp-bol outline-regexp) ; for org-cycle, org-shifttab
  (setq-local org-outline-regexp outline-regexp) ; for org-cycle, org-shifttab
  (setq-local org-complex-heading-regexp outline-regexp) ; for org-cycle, org-shifttab
  (setq-local outline-level shrface-level))


;;;###autoload
(defun shrface-occur ()
  "Use `occur' to find all `shrface-tag-h1' to `shrface-tag-h6'."
  (interactive)
  (occur (shrface-outline-regexp)))

;;;###autoload
(define-minor-mode shrface-mode
  "Toggle shr minor mode.
1. imenu
2. outline-minor-mode
3. org-indent-mode
But the shrface-ioccur can still work"
  :group 'shrface
  (cond
   (shrface-mode
    ;; (shrface-basic)
    ;; (shrface-trial)
    (shrface-regexp)
    (setq imenu-create-index-function #'shrface-imenu-get-tree)
    (outline-minor-mode)
    (org-indent-mode)
    (run-hooks 'shrface-mode-hook))
   (t
    ;; (shrface-resume)
    ;; (setq shr-bullet "* ")
    (setq imenu-create-index-function nil)
    (outline-minor-mode -1)
    (org-indent-mode -1))))

(defun shrface-basic()
  "Enable the shrface faces. Need to be called once before
loading eww, nov.el, dash-docs, mu4e, after shr."
  (interactive)
  (shrface-shr-item-bullet)
  (dolist (sub shrface-supported-faces-alist)
    (unless (member sub shr-external-rendering-functions)
      (add-to-list 'shr-external-rendering-functions sub)))
  ;; this setting is still needed to be setup by the user
  ;; (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  ;; (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  )

(defun shrface-resume ()
  "Resume the original faces. You can use it to resume the
original faces. All shrface faces will be disabled. Can be called
at any time."
  (interactive)
  (setq shr-external-rendering-functions nil)
  ;; this setting is still needed to be setup by the user
  ;; (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq shr-bullet "* "))

(defun shrface-trial ()
  "Experimental features. Need to be called once before loading
eww, nov, dash-docs, mu4e, after shr. shrface-tag-code is
experimental, sometimes eww will hangup."
  (interactive)
  (unless (member '(code . shrface-tag-code) shr-external-rendering-functions)
    (add-to-list 'shr-external-rendering-functions '(code   . shrface-tag-code))))


(provide 'shrface)
;;; shrface.el ends here
