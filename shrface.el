;;; shrface.el --- Extend shr/eww with org features and analysis capability -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/shrface
;; Keywords: faces
;; Created: 10 April 2020
;; Version: 2.6.4
;; Package-Requires: ((emacs "25.1") (org "9.0") (language-detection "0.1.0"))

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

;; This package extends `shr' / `eww' with org features and analysis capability.
;; It can be used in `dash-docs', `eww', `nov.el', `mu4e', `anki.el', etc.
;; - Configurable org-like heading faces, headline bullets, item bullets, paragraph
;;   indentation, fill-column, item bullet, versatile hyper
;;   links(http/https/file/mailto/etc) face and so on.
;; - Browse the internet or local html file with `eww' just like org mode.
;; - Read dash docsets with `dash-docs' and the beauty of org faces.
;; - Read epub files with `nov.el' , just like org mode.
;; - Read html email with `mu4e' , the same reading experience just like org mode
;;   without formatting html to org file.
;; - Switch/jump the headlines just like org-mode in `eww' and `nov.el' with `imenu'
;; - Toggle/cycle the headlines just like org-mode in `eww' and `nov.el' with `outline-minor-mode'
;;   and `org-cycle'/`org-shifttab'
;; - Enable indentation just like org-mode in `eww' and `nov.el' with `org-indent-mode'
;; - Analysis capability:
;;   - Headline analysis: List all headlines with clickable texts.
;;   - URL analysis: List all classified URL with clickable texts.
;; - Export HTML buffer to an org file using shr engine (no Pandoc is needed).


;;; Code:

(require 'shr)
(require 'org)
(require 'org-table)
(require 'org-faces)
(require 'outline)
(require 'org-indent)
(require 'compile)
(require 'pcase)
(require 'cl-lib)
(require 'url)

(ignore-errors
  ;; in case the users lazy load org-mode before require shrface
  ;; require org-superstar and org-bullets
  (require 'org-superstar)
  (require 'org-bullets)
  (require 'all-the-icons)
  (require 'consult)
  (require 'ivy)
  (require 'helm)
  (require 'helm-utils))

;;; shrface

(defgroup shrface nil
  "Org-like faces setting for shr"
  :group 'shr)

(defgroup shrface-faces nil
  "Org-like faces for shr"
  :group 'shrface
  :group 'faces)

(defgroup shrface-analysis-faces nil
  "Faces for shrface analysis realted buffers"
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

(define-obsolete-variable-alias 'shrface-paragraph-indentation
  'shr-indentation "shrface 2.6.4"
  "Please use `shr-indentation' or `org-indent-mode' to control
  the indentation.")

(define-obsolete-variable-alias 'shrface-paragraph-fill-column
  'shr-width "shrface 2.6.4"
  "Please use `nov-text-width' for `nov-mode'. For other modes,
  please use `shr-width.'")

(defcustom shrface-item-bullet '?–'
  "Bullet used for unordered lists."
  :group 'shrface
  :type 'character)

(defcustom shrface-mode-hook nil
  "Hook run after enable variable `shrface-mode' buffers.
This hook is evaluated when enable variable `shrface-mode'."
  :group 'shrface
  :type 'hook)

(defcustom shrface-href-versatile nil
  "NON-nil to enable versatile href faces."
  :group 'shrface
  :type 'boolean)

(defcustom shrface-imenu-depth 5
  "The maximum level for Imenu access to shrface headlines."
  :group 'shrface
  :type 'integer)

(defcustom shrface-toggle-bullets nil
  "Non-nil to disable headline bullets globally.
The following features are also disabled:
1. function `shrface-occur'
2. variable `shrface-mode'"
  :group 'shrface
  :type 'boolean)

(defvar shrface-org nil
  "NON-nil to render with original org features.")

(defvar shrface-org-title nil
  "Title of exported org file, only used in overwriting `shrface-title'.")

(defvar shrface-title nil
  "Title of html.")

(defvar shrface-request-url nil
  "Request url of html, bound it before generating the org buffer/file.")

(defvar shrface-headline-property 'shrface-headline
  "Property name to use for href.")

(defvar shrface-headline-number-property 'shrface-number
  "Property name to use for headline number.")

(defvar shrface-headline-number 0
  "Counter to count The nth Headline in the buffer.")

(defvar shrface-href-property 'shr-url
  "Property name to use for href.")

(defvar shrface-eww-image-property 'image-url
  "Property name to use for internet image.")

(defvar shrface-href-follow-link-property 'follow-link
  "Property name to use for follow link.")

;; (defvar shrface-nov-image-original-property 'display
;;   "Property name to use for external image.")

(defvar shrface-href-face 'shrface-href-face
  "Face name to use for href if `shrface-href-versatile' is nil.")

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
    (u  . shrface-tag-u)
    (strong  . shrface-tag-strong)
    (svg . shrface-tag-svg)
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
    (figure . shrface-tag-figure)
    ;; (code   . shrface-tag-code)
    )
  "Alist of shrface supported faces except experimental faces.")

(defvar shrface-href-collected-list nil
  "Global list to save the collected href items.
Used when we run `shrface-links' or `shrface-links-counsel'.
Used for later analysis, sorting, exporting etc.")

(defvar shrface-headline-collected-list nil
  "Global list to save the collected href items.
Used when we run `shrface-headline-counsel'.
Used for later analysis, sorting, exporting etc.")

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

(defface shrface-highlight '((t :inherit mode-line-highlight))
  ";;Face used for highlight."
  :group 'shrface-faces)

(defface shrface-verbatim '((t :inherit org-verbatim))
  "Face used for verbatim/emphasis - <em>."
  :group 'shrface-faces)

(defface shrface-code '((t :inherit org-code))
  "TODO Face used for inline <code>"
  :group 'shrface-faces)

(defface shrface-figure '((t :inherit org-table))
  "Face used for figure <figure>, e.g. figure captions."
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


;;; Faces for shrface-analysis realted buffers

(defface shrface-links-title-face '((t :inherit default))
  "Face used for *shrface-links* title"
  :group 'shrface-analysis-faces)

(defface shrface-links-url-face '((t :inherit font-lock-comment-face))
  "Face used for *shrface-links* url"
  :group 'shrface-analysis-faces)

(defface shrface-links-mouse-face '((t :inherit mode-line-highlight))
  "Face used for *shrface-links* mouse face"
  :group 'shrface-analysis-faces)

(defvar-local shrface-outline--cycle-buffer-state 'show-all
  "Interval variable used for tracking buffer cycle state.")

;;; Keys
(defvar shrface-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for function `shrface-mode'.")

(defun shrface-default-keybindings ()
  "Setup default keybingings for variable `shrface-mode'."
  (interactive)
  "Sets up the default keybindings for `shrface-mode'."
  (define-key shrface-mode-map (kbd "TAB") 'shrface-outline-cycle)
  (define-key shrface-mode-map (kbd "<backtab>") 'shrface-outline-cycle-buffer)
  (define-key shrface-mode-map (kbd "C-t") 'shrface-toggle-bullets)
  (define-key shrface-mode-map (kbd "C-j") 'shrface-next-headline)
  (define-key shrface-mode-map (kbd "C-k") 'shrface-previous-headline)
  (define-key shrface-mode-map (kbd "M-l") 'shrface-links-counsel) ; or 'shrface-links-helm
  (define-key shrface-mode-map (kbd "M-h") 'shrface-headline-counsel))

;;; Utility

;;;###autoload
(defun shrface-shr-fontize-dom (dom props face)
  "Fontize the sub Optional argument DOM PROPS and FACE."
  (let (start end) ;; remember start of inserted region
    (setq start (point))
    (shr-generic dom) ;; inserts the contents of the tag
    (setq end (point))
    (shrface-shr-add-props start end props)
    (shrface-shr-add-face start end face))) ;; puts text properties of TYPES on the inserted contents

;;;###autoload
(defun shrface-shr-add-props (start end props)
  "Fontize the string.
Argument START start point.
Argument END end point.
Argument PROPS text properties."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (bolp)
        (skip-chars-forward " "))
      (add-text-properties (point) (min (line-end-position) end) props)
      (if (< (line-end-position) end)
          (forward-line 1)
        (goto-char end)))))

;;;###autoload
(defun shrface-shr-add-face (start end face)
  "Fontize the string.
Argument START start point.
Argument END end point.
Argument FACE face."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (bolp)
        (skip-chars-forward " "))
      (add-face-text-property (point) (min (line-end-position) end) face)
      (if (< (line-end-position) end)
          (forward-line 1)
        (goto-char end)))))

;;;###autoload
(defun shrface-shr-urlify (start url &optional title)
  "Fontize the URL.
Argument START start point.
Argument END the url."
  (shr-add-font start (point) 'shr-link)
  (and shrface-href-versatile (stringp url)
    (let* ((extract (let ((sub url)
                          (regexp "\\(https:\\)\\|\\(http:\\)\\|\\(ftp:\\)\\|\\(file:\\)\\|\\(mailto:\\)"))
                        (when (string-match regexp sub)
                          (match-string 0 sub))))
           (match (cond
                  ((equal extract "http:")  shrface-href-http-face)
                  ((equal extract "https:") shrface-href-https-face)
                  ((equal extract "ftp:") shrface-href-ftp-face)
                  ((equal extract "file:") shrface-href-file-face)
                  ((equal extract "mailto:") shrface-href-mailto-face)
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
             'face match
             `,match t
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
       (if (characterp (car shrface-bullets-bullet-list))
           (mapcar 'char-to-string shrface-bullets-bullet-list)
         shrface-bullets-bullet-list)))

(defun shrface-outline-regexp ()
  "TODO: Regexp to match shrface headlines."
  (concat " ?+"
          (regexp-opt
           (if (characterp (car shrface-bullets-bullet-list))
               (mapcar 'char-to-string shrface-bullets-bullet-list)
             shrface-bullets-bullet-list)
           t) " +"))

(defun shrface-outline-regexp-bol ()
  "TODO: Regexp to match shrface headlines.
This is similar to `shrface-outline-regexp' but additionally makes
sure that we are at the beginning of the line."
  (concat " ?+"
          (regexp-opt
           (if (characterp (car shrface-bullets-bullet-list))
               (mapcar 'char-to-string shrface-bullets-bullet-list)
             shrface-bullets-bullet-list)
           t) "\\( +\\)"))

(defun shrface-imenu-regexp-bol ()
  "TODO: Regexp to match shrface headlines.
This is similar to `shrface-outline-regexp' but additionally makes
sure that we are at the beginning of the line."
  (concat "^\\(?: ?+\\)"
          (regexp-opt
           (if (characterp (car shrface-bullets-bullet-list))
               (mapcar 'char-to-string shrface-bullets-bullet-list)
             shrface-bullets-bullet-list)
           t) "\\( .*\\)$"))

(defun shrface-clear (DOM)
  "Clear the `shrface-headline-number'.
Argument DOM The DOM."
  (listp DOM) ; just make the compile do not complain
  (setq shrface-headline-number 0))

(defun shrface-tag-h1 (dom)
  "Fontize tag h1.
Argument DOM dom."
  (setq-local shrface-headline-number (1+ shrface-headline-number))
  (shrface-shr-h1 dom `(shrface-headline "shrface-h1" ,shrface-headline-number-property ,shrface-headline-number) 'shrface-h1-face))

(defun shrface-tag-h2 (dom)
  "Fontize tag h2.
Argument DOM dom."
  (setq-local shrface-headline-number (1+ shrface-headline-number))
  (shrface-shr-h2 dom `(shrface-headline "shrface-h2" ,shrface-headline-number-property ,shrface-headline-number) 'shrface-h2-face))

(defun shrface-tag-h3 (dom)
  "Fontize tag h3.
Argument DOM dom."
  (setq-local shrface-headline-number (1+ shrface-headline-number))
  (shrface-shr-h3 dom `(shrface-headline "shrface-h3" ,shrface-headline-number-property ,shrface-headline-number) 'shrface-h3-face))

(defun shrface-tag-h4 (dom)
  "Fontize tag h4.
Argument DOM dom."
  (setq-local shrface-headline-number (1+ shrface-headline-number))
  (shrface-shr-h4 dom `(shrface-headline "shrface-h4" ,shrface-headline-number-property ,shrface-headline-number) 'shrface-h4-face))

(defun shrface-tag-h5 (dom)
  "Fontize tag h5.
Argument DOM dom."
  (setq-local shrface-headline-number (1+ shrface-headline-number))
  (shrface-shr-h5 dom `(shrface-headline "shrface-h5" ,shrface-headline-number-property ,shrface-headline-number) 'shrface-h5-face))

(defun shrface-tag-h6 (dom)
  "Fontize tag h6.
Argument DOM dom."
  (setq-local shrface-headline-number (1+ shrface-headline-number))
  (shrface-shr-h6 dom `(shrface-headline "shrface-h6" ,shrface-headline-number-property ,shrface-headline-number) 'shrface-h6-face))

(defun shrface-shr-item-bullet ()
  "Build a `shr-bullet' based on `shrface-item-bullet'."
  (setq shr-bullet (concat (char-to-string shrface-item-bullet) " ")))

(defun shrface-shr-h1 (dom props face)
  "Insert the Fontized tag h1.
Argument DOM dom.
Argument PROPS text properties.
Argument FACE face."
  (shr-ensure-paragraph)
  (if shrface-org
      (progn
        (insert "* ")
        (let ((url (dom-attr (dom-by-tag dom 'a) 'href)))
          (shrface-insert-org-link url dom)))
    (unless shrface-toggle-bullets
      (insert (propertize (concat (shrface-bullets-level-string 1) " ") 'face 'shrface-h1-face 'shrface-bullet "shrface-h1-bullet")))
    (shrface-shr-fontize-dom dom props face))
  (shr-ensure-paragraph))

(defun shrface-shr-h2 (dom props face)
  "Insert the Fontized tag h2.
Argument DOM dom.
Argument PROPS text properties.
Argument FACE face."
  (shr-ensure-paragraph)
  (if shrface-org
      (progn
        (insert "** ")
        (let ((url (dom-attr (dom-by-tag dom 'a) 'href)))
          (shrface-insert-org-link url dom)))
    (unless shrface-toggle-bullets
      (insert (propertize (concat " " (shrface-bullets-level-string 2) " ") 'face 'shrface-h2-face 'shrface-bullet "shrface-h2-bullet")))
    (shrface-shr-fontize-dom dom props face))
  (shr-ensure-paragraph))

(defun shrface-shr-h3 (dom props face)
  "Insert the Fontized tag h3.
Argument DOM dom.
Argument PROPS text properties.
Argument FACE face."
  (shr-ensure-paragraph)
  (if shrface-org
      (progn
        (insert "*** ")
        (let ((url (dom-attr (dom-by-tag dom 'a) 'href)))
          (shrface-insert-org-link url dom)))
    (unless shrface-toggle-bullets
      (insert (propertize (concat "  " (shrface-bullets-level-string 3) " ") 'face 'shrface-h3-face 'shrface-bullet "shrface-h3-bullet")))
    (shrface-shr-fontize-dom dom props face))
  (shr-ensure-paragraph))

(defun shrface-shr-h4 (dom props face)
  "Insert the Fontized tag h4.
Argument DOM dom.
Argument PROPS text properties.
Argument FACE face."
  (shr-ensure-paragraph)
  (if shrface-org
      (progn
        (insert "**** ")
        (let ((url (dom-attr (dom-by-tag dom 'a) 'href)))
          (shrface-insert-org-link url dom)))
    (unless shrface-toggle-bullets
      (insert (propertize (concat "   " (shrface-bullets-level-string 4) " ") 'face 'shrface-h4-face 'shrface-bullet "shrface-h4-bullet")))
    ;; (insert (propertize  "**** " 'face 'shrface-h4-face))
    (shrface-shr-fontize-dom dom props face))
  (shr-ensure-paragraph))

(defun shrface-shr-h5 (dom props face)
  "Insert the Fontized tag h5.
Argument DOM dom.
Argument PROPS text properties.
Argument FACE face."
  (shr-ensure-paragraph)
  (if shrface-org
      (progn
        (insert "***** ")
        (let ((url (dom-attr (dom-by-tag dom 'a) 'href)))
          (shrface-insert-org-link url dom)))
    (unless shrface-toggle-bullets
      (insert (propertize (concat "    " (shrface-bullets-level-string 5) " ") 'face 'shrface-h5-face 'shrface-bullet "shrface-h5-bullet")) )
    (shrface-shr-fontize-dom dom props face))
  (shr-ensure-paragraph))

(defun shrface-shr-h6 (dom props face)
  "Insert the Fontized tag h6.
Argument DOM dom.
Argument PROPS text properties.
Argument FACE face."
  (shr-ensure-paragraph)
  (if shrface-org
      (progn
        (insert "****** ")
        (let ((url (dom-attr (dom-by-tag dom 'a) 'href)))
          (shrface-insert-org-link url dom)))
    (unless shrface-toggle-bullets
      (insert (propertize (concat "     " (shrface-bullets-level-string 6) " ") 'face 'shrface-h6-face 'shrface-bullet "shrface-h6-bullet")))
    ;; (insert (propertize  "****** " 'face 'shrface-h6-face))
    (shrface-shr-fontize-dom dom props face))
  (shr-ensure-paragraph))

(defun shrface-tag-code (dom)
  "Fontize tag code.
Argument DOM dom."
  (if shrface-org
      ;; TODO not a good way to check if it is inline code
      (if (string-match-p "\n" (dom-text dom))
          (shr-generic dom)
        (insert "~")
        (shr-generic dom)
        (insert "~"))
    (shrface-shr-fontize-dom dom '(comment t) 'shrface-code)))

(defun shrface-tag-figure (dom)
  "Fontize tag figure.
Argument DOM dom."
  (shr-ensure-newline)
  (shrface-shr-fontize-dom dom '(comment t) 'shrface-figure)
  (shr-ensure-newline))

(defun shrface-tag-p (dom)
  "Fontize tag p.
Argument DOM dom."
  (if shrface-org
      (progn
        (shr-ensure-paragraph)
        (let ((url (dom-attr dom 'href)))
          (shrface-insert-org-link url dom))
        (shr-ensure-paragraph))
    (shr-ensure-paragraph)
    (shr-generic dom)
    (shr-ensure-paragraph)))

(defun shrface-tag-em (dom)
  "Fontize tag em.
Argument DOM dom."
  (if shrface-org
      (progn
        (insert "/")
        (shr-generic dom)
        (insert "/"))
    (shrface-shr-fontize-dom dom '(comment t) 'shrface-verbatim)))

(defun shrface-tag-strong (dom)
  "Fontize tag strong.
Argument DOM dom."
  (if shrface-org
      (progn
        (insert "*")
        (shr-generic dom)
        (insert "*"))
    (shr-fontize-dom dom 'bold)))

(defun shrface-tag-u (dom)
  "Fontize tag u.
Argument DOM dom."
  (if shrface-org
      (progn
        (insert "_")
        (shr-generic dom)
        (insert "_"))
    (shr-fontize-dom dom 'underline)))

(defun shrface-fix-url (url r-url)
  "Fix URL based on R-URL."
  (if r-url                             ; r-url is Non-Nil
      (progn
        (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" r-url) ; fix `r-url' type
          (setq r-url (concat "http://" r-url)))
        (if url                         ; url is Non-Nil
            (let* ((type (url-type (url-generic-parse-url r-url)))
                   (host (url-host (url-generic-parse-url r-url)))
                   (filename (url-filename (url-generic-parse-url r-url)))
                   (directory (or (file-name-directory filename) "/")))
              (if (string-match-p "^/.*" url) ; url start with /, absulute url
                  (setq url
                        (format "%s%s%s"
                                (concat type "://")
                                (if host
                                    host
                                  (if filename filename "")) ; no host, just use filename
                                url))
                (setq url
                      (format "%s%s%s"
                              (concat type "://")
                              (if host
                                  (concat host directory)
                                (if filename filename "")) ; no host, just use filename
                              url))))
          "") )
    url))

(defun shrface-scheme-p (url)
  "Check URL if has scheme or not."
  (if (not (url-type (url-generic-parse-url url)))
      t))

(defun shrface-tag-svg (dom)
  "Fontize tag svg.
Argument DOM dom."
  (if shrface-org
      nil
      (shr-tag-svg dom)))

(defun shrface-tag-img (dom)
  "Fontize tag svg.
Argument DOM dom."
  (if shrface-org
      (let ((url (dom-attr dom 'src)))
        (shr-ensure-newline)
        (shrface-insert-org-link url dom)
        (shr-ensure-newline))
    (shr-tag-img dom)))

(defun shrface-tag-pre (dom)
  "Fontize tag pre.
Argument DOM dom."
  (if shrface-org
      (progn
        (let ((shr-folding-mode 'none)
              (shr-current-font 'default))
          (require 'language-detection)
          (shr-ensure-newline)
          (insert (format "#+BEGIN_SRC %s" (symbol-name
                                            (with-temp-buffer
                                              (shr-generic dom)
                                              (language-detection-buffer)))))
          (shr-ensure-newline)
          (shr-generic dom)
          (shr-ensure-newline)
          (insert "#+END_SRC")
          (shr-ensure-newline)))
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (shr-generic dom)
      (shr-ensure-newline))))

(defun shrface-tag-title (dom)
  "Fontize tag title.
Argument DOM dom."
  (if shrface-org
        (setq shrface-title
              (decode-coding-string
               (dom-text dom)
               'utf-8 t))
    (shr-tag-title dom)))

(defun shrface-tag-span (dom)
  "Fontize tag span.
Argument DOM dom."
  (if shrface-org
      (let ((url (dom-attr dom 'href)))
        (shrface-insert-org-link url dom))
    (shr-tag-span dom)))

(defun shrface-insert-org-link (url dom)
  "TODO: Insert org link based on URL and DOM."
  (let* (
         (url (if (shrface-scheme-p url)
                  (shrface-fix-url url shrface-request-url)
                url))
         (img-dom (dom-by-tag dom 'img))
         (img-src (or (dom-attr img-dom 'data-src) ; some sites use data-src as real img data
                      (dom-attr img-dom 'src)
                      (let ((srcset (or (dom-attr img-dom 'srcset)
                                        (dom-attr img-dom 'data-srcset))))
                        (if (stringp srcset)
                            (car (split-string
                                  (car (split-string srcset ","))
                                  " " )))))) ; get car of src-set, split ',' then split ' '
         (img-alt (dom-attr img-dom 'alt))
         (img-title (dom-attr img-dom 'title))
         (img-width (dom-attr img-dom 'width))
         (img-height (dom-attr img-dom 'height)))
    (cond
     ((equal url "") (shr-generic dom))
     ((equal url nil) (shr-generic dom))
     (img-src
      (shr-ensure-newline)
      (if (not (equal (or img-alt img-title img-src) ""))
          (insert (format "#+CAPTION: %s" (or img-alt img-title img-src))))
      (shr-ensure-newline)
      (let ((width (if (stringp img-width) (string-to-number img-width) 0))
            (height (if (stringp img-height) (string-to-number img-height) 0))
            (fill-pixels (* (frame-char-width) fill-column)))
        (cond ((>= width fill-pixels)
               (insert (format "#+ATTR_ORG: :width %s" fill-pixels)))
              ((and (> width 0) (> height 0))
               (insert (format "#+ATTR_ORG: :width %s :height %s" img-width img-height)))
              ((> width 0)
               (insert (format "#+ATTR_ORG: :width %s" img-width)))
              ((> height 0)
               (insert (format "#+ATTR_ORG: :height %s" img-height)))))
      (shr-ensure-newline)
      (if (shrface-scheme-p img-src)
          (if (file-exists-p img-src)
              (insert (format "[[./%s]]" img-src)) ; if local images exists, add ./
            (insert (format "[[%s]]" (shrface-fix-url img-src shrface-request-url))))    ; if no local images, just fix img-src
        (insert (format "[[%s]]" img-src))) ; if not relative path, just orignial img-src
      (shr-ensure-newline))
     ((equal (dom-texts dom) "")
      (insert (format "[[%s]]" url)))
     ((= (length (replace-regexp-in-string "^\\s-*" "" (dom-texts dom))) 0) ;; delete heading whitespaces
      (insert (format "[[%s]]" url)))
     ((> (length (replace-regexp-in-string "^\\s-*" "" (dom-texts dom))) 0) ;; delete heading whitespaces
      (insert (format "[[%s][%s]]" url (replace-regexp-in-string "^\\s-*" "" (dom-texts dom)))))
     (t (shr-generic dom)))))

(defun shrface-tag-a (dom)
  "Fontize tag a.
Argument DOM dom."
  (let ((url (dom-attr dom 'href))
        (title (dom-attr dom 'title))
        (start (point))
        shr-start)
    (if shrface-org
        (shrface-insert-org-link url dom)
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
        (shrface-shr-urlify (or shr-start start) (shr-expand-url url) title)))))

(defun shrface-tag-li (dom)
  "Fontize tag li.
Argument DOM dom."
  (shr-ensure-newline)
  ;; (setq shr-indentation 40)
  (if shrface-org
      (progn
        (insert shr-bullet)
        (insert " ")
        (shr-generic dom))
    (let ((start (point)))
      (let* ((bullet
              (if (numberp shr-list-mode)
                  (prog1
                      (format "%d. " shr-list-mode)
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
          (shr-generic dom)))))
  (unless (bolp)
    (insert "\n")))

(defun shrface-tag-dt (dom)
  "Fontize tag dt.
Argument DOM dom."
  (shr-ensure-newline)
  (shrface-shr-fontize-dom dom '(comment t) 'shrface-description-list-term-face)
  (shr-ensure-newline))

;;;###autoload
(defun shrface-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (org-with-wide-buffer
   (goto-char (point-max))
   (let* ((re (shrface-imenu-regexp-bol))
          (subs (make-vector (1+ shrface-imenu-depth) nil))
          (last-level 0))
     (while (re-search-backward re nil t)
       ;; (message (int-to-string (shrface-level (match-string 1))))
       (let ((level (1- (funcall shrface-level)))
             (headline (match-string 2)))
         (message (int-to-string level ))
         (message headline)
         ;; (when  (<= level shrface-imenu-depth)
         (when (and (<= level shrface-imenu-depth) (org-string-nw-p headline))
           (let* ((m (point-marker))
                  (item (propertize headline 'org-imenu-marker m 'org-imenu t)))
             (message item)
             (push m org-imenu-markers)
             (if (>= level last-level)
                 (push (cons item m) (aref subs level))
               (push (cons item
                           (cl-mapcan #'identity (cl-subseq subs (1+ level))))
                     (aref subs level))
               (cl-loop for i from (1+ level) to shrface-imenu-depth
                        do (aset subs i nil)))
             (setq last-level level)))))
     (car (cl-remove nil (append subs nil) :test #'eq)))))

(defun shrface-level ()
  "Function of no args to compute a header's nesting level in an outline."
  (1+ (cl-position (match-string 1) (if (characterp (car shrface-bullets-bullet-list))
                                        (mapcar 'char-to-string shrface-bullets-bullet-list)
                                      shrface-bullets-bullet-list) :test 'equal)))

(defun shrface-regexp ()
  "Set regexp for outline minor mode."
  (setq-local outline-regexp (shrface-outline-regexp))
  (setq-local org-outline-regexp-bol outline-regexp) ; for org-cycle, org-shifttab
  (setq-local org-outline-regexp outline-regexp) ; for org-cycle, org-shifttab
  (setq-local org-complex-heading-regexp outline-regexp) ; for org-cycle, org-shifttab
  (setq-local outline-level shrface-level))


;;;###autoload
(defun shrface-occur ()
  "Use `occur' to find all `shrface-tag-h1' to `shrface-tag-h6'.
`shrface-occur' will disable if variable `shrface-toggle-bullets' is Non-nil."
  (interactive)
  (if (not shrface-toggle-bullets)
      (occur (shrface-outline-regexp)))
  (message "Please set `shrface-toggle-bullets' nil to use `shrface-occur'"))

(defun shrface-occur-flash ()
  "Flash the occurrence line."
  (shrface-flash-show (line-beginning-position) (line-end-position) 'shrface-highlight 0.5)
  (overlay-put compilation-highlight-overlay 'window (selected-window)))

;;;###autoload
(define-minor-mode shrface-mode
  "Toggle shr minor mode.
1. imenu
2. outline-minor-mode
3. org-indent-mode"
  :group 'shrface
  (cond
   (shrface-mode
    (shrface-basic)
    (shrface-trial)
    (unless shrface-toggle-bullets
      (shrface-regexp)
      (setq imenu-create-index-function #'shrface-imenu-get-tree)
      (outline-minor-mode)
      (org-indent-mode))
    (run-hooks 'shrface-mode-hook))
   (t
    (shrface-resume)
    (setq shr-bullet "* ")
    (setq imenu-create-index-function nil)
    (outline-minor-mode -1)
    (org-indent-mode -1))))

(defun shrface-basic()
  "Enable the shrface faces.
Need to be called once before loading eww, nov.el, dash-docs, mu4e, after shr."
  (interactive)
  (shrface-shr-item-bullet)
  (dolist (sub shrface-supported-faces-alist)
    (unless (member sub shr-external-rendering-functions)
      (add-to-list 'shr-external-rendering-functions sub)))
  (if (boundp 'nov-shr-rendering-functions)
      (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))))
  (if (boundp 'nov-shr-rendering-functions)
      (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))

  ;; setup occur flash
  (add-hook 'occur-mode-find-occurrence-hook #'shrface-occur-flash)

  ;; setup `shrface-links-counsel' ivy actions
  (shrface-links-counsel-set-actions)

  ;; add a simple advice to clear the counter every time reload the shr buffer.
  (advice-add 'shr-insert-document :after #'shrface-clear))

(defun shrface-resume ()
  "Resume the original faces.
You can use it to resume the original faces. All shrface
faces/features will be disabled. Can be called at any time."
  (interactive)
  (setq shr-external-rendering-functions nil)
  ;; this setting is still needed to be setup by the user
  ;; (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq shr-bullet "* ")
  ;; remove occur hook
  (remove-hook 'occur-mode-find-occurrence-hook #'shrface-occur-flash)
  ;; remove shr advice
  (advice-remove 'shr-insert-document #'shrface-clear))

(defun shrface-trial ()
  "Experimental features.
Need to be called once before loading
eww, nov, dash-docs, mu4e, after shr. `shrface-tag-code' is
experimental, sometimes eww will hangup."
  (interactive)
  (unless (member '(code . shrface-tag-code) shr-external-rendering-functions)
    (add-to-list 'shr-external-rendering-functions '(code   . shrface-tag-code))))

(defun shrface-toggle-bullets ()
  "Toggle shrface headline bullets locally and reload the current buffer.
Set Non-nil to disable headline bullets, besides, following
features are also disabled:
  1. function `shrface-occur'
  2. variable `shrface-mode'
FIXME: If variable `mu4e-view-mode' is t, bullets will disable/enable globally."
  (interactive)
  (cond ((equal major-mode 'eww-mode)
         (if (setq-local shrface-toggle-bullets (if (eq shrface-toggle-bullets nil) t nil))
             (message "shrface bullets disabled.")
           (message "shrface bullets enabled."))
         (when (fboundp 'eww-reload)
           (eww-reload)))
        ((equal major-mode 'nov-mode)
         (if (setq-local shrface-toggle-bullets (if (eq shrface-toggle-bullets nil) t nil))
             (message "shrface bullets disabled.")
           (message "shrface bullets enabled."))
         (when (fboundp 'nov-render-document)
           (nov-render-document)
           (when (boundp 'nov-mode-hook)
             (if (memq 'shrface-mode nov-mode-hook)
                 (shrface-mode)))))
        ((equal major-mode 'mu4e-view-mode)
         (if (setq shrface-toggle-bullets (if (eq shrface-toggle-bullets nil) t nil))
             (message "shrface bullets disabled globally.")
           (message "shrface bullets enabled globally."))
         (when (fboundp 'mu4e-view-refresh)
           (mu4e-view-refresh)))
        (t
         (if (setq shrface-toggle-bullets (if (eq shrface-toggle-bullets nil) t nil))
             (message "shrface bullets disabled globally.")
           (message "shrface bullets enabled globally.")))))

;;; shrface-analysis
;; `shrface-links'

(defun shrface-links()
  "`shrface-links' the links analysis feature of `shrface-analysis'.
Collect the positions of href links in the
current buffer and display the clickable result in
*shrface-links* buffer"
  (interactive)
  (shrface-href-collect-all)
  (let ((buf-name "*shrface-links*") occur-buf)
    (setq occur-buf (get-buffer-create buf-name))
    (when (buffer-live-p occur-buf)
      (switch-to-buffer-other-window occur-buf)
      (read-only-mode)
      (outline-minor-mode)
      (org-indent-mode)
      (goto-char (point-min)))))

(defun shrface-href-collect-all ()
  "OBSOLETE Collect all positions of URLs in the current buffer.
The value of the `shrface-href-collected-list' is returned."
  (setq shrface-href-collected-list nil) ; TODO use local list instead
  (let ((buf-name "*shrface-links*") occur-buf)
    (setq occur-buf (get-buffer-create buf-name))
    (with-current-buffer occur-buf
      (read-only-mode -1)
      (shrface-regexp)
      (erase-buffer))
    ;; (plist-get (cdr '(image :type imagemagick :file "/var/folders/st/mkq0gxld3rv39t6y6zv45j540000gn/T/nov-cKFscX.epub/EPUB/media/file6" :scale 1 :ascent 100 :max-width 1382 :max-height 820)) :file)
    ;; (shrface-href-collect shrface-nov-image-original-property shrface-nov-image-original-property "images" occur-buf) ; TODO: collect nov.el images links
    (shrface-collect shrface-eww-image-property shrface-eww-image-property "images" occur-buf shrface-href-collected-list) ; collect internet images links
    (shrface-collect shrface-href-property shrface-href-https-face "https" occur-buf shrface-href-collected-list) ; collect https links
    (shrface-collect shrface-href-property shrface-href-http-face "http" occur-buf shrface-href-collected-list) ; collect http links
    (shrface-collect shrface-href-property shrface-href-file-face "file" occur-buf shrface-href-collected-list) ; collect file links
    (shrface-collect shrface-href-property shrface-href-mailto-face "mailto" occur-buf shrface-href-collected-list) ; collect mailto links
    (shrface-collect shrface-href-property shrface-href-other-face "other" occur-buf shrface-href-collected-list))) ; collect other links

(defun shrface-href-collect-all-ordered ()
  "Collect all positions of URLs in the current buffer in order.
The value of the `shrface-href-collected-list' is returned."
  (setq shrface-href-collected-list nil) ; TODO use local list instead
  (let ((buf-name "*shrface-links*") occur-buf)
    (setq occur-buf (get-buffer-create buf-name))
    (with-current-buffer occur-buf
      (read-only-mode -1)
      (shrface-regexp)
      (erase-buffer))
    (shrface-collect shrface-href-property shrface-href-follow-link-property "All" occur-buf shrface-href-collected-list)))

(defun shrface-collect (property face title buf-name collected-list)
  "Collect the matched text elements in the current buffer.
Argument PROPERTY the property to be searched.
Argument FACE the property to be collected.
Argument TITLE the section title.
Argument BUF-NAME the buffer the results reside.
Argument COLLECTED-LIST the list to be returned of collected items ."

  ;; check whether `face' exist in the whole buffer or not
  (if (text-property-not-all (point-min) (point-max) `,face nil)
      (with-current-buffer buf-name
        (let (beg end)
          (setq beg (point))
          (insert (propertize (concat (shrface-bullets-level-string 1) " " title "\n") 'face 'shrface-h1-face))
          (setq end (point))
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<tab>") 'org-cycle)
            (define-key map (kbd "S-<tab>") 'org-shifttab)
            (put-text-property beg end 'keymap map)))))

  (save-excursion
    (save-restriction
      (narrow-to-region
       (point-min)
       (point-max))
      (goto-char (point-min))
      (let (beg end buf string url start final)
        (setq buf (current-buffer))
        (setq end
              (if (get-text-property (point) `,face)
                  (point)
                (text-property-any
                 (point) (point-max) `,face nil)))

        (while (setq beg (text-property-not-all
                          end (point-max) `,face nil))
          (goto-char beg)
          (setq url (get-text-property beg `,property))

          ;; TODO collect nov.el images links
          ;; (equal (car (get-text-property (point) 'display)) 'image)
          ;; (if (equal `,property 'display)
          ;;     (if (equal (car (get-text-property beg `,property)) 'image)
          ;;         (setq url (plist-get (cdr (get-text-property beg `,property)) :file))
          ;;       (setq face nil)    ; just set `face' nil to skip the following checking
          ;;       )
          ;;   (setq url (get-text-property beg `,property)))

          ;; TODO Disable, because it will make eww loop and hang
          ;; Skip leading newlines in the next link text.  They make things very
          ;; ugly when running `shrface-analysis' since the characters to jump to
          ;; each link will be displayed on the line before its visible text.
          ;; (skip-chars-forward "\n")
          ;; Handle the case where a link is all newlines by skipping them.

          ;; save the begining location to `beg'
          (setq beg (point))

          ;; Extract the current point text properties if it matched by giving
          ;; property `face', and insert it to `buf-name'
          (if (get-text-property (point) `,face)
              (progn
                (setq end (next-single-property-change (point) `,face nil (point-max)))
                ;; When link at the end of buffer, end will be set to nil.
                (if (not end)
                    (setq end (point-max)))

                (setq string (buffer-substring-no-properties beg end)) ; save the url title

                (with-current-buffer buf-name
                  (setq start (point)) ; save the start location before insertion
                  (if (stringp url)
                      (insert
                       (propertize
                        (if (fboundp 'all-the-icons-icon-for-url)
                            (all-the-icons-icon-for-url url :height 1.1)
                          "")
                        'mouse-face 'shrface-links-mouse-face
                        'help-echo "mouse-1: go to this occurrence; mouse-2: copy link; mouse-3: browse url")))
                  (insert
                   (propertize
                    (format " %s" string)
                    'face 'shrface-links-title-face
                    'mouse-face 'shrface-links-mouse-face
                    'help-echo "mouse-1: go to this occurrence; mouse-2: copy link; mouse-3: browse url") "\n  ")
                  ;; (insert (propertize "  " 'face 'shrface-h3-face))
                  (insert
                   (concat
                    (propertize
                     (format "%s" url)
                     'face 'shrface-links-url-face
                     'mouse-face 'shrface-links-mouse-face
                     'help-echo "mouse-1: go to this occurrence; mouse-2: copy link; mouse-3: browse url") "\n"))
                  ;; (insert "\n")
                  (setq final (point)) ; save the final location before insertion

                  ;; Put keymap and text properties to the texts between `start' to `final'
                  (let ((map (make-sparse-keymap)))
                    (define-key map [mouse-1] 'shrface-mouse-1)
                    (define-key map [mouse-2] 'shrface-mouse-2)
                    (define-key map [mouse-3] 'shrface-mouse-3)
                    (define-key map (kbd "<RET>") 'shrface-ret)
                    (put-text-property start final 'keymap map))
                  (put-text-property start final 'shrface-buffer buf)
                  (put-text-property start final 'shrface-url url)
                  (put-text-property start final 'shrface-beg beg)
                  (put-text-property start final 'shrface-end end))
                (push (list string url beg end) collected-list)))))))
  collected-list)

(defun shrface-mouse-1 (event)
  "Visit the location click on.
Argument EVENT mouse event."
  (interactive "e")
  ;; (message "click mouse-1")
  ;; (text-properties-at (point))
  ;; (message (get-text-property (point) 'shrface-url))
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No URL chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (let ((beg (get-text-property (point) 'shrface-beg))
            (end (get-text-property (point) 'shrface-end))
            (buffer (get-text-property (point) 'shrface-buffer)))
        (switch-to-buffer-other-window buffer)
        (remove-overlays)
        (goto-char beg)
        ;; (setq xx (make-overlay beg end))
        ;; (overlay-put xx 'face '(:background "gray" :foreground "black"))
        ;; (overlay-put xx 'face 'shrface-highlight)
        ;; (set-mark beg)
        ;; (goto-char end)
        (shrface-flash-show beg end 'shrface-highlight 0.5)
        (overlay-put compilation-highlight-overlay 'window (selected-window))))))

(defun shrface-mouse-2 (event)
  "Copy the url click on.
Argument EVENT mouse event."
  (interactive "e")
  ;; (message "click mouse-2")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No URL chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (let ((url (get-text-property (point) 'shrface-url)))
        (kill-new url)
        (message (concat "URL copied: " url))))))

(defun shrface-mouse-3 (event)
  "Browser the url click on.
Argument EVENT mouse event."
  (interactive "e")
  ;; (message "click mouse-3")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No URL chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (browse-url (get-text-property (point) 'shrface-url)))))

(defun shrface-ret ()
  "Goto url under point."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((beg (get-text-property (point) 'shrface-beg))
          (end (get-text-property (point) 'shrface-end))
          (buffer (get-text-property (point) 'shrface-buffer)))
      (switch-to-buffer-other-window buffer)
      (remove-overlays)
      (goto-char beg)
      (shrface-flash-show beg end 'shrface-highlight 0.5)
      (overlay-put compilation-highlight-overlay 'window (selected-window))
      ;; (setq xx (make-overlay beg end))
      ;; (overlay-put xx 'face '(:background "gray" :foreground "black"))
      ;; (overlay-put xx 'face 'shrface-highlight)
      ;; (set-mark beg)
      ;; (goto-char end)
       )))

(defun shrface-flash-show (pos end-pos face delay)
  "Flash a temporary highlight to help the user find something.
POS start position

END-POS end position, flash the characters between the two
points

FACE the flash face used

DELAY the flash delay"
  (when (and (numberp delay)
             (> delay 0))
    ;; else
    (when (timerp next-error-highlight-timer)
      (cancel-timer next-error-highlight-timer))
    (setq compilation-highlight-overlay (or compilation-highlight-overlay
                                            (make-overlay (point-min) (point-min))))
    (overlay-put compilation-highlight-overlay 'face face)
    (overlay-put compilation-highlight-overlay 'priority 10000)
    (move-overlay compilation-highlight-overlay pos end-pos)
    (add-hook 'pre-command-hook #'compilation-goto-locus-delete-o)
    (setq next-error-highlight-timer
          (run-at-time delay nil #'compilation-goto-locus-delete-o))))

(defun shrface-links-selectable-list ()
  "Return a fontified selecable url list in order.
It will be used in ivy counsel."
  (nreverse (mapcar #'(lambda (x)
                        (let* ((item x)
                               (title (car item))
                               (url (nth 1 item))
                               (beg (nth 2 item))
                               (end (nth 3 item)))
                          (list (format
                                 "%s\t%s"
                                 ;; insert icons will slow the list to be shown
                                 ;; (propertize
                                 ;;  (if (fboundp 'all-the-icons-icon-for-url)
                                 ;;      (all-the-icons-icon-for-url url :height 1.1)
                                 ;;    ""))
                                 (propertize (or title "") 'face 'shrface-links-title-face)
                                 (propertize (or url "") 'face 'shrface-links-url-face)) beg end)))
                    (shrface-href-collect-all-ordered))))

(defun shrface-links-counsel ()
  "Use counsel to present all urls in order founded in the buffer.
Next url will be the one of the candidates to initially select,
so that you would not lost if you call \\[ivy-call],
\\[ivy-next-line-and-call] or \\[ivy-previous-line-and-call] to
jump around the list."
  (interactive)
  (let ((start (point)) next url)
    ;; get the next nearest url
    (setq next (text-property-not-all
                (point) (point-max) shrface-href-follow-link-property nil))
    ;; only if the next url exists
    (if next
      (setq url (get-text-property next shrface-href-property)))
    (if (fboundp 'ivy-read)
        (ivy-read "shrface-links: " (shrface-links-selectable-list)
                  :action (lambda (x)
                            (remove-overlays)
                            (let ((beg (nth 1 x))
                                  (end (nth 2 x)) xx)
                              (goto-char beg)
                              (setq xx (make-overlay beg end))
                              (overlay-put xx 'face 'shrface-highlight)
                              (cond ((memq this-command '(ivy-done
                                                          ivy-alt-done
                                                          ivy-immediate-done))
                                     (remove-overlays)))))
                  :preselect url
                  :require-match t
                  :unwind (lambda ()
                            (remove-overlays)
                            (if (get-buffer "*shrface-links*")
                                (kill-buffer "*shrface-links*"))
                            (goto-char start))
                  :sort nil
                  :caller 'shrface-links-counsel)
      (message "Please install 'counsel' before using 'shrface-links-counsel'"))))

(defun shrface-links-helm ()
  "Use helm to present all urls in order founded in the buffer."
  (interactive)
  (let (result)
    (if (fboundp 'helm-comp-read)
        (progn
          (setq result (helm-comp-read
                      "shrface-links: " (shrface-links-selectable-list)
                      :persistent-action
                      (lambda (candidate)
                        (goto-char (nth 0 candidate))
                        (if (fboundp 'helm-highlight-current-line)
                            (helm-highlight-current-line)))))
          (goto-char (nth 0 result)))
      (message "Please enable 'helm-mode' before using 'shrface-headline-helm'"))))

(defun shrface-links-consult ()
  "Use consult to present all urls in order founded in the buffer."
  (interactive)
  (let ((start (point)) next url)
    ;; get the next nearest url
    (setq next (text-property-not-all
                (point) (point-max) shrface-href-follow-link-property nil))
    ;; only if the next url exists
    (if next
        (setq url (get-text-property next shrface-href-property)))
    (if (fboundp 'consult--read)
        (consult--read (shrface-links-selectable-list)
                       :prompt "shrface-links:"
                       :category 'shrface-links-consult
                       :sort nil
                       :lookup (lambda(cand candidates input-string _)
                                 (goto-char (nth 1 (assoc cand candidates)))
                                 (recenter nil)))
      (message "Please install 'consult' before using 'shrface-links-consult'"))))

(defun shrface-links-counsel-set-actions ()
  "Set actions for function `shrface-links-counsel' when call \\[ivy-occur]."
  (if (fboundp 'ivy-set-actions)
      (ivy-set-actions
       'shrface-links-counsel
       '(("v"
          (lambda (res)
            (eww-browse-url (nth 0 res)))  "eww browse url")))))

;;; shrface-analysis
;; `shrface-headline'

(defun shrface-headline-collect-all-ordered ()
  "Collect all positions of headlines in the current buffer in order.
The value of the `shrface-headline-collected-list' is returned."
  (setq shrface-headline-collected-list nil) ; TODO use local list instead
  (let ((buf-name "*shrface-headline*") occur-buf)
    (setq occur-buf (get-buffer-create buf-name))
    (with-current-buffer occur-buf
      (read-only-mode -1)
      (shrface-regexp)
      (erase-buffer))
    (shrface-collect shrface-headline-property shrface-headline-property "Headline" occur-buf shrface-headline-collected-list)))

(defun shrface-headline-selectable-list ()
  "Return a fontified selecable headline list in order.
It will be used in ivy counsel."
  (nreverse (mapcar #'(lambda (x)
                        (let* ((item x)
                               (title (car item))
                               (url (nth 1 item))
                               (beg (nth 2 item))
                               (end (nth 3 item)))
                          (list (format
                                 "%s"
                                 ;; insert icons will slow the list to be shown
                                 ;; (propertize
                                 ;;  (if (fboundp 'all-the-icons-icon-for-url)
                                 ;;      (all-the-icons-icon-for-url url :height 1.1)
                                 ;;    ""))
                                 ;; (propertize title 'face 'shrface-links-title-face)
                                 (cond
                                  ((equal "shrface-h1" url) (propertize title 'face 'shrface-h1-face))
                                  ((equal "shrface-h2" url) (propertize (concat " " title) 'face 'shrface-h2-face))
                                  ((equal "shrface-h3" url) (propertize (concat "  " title) 'face 'shrface-h3-face))
                                  ((equal "shrface-h4" url) (propertize (concat "   " title) 'face 'shrface-h4-face))
                                  ((equal "shrface-h5" url) (propertize (concat "     " title) 'face 'shrface-h5-face))
                                  ((equal "shrface-h6" url) (propertize (concat "      " title) 'face 'shrface-h6-face)))) beg end)))
                    (shrface-headline-collect-all-ordered))))

(defun shrface-headline-counsel ()
  "Use counsel to show all headlines in order founded in the buffer.
Current headline will be the one of the candidates to initially select,
so that you would not lost if you call \\[ivy-call],
\\[ivy-next-line-and-call] or \\[ivy-previous-line-and-call] to
jump around the list."
  (interactive)
  (let ((current (point-min)) (start (1+ (point))) point number)
    ;; Scan from point-min to (1+ (point)) to find the current headline.
    ;; (1+ (point)) to include under current point headline into the scan range.
    (unless (> start (point-max))
        (while (setq point (text-property-not-all
                            current start shrface-headline-number-property nil))
          (setq current (1+ point))))

    (cond ((equal (point) 1) (setq number 0))
          ((equal (point) 2) (setq number 0))
          ((equal (point) (point-max)) (setq number 0))
          (t
           (ignore-errors (setq number (1- (get-text-property (1- current) shrface-headline-number-property))))))

    ;; Start the ivy-read
    (setq start (point)) ; save the starting point
    (if (fboundp 'ivy-read)
        (ivy-read "shrface-headline: " (shrface-headline-selectable-list)
                  :action (lambda (x)
                            (remove-overlays)
                            (let ((beg (nth 1 x))
                                  (end (nth 2 x)) xx)
                              (goto-char beg)
                              (recenter nil)
                              (setq xx (make-overlay beg end))
                              (overlay-put xx 'face 'shrface-highlight)
                              (cond ((memq this-command '(ivy-done
                                                          ivy-alt-done
                                                          ivy-immediate-done))
                                     (remove-overlays)))))
                  :preselect number
                  :require-match t
                  :unwind (lambda ()
                            (remove-overlays)
                            (if (get-buffer "*shrface-headline*")
                                (kill-buffer "*shrface-headline*"))
                            (goto-char start))
                  :sort nil
                  :caller 'shrface-headline-counsel)
      (message "Please install 'counsel' before using 'shrface-headline-counsel'"))))

(defun shrface-headline-helm ()
  "Use helm to show all headlines in order founded in the buffer."
  (interactive)
  (let (result)
    (if (fboundp 'helm-comp-read)
        (progn
          (setq result (helm-comp-read
                      "shrface-headline: " (shrface-headline-selectable-list)
                      :persistent-action
                      (lambda (candidate)
                        (goto-char (car candidate))
                        (if (fboundp 'helm-highlight-current-line)
                            (helm-highlight-current-line)))))
          (goto-char (car result))
          (recenter nil))
      (message "Please enable 'helm-mode' before using 'shrface-headline-helm'"))))

(defun shrface-headline-consult ()
    "Use consult to show all headlines in order founded in the buffer.
Current headline will be the one of the candidates to initially select."
    (interactive)
    (let ((current (point-min)) (start (1+ (point))) point number)
      ;; Scan from point-min to (1+ (point)) to find the current headline.
      ;; (1+ (point)) to include under current point headline into the scan range.
      (unless (> start (point-max))
        (while (setq point (text-property-not-all
                            current start shrface-headline-number-property nil))
          (setq current (1+ point))))

      (cond ((equal (point) 1) (setq number 0))
            ((equal (point) 2) (setq number 0))
            ((equal (point) (point-max)) (setq number 0))
            (t
             (ignore-errors (setq number (1- (get-text-property (1- current) shrface-headline-number-property))))))

      ;; Start the consult--read
      (setq start (point)) ; save the starting point
      (if (fboundp 'consult--read)
          (consult--read (shrface-headline-selectable-list)
                         :prompt "shrface-headline:"
                         :category 'shrface-headlines-consult
                         :sort nil
                         :lookup (lambda (cand candidates input-string _)
                                   (goto-char (nth 1 (assoc cand candidates)))
                                   (recenter nil)))
        (message "Please install 'consult' before using 'shrface-headlines-consult'"))))

(defun shrface-previous-headline ()
  "Jump to previous headline."
  (interactive)
  (let ((location (get-text-property (point) shrface-headline-number-property)) previous)
    (cond
     ;; check the current point headline number first
     ((numberp location)
      (setq previous (text-property-any (point-min) (point-max) shrface-headline-number-property (1- location)))
      (if (numberp previous)
          (goto-char previous)
        (goto-char (point-min))))
     ;; check the current point if >= the first header (no matter level), keep (point) if no headlines
     ((>= (or (text-property-not-all (point-min) (point-max) shrface-headline-number-property nil) (point)) (point))
      (message "Beginning of buffer")
      (goto-char (point-min)))
     (t
      (let ((current (point-min)) (start (1+ (point))) point number)
        ;; Scan from point-min to (1+ (point)) to find the current headline.
        ;; (1+ (point)) to include under current point headline into the scan range.
        (if (<= start (point-max))
            (while (setq point (text-property-not-all
                                current start shrface-headline-number-property nil))
              (setq current (1+ point))) ; not at (point-max)
          (while (setq point (text-property-not-all
                              current (point-max) shrface-headline-number-property nil))
            (setq current (1+ point)))) ; at the (point-max)
        (setq number (1- (get-text-property (1- current) shrface-headline-number-property)))
        (goto-char (text-property-any (point-min) (point-max) shrface-headline-number-property (1+ number))))))))

(defun shrface-next-headline ()
  "Jump to next headline."
  (interactive)
  (let* ((header-in-line (text-property-not-all (line-beginning-position) (line-end-position) shrface-headline-number-property nil))
         (location (get-text-property (or header-in-line (point)) shrface-headline-number-property))
         next)
    (cond
     ;; check the current line headline number first, since if use org-cycle, cursor will go to the begining of line
     ((numberp location)
      (setq next (text-property-any (point-min) (point-max) shrface-headline-number-property (1+ location)))
      (if (numberp next)
          (goto-char next)
        (goto-char (point-max))))
     ;; check the current point if >= the first header (no matter level), keep (point) if no headlines
     ((>= (setq next (or (text-property-not-all (point-min) (point-max) shrface-headline-number-property nil) (point))) (point))
      (if (equal next (point))
          (progn
            (message "End of buffer")
            (goto-char (point-max)) )
        (goto-char next)))
     (t
      (let ((current (point-min)) (start (1+ (point))) point number)
        ;; Scan from point-min to (1+ (point)) to find the current headline.
        ;; (1+ (point)) to include under current point headline into the scan range.
        (unless (> start (point-max))
          (while (setq point (text-property-not-all
                              current start shrface-headline-number-property nil))
            (setq current (1+ point))))
        (cond ((equal (point) 1) (setq number 0))
              ((equal (point) 2) (setq number 0))
              ((equal (point) (point-max)) (setq number (point-max)) (message "End of buffer"))
              (t
               (setq number (1- (get-text-property (1- current) shrface-headline-number-property)))))
        (goto-char (or (text-property-any (point-min) (point-max) shrface-headline-number-property (+ 2 number)) (point-max))))))))

;;; outline cycle
;; Reference from https://github.com/casouri/lunarymacs/blob/master/site-lisp/outline+.el
(defun shrface-outline--cycle-state ()
  "Return the cycle state of current heading.
Return either 'hide-all, 'headings-only, or 'show-all."
  (save-excursion
    (let (start end ov-list heading-end)
      (outline-back-to-heading)
      (setq start (point))
      (outline-end-of-heading)
      (setq heading-end (point))
      (outline-end-of-subtree)
      (setq end (point))
      (setq ov-list (cl-remove-if-not
                     (lambda (o) (eq (overlay-get o 'invisible) 'outline))
                     (overlays-in start end)))
      (cond ((eq ov-list nil) 'show-all)
            ;; (eq (length ov-list) 1) wouldn’t work: what if there is
            ;; one folded subheading?
            ((and (eq (overlay-end (car ov-list)) end)
                  (eq (overlay-start (car ov-list)) heading-end))
             'hide-all)
            (t 'headings-only)))))

(defun shrface-outline-has-subheading-p ()
  "Return t if this heading has subheadings, nil otherwise."
  (save-excursion
    (outline-back-to-heading)
    (< (save-excursion (outline-next-heading) (point))
       (save-excursion (outline-end-of-subtree) (point)))))

(defun shrface-outline-cycle ()
  "Cycle between “hide all”, “headings only” and “show all”.
“Hide all” means hide all subheadings and their bodies.
“Headings only” means show sub headings but not their bodies.
“Show all” means show all subheadings and their bodies."
  (interactive)
  (pcase (shrface-outline--cycle-state)
    ('hide-all (if (shrface-outline-has-subheading-p)
                   (progn (outline-show-children)
                          (message "Only headings"))
                 (outline-show-subtree)
                 (message "Show all")))
    ('headings-only (outline-show-subtree)
                    (message "Show all"))
    ('show-all (outline-hide-subtree)
               (message "Hide all"))))

(defun shrface-outline-cycle-buffer ()
  "Cycle the whole buffer like in ‘shrface-outline-cycle’."
  (interactive)
  (pcase shrface-outline--cycle-buffer-state
    ('show-all (save-excursion
                 (let ((start-point (point)))
                   (while (not (eq (point) start-point))
                     (outline-up-heading 1))
                   (outline-hide-sublevels
                    (progn (outline-back-to-heading)
                           (funcall 'outline-level)))))
               (setq shrface-outline--cycle-buffer-state 'top-level)
               (message "Top level headings"))
    ('top-level (outline-show-all)
                (outline-hide-region-body (point-min) (point-max))
                (setq shrface-outline--cycle-buffer-state 'all-heading)
                (message "All headings"))
    ('all-heading (outline-show-all)
                  (setq shrface-outline--cycle-buffer-state 'show-all)
                  (message "Show all"))))

(defun shrface-parse-html ()
  "Parse html to dom. Refer to `eww-display-html'."
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (goto-char (point-min))
  (condition-case nil
      (decode-coding-region (point-min) (point-max) 'utf-8)
    (coding-system-error nil))
  (save-excursion
    ;; Remove CRLF and replace NUL with &#0; before parsing.
    (while (re-search-forward "\\(\r$\\)\\|\0" nil t)
      (replace-match (if (match-beginning 1) "" "&#0;") t t)))
  ;; eww--preprocess-html
  ;; Translate all < characters that do not look like start of tags into &lt;.
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point-max))
      (goto-char (point))
      (let ((case-fold-search t))
        (while (re-search-forward "<[^0-9a-z!/]" nil t)
          (goto-char (match-beginning 0))
          (delete-region (point) (1+ (point)))
          (insert "&lt;")))))
  (libxml-parse-html-region (point) (point-max)))

(defun shrface-html-convert-as-org-string (&optional html)
  "Convert HTML buffer/string/file and return as org string.
Optional argument HTML:
1. If HTML is a valid file, will convert the HTML file to org string.
2. If HTML is a string, will convert the HTML string to org string.
Detail uses cases can be found at test.el."
  (let* ((current-directory default-directory)
         (buf (current-buffer))
         (html (or html (if (equal (file-name-extension (or buffer-file-name "")) "html")
                            buffer-file-name
                          (buffer-string))))
         (default-directory (if (file-exists-p html)
                                (if (file-name-directory html)
                                    (file-name-directory html)
                                  current-directory)
                              current-directory)))
    (with-temp-buffer
      (let ((shrface-org t)
            (shr-bullet "- ")
            (shr-table-vertical-line "|")
            (shr-external-rendering-functions
             '((figure . shrface-tag-figure)
               (dt . shrface-tag-dt)
               (li . shrface-tag-li)
               (p . shrface-tag-p)
               (a . shrface-tag-a)
               (h6 . shrface-tag-h6)
               (h5 . shrface-tag-h5)
               (h4 . shrface-tag-h4)
               (h3 . shrface-tag-h3)
               (h2 . shrface-tag-h2)
               (h1 . shrface-tag-h1)
               (svg . shrface-tag-svg)
               (strong . shrface-tag-strong)
               (u . shrface-tag-u)
               (em . shrface-tag-em)
               (pre . shrface-tag-pre)
               (title . shrface-tag-title)
               (span . shrface-tag-span)
               (img . shrface-tag-img))))
        (cond
         ((not html)
          (shr-insert-document
           (with-current-buffer buf
             (shrface-parse-html))))
         ((file-exists-p html)
          (shr-insert-document
           (with-temp-buffer
             (insert-file-contents html)
             (shrface-parse-html))))
         ((stringp html)
          (shr-insert-document
           (with-temp-buffer
             (insert html)
             (shrface-parse-html))))
         (t (shr-insert-document
             (with-current-buffer buf
               (shrface-parse-html)))))
        (goto-char (point-min))
        (insert (format "#+TITLE: %s\n" (or shrface-org-title shrface-title)))
        (if shrface-request-url
            (insert (format "#+LINK_HOME: %s\n" shrface-request-url))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defmacro shrface--with-shrface-org-buffer (name &rest body)
  "Basic setup for a `shrface-org-mode' buffer.
Create a new buffer with name NAME if it does not exist.  Turn
on `shrface-org-mode' for that buffer, eval BODY and then switch to it."
  (declare (indent 0) (debug t))
  (let ((buffer (make-symbol "shrfacce-org-buffer")))
    `(let ((,buffer (generate-new-buffer ,name)))
       ;; `kill-buffer' can change current-buffer in some odd cases.
       (with-current-buffer ,buffer
         (unwind-protect
             (progn
               ,@body
               (org-mode))
           (and (buffer-name ,buffer)
                (switch-to-buffer ,buffer)
                (org-table-map-tables 'org-table-align)
                (goto-char (point-min))))))))

(defun shrface-html-export-as-org (&optional html)
  "Export HTML to an org buffer.
Optional argument HTML:
1. If HTML is a valid file, will convert the HTML file to buffer
*Shrface Org Export*.
2. If HTML is a string, will convert the HTML string to buffer
*Shrface Org Export*.
Detail uses cases can be found at test.el."
  (interactive)
  ;; (image-file-name-regexps "\\(.*svg.*\\)\\|\\(.*jpg.*\\)\\|\\(.*png.*\\)")
  (let* ((image-file-name-regexps ".*") ; Any files can be treated as images, since internet images may have no extenstion
         (str (shrface-html-convert-as-org-string html)))
    (shrface--with-shrface-org-buffer shrface-title
                                      (insert str))))

(defun shrface-html-export-to-org (&optional html filename slient)
  "Export HTML to an org file as FILENAME.
Optional argument HTML The html file name/string

1. If HTML is a valid file, will convert the HTML file to file
specified by FILENAME.

2. If HTML is a string, will convert the HTML string to file
specified by FILENAME.

3. If HTML is Nil, will try to select the current html buffer as
   input. if current buffer is not html, simply select the
   current `buffer-string'.

Optional argument FILENAME The org file name.
1. If FILENAME is provided, save as FILENAME.

2. If FILENAME is nil, save as a org file with same file name
base as HTML, under same directory as HTML.

Optional argument SLIENT Non-Nil to export sliently. Set Nil will
open the exported org file after exporting. Detail uses cases can
be found at test.el."
  (interactive)
  ;; (image-file-name-regexps "\\(.*svg.*\\)\\|\\(.*jpg.*\\)\\|\\(.*png.*\\)")
  (let* ((image-file-name-regexps ".*") ; Any files can be treated as images, since internet images may have no extenstion
         (str (shrface-html-convert-as-org-string html))
         (html (or html (if (equal (file-name-extension (or buffer-file-name "")) "html")
                            buffer-file-name
                          (buffer-string))))
         (file (or filename
                   (if (file-exists-p html)
                       (expand-file-name
                        (concat (file-name-base html) ".org")
                        (file-name-directory html))))))
    (with-temp-buffer
      (erase-buffer)
      (insert str)
      (org-table-map-tables 'org-table-align)
      (write-region (point-min) (point-max) file)
      (unless slient
        (find-file file)
        (write-file file nil)
        (goto-char (point-min))))))

(provide 'shrface)
;;; shrface.el ends here
