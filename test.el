;;; test.el -*- lexical-binding: t; -*-

(require 'shrface)

;; to org file with specified file name, same folder as html
(shrface-html-export-to-org
 "/var/folders/st/mkq0gxld3rv39t6y6zv45j540000gn/T/github-chenyanmingshrface-extend-ewwnov-with-org-mode-features-archive-web-pages-to-org-files-with-shr.html"
 "/var/folders/st/mkq0gxld3rv39t6y6zv45j540000gn/T/test.org")
;; to org file no file name provided
(shrface-html-export-to-org
 "/var/folders/st/mkq0gxld3rv39t6y6zv45j540000gn/T/github-chenyanmingshrface-extend-ewwnov-with-org-mode-features-archive-web-pages-to-org-files-with-shr.html")
;; to org file with specified file name, different folder with html
(shrface-html-export-to-org
 "/var/folders/st/mkq0gxld3rv39t6y6zv45j540000gn/T/github-chenyanmingshrface-extend-ewwnov-with-org-mode-features-archive-web-pages-to-org-files-with-shr.html"
 "test.org")
;; to org file with specified file name, relative html
(shrface-html-export-to-org "test.html" "hello.org")
;; no html and filename
(shrface-html-export-to-org)
;; convert sliently
(shrface-html-export-to-org "test.html" "helloworld-man.org" t)

;; html string to org string
(shrface-html-convert-as-org-string "<h1>hello</h1>")
;; html file (absolute) to org string
(shrface-html-convert-as-org-string "/var/folders/st/mkq0gxld3rv39t6y6zv45j540000gn/T/github-chenyanmingshrface-extend-ewwnov-with-org-mode-features-archive-web-pages-to-org-files-with-shr.html")
;; html file (relative) to org string
(shrface-html-convert-as-org-string "test.html")
;; no html
(shrface-html-convert-as-org-string)



;; html string to org string
(shrface-html-export-as-org "<h1>hello</h1>")
;; html file (absolute) to org string
(shrface-html-export-as-org "/var/folders/st/mkq0gxld3rv39t6y6zv45j540000gn/T/github-chenyanmingshrface-extend-ewwnov-with-org-mode-features-archive-web-pages-to-org-files-with-shr.html")
;; html file (relative) to org string
(shrface-html-export-as-org "test.html")
;; no html
(shrface-html-export-as-org)


(replace-regexp-in-string "\\(.*\\)/$" "\\1" "http://hello.com/")

(shrface-fix-url "/ox-hugo/mindmap-what-raspberrypi-can-do.jpg" "https://hkvim.com/post/calibredb/" )
(shrface-fix-url "post/calibredb/" "hkvim.com/" )
(shrface-fix-url "post/calibredb/" "hkvim.com/a" )

(shrface-fix-url "/post/calibredb/" "hkvim.com" )
(shrface-fix-url "/post/calibredb/" "hkvim.com/" )
(shrface-fix-url "/post/calibredb/" "hkvim.com/a" )

(shrface-fix-url "/post/calibredb/" "http://hkvim.com" )
(shrface-fix-url "/post/calibredb/" "http://hkvim.com/" )
(shrface-fix-url "/post/calibredb/" "http://hkvim.com/a" )

(shrface-fix-url "post/calibredb/" "http://hkvim.com/a" )
(shrface-fix-url "post/calibredb/" "http://hkvim.com/" )
(shrface-fix-url "post/calibredb/" "http://hkvim.com" )

(defun request-url-as-org (url)
  "Request URL and save it as org buffer."
  (interactive "sRequest url: ")
  (let ((name "request-url")
        (buf "*request-url*")
        (cmd (format "curl -L %s" url)))
    (if (get-buffer buf)
        (kill-buffer buf))
    (set-process-sentinel
     (start-process-shell-command name buf cmd)
     (lambda (p e)
       (when (= 0 (process-exit-status p))
         (with-current-buffer buf
           (shrface-html-export-as-org (buffer-string))))))))

(defun request-url-to-org (url)
  "Request URL and save it to org file."
  (interactive "sRequest url: ")
  (let ((name "request-url")
        (buf "*request-url*")
        (cmd (format "curl -L %s" url)))
    (if (get-buffer buf)
        (kill-buffer buf))
    (set-process-sentinel
     (start-process-shell-command name buf cmd)
     (lambda (p e)
       (when (= 0 (process-exit-status p))
         (with-current-buffer buf
           (shrface-html-export-to-org (buffer-string) "request.org")))))))


(defun request-url-at-point-as-org ()
  "Request URL with avy to org buffer."
  (interactive)
  (require 'link-hint)
  (link-hint-copy-link)
  (message "Connecting %s..." (current-kill 0 t))
  (request-url-as-org (current-kill 0 t)))
