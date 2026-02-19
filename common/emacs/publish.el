(require 'org)
(require 'ox-publish)

(defvar my/blog-base-dir (expand-file-name "~/Documents/Blogs/"))
(defvar my/blog-org-dir (expand-file-name "org/" my/blog-base-dir))
(defvar my/blog-public-dir (expand-file-name "public/" my/blog-base-dir))
(defvar my/blog-static-dir (expand-file-name "static/" my/blog-base-dir))
(defvar my/blog-pi-path ("pi@192.168.0.33:/home/pi/blog/"))

(setq org-publish-project-alist
      `(
	("blog-org"
	 :base-directory ,my/blog-org-dir
	 :base-extension "org"
	 :publishing-directory ,my/blog-public-dir
	 :publishing-function org-html-publish-to-html
	 :with-tags t
	 :recursive t

	 :author 'Robin Punnoose'

	 :auto-sitemap 1
	 :sitemap-filename "index.org"
	 :sitemap-title "Blog Posts"
	 :sitemap-sort-files anti-chronologically

	 :section-numbers nil
	 :with-toc nil
	 :html-head "<link rel=\"stylesheet\" 
		href=\"/css/style.css\" 
	 	type=\"text/css\" />"
 	 :html-preamble nil
	 :html-postamble nil)

	("blog-static"
	 :base-directory ,my/blog-static-dir
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
	 :publishing-directory ,my/blog-public-dir
	 :recursive t
	 :publishing-function org-publish-attachment)

	("blog" :components ("blog-org" "blog-static"))))

(defun my/copy-from-notes()
  (interactive)
  (let* (src (buffer-file-name))
    	(dest (expand-file-name (file-name-nondirectory src) my/blog-org-dir)))
    (unless src
      (user-error "Current buffer is not visiting a file"))
    (unless (file-directory-p my/blog-org-dir)
      (make-directory my/blog-org-dir t))
    (copy-file src dest t)
    (message "Copied to blog: %s" (file-name-nondirectory src)))

(defun my/publish-post()
  (interactive)
  (org-publish-current-file))

(defun my/publish-blog()
  (interactive)
  (org-publish-project "blog"))

(defun my/publish-and-deploy()
  (interactive)
  (org-publish-project "blog")
  (shell-command "rsync -avz %s %s" my/blog-public-dir my/blog-pi-path))
