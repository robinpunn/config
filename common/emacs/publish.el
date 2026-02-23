(require 'org)
(require 'ox-publish)

(setq my/blog-base-dir (expand-file-name "~/Blog/"))
(setq my/blog-org-dir (expand-file-name "org/" my/blog-base-dir))
(setq my/blog-public-dir (expand-file-name "public/" my/blog-base-dir))
(setq my/blog-static-dir (expand-file-name "static/" my/blog-base-dir))
(setq my/blog-pi-path "pi@192.168.0.33:/home/pi/Blog/")
(defvar my/blog-server-process nil)

(setq org-publish-project-alist
      `(("blog-org"
	 :base-directory ,my/blog-org-dir
	 :base-extension "org"
	 :publishing-directory ,my/blog-public-dir
	 :publishing-function org-html-publish-to-html
	 :with-tags t
	 :recursive t

	 :author "Robin Punnoose"

	 :auto-sitemap t
	 :sitemap-filename "index.org"
	 :sitemap-title "Blog Posts"
	 :sitemap-sort-files anti-chronologically

	 :section-numbers nil
	 :with-toc t
	 :html-head "<link rel=\"stylesheet\" 
		href=\"/css/pico.classless.min.css\" 
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
  (let* ((src (buffer-file-name))
    	(dest (expand-file-name (file-name-nondirectory src) my/blog-org-dir)))
    (unless src
      (user-error "Current buffer is not visiting a file"))
    (unless (file-directory-p my/blog-org-dir)
      (make-directory my/blog-org-dir t))
    (copy-file src dest t)
    (message "Copied to blog: %s" (file-name-nondirectory src))))

(defun my/blog-org-path ()
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (expand-file-name filename my/blog-org-dir)))

(defun my/blog-html-path ()
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (html-name (concat (file-name-sans-extension filename) ".html")))
    (expand-file-name html-name my/blog-public-dir)))

(defun my/open-blog-copy ()
  (interactive)
  (find-file (my/blog-org-path)))

(defun my/publish-current-blog-file ()
  (interactive)
  (org-publish-current-file t))

(defun my/start-blog-server()
  (unless (and my/blog-server-process
	       (process-live-p my/blog-server-process))
    (setq my/blog-server-process
	  (start-process "blog-server" "*blog-server*"
			 "python3" "-m" "http.server" "8080"
			 "--directory" my/blog-public-dir))
    (sleep-for 1)
    (message "Blog server started")))

(defun my/stop-blog-server()
  (interactive)
  (when (and my/blog-server-process
	     (process-live-p my/blog-server-process))
    (delete-process my/blog-server-process)
    (setq my/blog-server-process nil)
    (message "Blog server stopped")))

(defun my/open-blog-html ()
  (interactive)
  (let* ((html-path (my/blog-html-path))
	(html-url (concat "http://localhost:8080/"
			  (file-name-nondirectory html-path))))
    (unless (file-exists-p html-path)
      (user-error "Published HTML does not exist. Publish first."))
    (my/start-blog-server)
    (message "Opening: %s" html-url)
    (browse-url html-url)))

(defun my/preview-post-from-notes ()
  (interactive)
  (my/copy-from-notes)
  (my/open-blog-copy)
  (my/publish-current-blog-file)
  (my/open-blog-html))

(defun my/preview-post-from-source ()
  (interactive)
  (my/publish-current-blog-file)
  (my/open-blog-html))

(defun my/in-blog-source-p ()
  (and (buffer-file-name)
       (file-in-directory-p
        (buffer-file-name)
        my/blog-org-dir)))

(defun my/preview-post ()
  (interactive)
  (if (my/in-blog-source-p)
      (my/preview-post-from-source)
    (my/preview-post-from-notes)))

(defun my/publish-blog()
  (interactive)
  (org-publish-project "blog"))

(defun my/publish-and-deploy()
  (interactive)
  (my/publish-blog)
  (shell-command "rsync -avz %s %s" my/blog-public-dir my/blog-pi-path))
