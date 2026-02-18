(require 'org)
(require 'ox-publish)

(setq org-publish-project-alist
      '(
	("blog-org"
	 :base-directory "~/Documents/Blogs/org/"
	 :base-extension "org"
	 :publishing-directory "~/Documents/Blogs/public/"
	 :publishing-functon org-html-publish-to-html

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
	 :html-postable nil)

	("blog-static"
	 :base-directory "~/Documents/Blogs/static/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
	 :publishing-directory "~/Documents/Blogs/public/"
	 :recursive t
	 :publishing-functon org-publish-attachment)

	("blog" :components ("blog-org" "blog-static"))))
