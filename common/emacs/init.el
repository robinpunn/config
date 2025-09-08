;; set custom file to load Customize interface changes
(setq custom-file "~/.emacs.d/custom.el")

;; load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;; load trading journal functions
(load-file (expand-file-name "capture-templates.el" user-emacs-directory))

;; remove GUI 
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (setq inhibit-startup-screen t)

;; clean appearance
(setq-default cursor-type 'bar)
(show-paren-mode 1)
;; (global-hl-line-mode 1)

;; line numbers 
(global-display-line-numbers-mode)
(global-visual-line-mode 1)

;; organize directory 
(setq dired-listing-switches "--group-directories-first -alh")
