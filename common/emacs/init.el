;; set custom file to load Customize interface changes
(setq custom-file "~/.emacs.d/custom.el")

;; load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;; load trading journal functions
(load-file (expand-file-name "capture-templates.el" user-emacs-directory))

;; load ui.el
(load (expand-file-name "ui.el" user-emacs-directory) t)
