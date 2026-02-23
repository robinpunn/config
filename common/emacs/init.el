;; customize (theme)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ui
(load (expand-file-name "ui.el" user-emacs-directory))

;; blog
(load (expand-file-name "publish.el" user-emacs-directory))

;; trading journal
(load (expand-file-name "trading-journal.el" user-emacs-directory))
