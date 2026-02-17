;; remove GUI 
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; clean appearance
(setq-default cursor-type 'bar)
(show-paren-mode 1)
;; (global-hl-line-mode 1)

;; line numbers and wrapping
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)

;; Dired defaults
(setq dired-listing-switches "--group-directories-first -alh")
