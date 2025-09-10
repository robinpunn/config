;; Helper functions for trade movement between sections
(defun my/find-ticker-heading-position (date-pos)
  "Find the ** ticker heading position above the given date position."
  (save-excursion
    (goto-char date-pos)
    (unless (re-search-backward "^\\*\\* \\(.*\\)$" nil t)
      (error "Could not find ** ticker heading above current position"))
    (line-beginning-position)))

(defun my/cut-entire-ticker-section (ticker-pos)
  "Cut the entire ticker section starting at ticker-pos. Returns the cut text."
  (save-excursion
    (goto-char ticker-pos)
    ;; Find the end of this ticker section (next ** heading or next * section or end of file)
    (let ((start-pos ticker-pos)
          (end-pos (save-excursion
                     (forward-line 1)
                     (if (re-search-forward "^\\*\\* \\|^\\* " nil t)
                         (line-beginning-position)
                       (point-max)))))
      ;; Cut the entire section
      (goto-char start-pos)
      (let ((ticker-text (buffer-substring start-pos end-pos)))
        (delete-region start-pos end-pos)
        ticker-text))))

(defun my/find-section-in-trades (section-name)
  "Find a section (* Watch, * Open) in trades.org and return position after the heading."
  (goto-char (point-min))
  (unless (re-search-forward (format "^\\* %s" section-name) nil t)
    (error "Could not find * %s section in trades.org" section-name))
  ;; Find where to insert - after the section heading but before next * section
  (let ((section-start (line-end-position)))
    (forward-line 1)
    ;; Move to end of section content (before next * heading or end of file)
    (if (re-search-forward "^\\* " nil t)
        (line-beginning-position)
      (point-max))))

(defun my/paste-ticker-in-section (section-name ticker-text)
  "Paste ticker section into the specified section in trades.org."
  (let ((insert-pos (my/find-section-in-trades section-name)))
    (goto-char insert-pos)
    ;; Add newline if not at beginning of line or if there's content
    (unless (and (bolp) (or (eobp) (looking-at "^\\*")))
      (insert "\n"))
    (insert ticker-text)
    ;; Ensure there's a newline after our insertion
    (unless (bolp) (insert "\n"))))

;; Main movement function
(defun my/move-trade-watch-to-open ()
  "Move the current trade's ticker from Watch section to Open section in trades.org."
  (interactive)
  (save-excursion
    (let* ((date-pos (my/find-current-trade-date-heading))
           (ticker-pos (my/find-ticker-heading-position date-pos))
           (ticker-text (my/cut-entire-ticker-section ticker-pos)))
      ;; Paste into Open section
      (my/paste-ticker-in-section "Open" ticker-text)
      (save-buffer)
      (message "Trade moved from Watch to Open"))))

;; Helper functions for trade context and manipulation
(defun my/find-current-trade-date-heading ()
  "Find and move to the *** date heading for the current trade. Returns the position."
  (save-excursion
    (beginning-of-line)
    (unless (or (looking-at "^\\*\\*\\* ")
                (re-search-backward "^\\*\\*\\* " nil t))
      (error "Not in a trade entry (no *** date heading found)"))
    (point)))

(defun my/find-symbol-above-date (date-pos)
  "Find the ** symbol heading above the given date position. Returns the symbol string."
  (save-excursion
    (goto-char date-pos)
    (unless (re-search-backward "^\\*\\* \\(.*\\)$" nil t)
      (error "Could not find ** symbol heading above current position"))
    (string-trim (match-string 1))))

(defun my/find-and-extract-type-line (date-pos)
  "Find the '- type:' line after the date heading. Returns (type-value . type-line-pos)."
  (save-excursion
    (goto-char date-pos)
    (forward-line 1)
    (let ((search-limit (save-excursion
                          (if (re-search-forward "^\\*\\*\\*\\*" nil t)
                              (line-beginning-position)
                            (point-max))))
          (type-value nil)
          (type-line-pos nil))
      
      ;; Search for "- type: " line
      (while (and (< (point) search-limit)
                  (not type-value))
        (when (looking-at "^ *- type: \\(.*\\)$")
          (setq type-value (string-trim (match-string 1)))
          (setq type-line-pos (point)))
        (forward-line 1))
      
      (unless type-value
        (error "Could not find '- type: ' line in trade entry"))
      
      (cons type-value type-line-pos))))

(defun my/create-property-drawer-at-date (date-pos trade-id type-value direction)
  "Create a property drawer after the date heading at the given position."
  (let ((trade-type (if (or (string= type-value "call") (string= type-value "put"))
                        "options"
                      "stock")))
    (save-excursion
      (goto-char date-pos)
      (end-of-line)
      (insert "\n:PROPERTIES:")
      (insert (format "\n:TRADE_ID: %s" trade-id))
      (insert (format "\n:TYPE:     %s" trade-type))
      (insert (format "\n:DIRECTION: %s" direction))
      (insert "\n:STATUS:   open")
      (insert "\n:END:"))))

(defun my/remove-type-line (type-line-pos)
  "Remove the type line at the given position."
  (save-excursion
    (goto-char type-line-pos)
    (beginning-of-line)
    (kill-line 1)))

(defun my/check-for-existing-property-drawer (date-pos)
  "Check if a property drawer already exists after the date heading. Returns t if exists."
  (save-excursion
    (goto-char date-pos)
    (forward-line 1)
    (looking-at "^:PROPERTIES:")))

;; Main interactive functions

(defun my/create-trade-id ()
  "Create a tradeID and property drawer for the current trade entry."
  (interactive)
  (let* ((date-pos (my/find-current-trade-date-heading))
         (symbol (my/find-symbol-above-date date-pos))
         (type-info (my/find-and-extract-type-line date-pos))
         (type-value (car type-info))
         (type-line-pos (cdr type-info))
         (timestamp (format-time-string "%y%m%d-%H%M"))
         (trade-id (format "%s-%s" symbol timestamp)))
    
    ;; Check for existing property drawer
    (when (my/check-for-existing-property-drawer date-pos)
      (unless (y-or-n-p "Property drawer already exists. Overwrite? ")
        (error "Aborted - property drawer already exists")))
    
    ;; Remove the type line first (to avoid position shifting)
    (my/remove-type-line type-line-pos)
    
    ;; Create the property drawer
    (my/create-property-drawer-at-date date-pos trade-id type-value type-value)
    
    (message "Trade ID created: %s" trade-id)))

;; Helper functions for calculations -> trades workflow

(defun my/extract-and-clean-calculations-data ()
  "Extract and clean values from the current calculations table row."
  (let ((vals (my/extract-values-from-current-calculations-row)))
    (list
     (cons 'ticker (cdr (assoc 'ticker vals)))
     (cons 'trade-type (string-trim 
                        (replace-regexp-in-string "\"" "" 
                                                 (cdr (assoc 'type vals)))))
     (cons 'kijun (string-trim (cdr (assoc 'kijun vals))))
     (cons 'delta (string-trim (cdr (assoc 'delta vals))))
     (cons 'atr (string-trim (cdr (assoc 'atr vals))))
     (cons 'pba (string-trim (cdr (assoc 'pba vals)))))))

(defun my/generate-trade-template (trade-type kijun pba atr delta)
  "Generate the trade template text based on trade type and indicators."
  (let ((is-options (or (string= trade-type "call") (string= trade-type "put")))
        (date-str (format-time-string "%m/%d/%y")))
    (if is-options
        (format
         "*** %s\n - type: %s\n**** opening indicators\n***** daily\n - kijun\n   - value: %s\n   - direction:\n - pbar: %s\n - stoch:\n   - %%k:\n   - %%d:\n - cmf:\n   - value:\n   - direction:\n - rsi:\n   - value:\n   - direction:\n - atr: %s\n - hv:\n\n***** chain\n - delta: %s\n - gamma:\n - vega:\n - theta:\n - ivr:\n - ivx:\n\n**** open\n***** fill\n - date:\n - strike:\n - exp:\n - quantity:\n - price:\n**** close\n***** take profit 1\n - date:\n - strike:\n - exp:\n - quantity:\n - price:\n***** trade close\n - date:\n - strike:\n - exp:\n - quantity\n - price:\n\n**** lessons\n"
         date-str trade-type kijun pba atr delta)
      (format
       "*** %s\n - type: %s\n**** opening indicators\n***** daily\n - kijun\n   - value: %s\n   - direction:\n - pbar: %s\n - stoch:\n   - %%k:\n   - %%d:\n - cmf:\n   - value:\n   - direction:\n - rsi:\n   - value:\n   - direction:\n - atr: %s\n - hv:\n\n**** open\n***** fill\n - date:\n - quantity:\n - price:\n**** close\n***** take profit 1\n - date:\n - quantity:\n - price:\n***** trade close\n - date:\n - quantity\n - price:\n\n**** lessons\n"
       date-str trade-type kijun pba atr))))

(defun my/insert-trade-template-in-trades-file (ticker trade-text)
  "Insert the trade template in the trades.org file under the appropriate ticker."
  (with-current-buffer (find-file-noselect "~/Documents/Practice/trades.org")
    (let ((pos (my/find-or-create-ticker-heading ticker)))
      (goto-char pos)
      (unless (bolp) (insert "\n"))
      (insert trade-text)
      (save-buffer))))

;; Refactored main function

(defun my/insert-prep-trade ()
  "Insert a prepared trade using values extracted from the current table row in calculations.org."
  (interactive)
  (if (not (org-at-table-p))
      (message "Not in a table!")
    (let* ((data (my/extract-and-clean-calculations-data))
           (ticker (cdr (assoc 'ticker data)))
           (trade-type (cdr (assoc 'trade-type data)))
           (kijun (cdr (assoc 'kijun data)))
           (delta (cdr (assoc 'delta data)))
           (atr (cdr (assoc 'atr data)))
           (pba (cdr (assoc 'pba data))))
      
      (when (y-or-n-p (format "Create %s trade template for %s? " trade-type ticker))
        (let ((trade-text (my/generate-trade-template trade-type kijun pba atr delta)))
          (my/insert-trade-template-in-trades-file ticker trade-text)
          (message "%s trade template created for %s" trade-type ticker))))))
