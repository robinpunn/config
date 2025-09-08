(defun my/extract-values-from-current-calculations-row ()
  "Extract needed values from the current row in calculations.org table."
  (list
   (cons 'ticker (org-table-get-field 1))
   (cons 'type (org-table-get-field 2))
   (cons 'kijun (org-table-get-field 4))
   (cons 'delta (org-table-get-field 13))
   (cons 'atr (org-table-get-field 5))
   (cons 'pba (org-table-get-field 6))))

(defun my/find-or-create-ticker-heading (ticker)
  "Find or create a ticker heading under * Watch section."
  (goto-char (point-min))
  (when (re-search-forward "^\\* Watch" nil t)
    (let* ((search-limit (save-excursion 
                           (if (re-search-forward "^\\* " nil t)
                               (line-beginning-position)
                             (point-max))))
           (clean-ticker (string-trim ticker))
           (found nil)
           (ticker-start nil))
      ;; Loop through ** headings after * Watch
      (while (and (not found)
                  (re-search-forward "^\\*\\* \\(.*\\)$" search-limit t))
        (let ((heading (string-trim (match-string 1))))
          (when (string= heading clean-ticker)
            (setq found t)
            (setq ticker-start (line-beginning-position)))))
      (if found
          ;; Go to the line after the ticker heading (top of its content)
          (progn
            (goto-char ticker-start)
            (forward-line 1)
            (point))
        ;; Create new ticker heading
        (goto-char search-limit)
        (unless (bolp) (insert "\n"))
        (insert "** " clean-ticker "\n")
        (point)))))

(defun my/insert-prep-trade ()
  "Insert a prepared trade using values extracted from the current table row in calculations.org."
  (interactive)
  (if (not (org-at-table-p))
      (message "Not in a table!")
    (let* ((vals (my/extract-values-from-current-calculations-row))
           (ticker (cdr (assoc 'ticker vals)))
           (raw-trade-type (cdr (assoc 'type vals)))
           ;; Clean up the type field - remove quotes and trim whitespace
           (trade-type (string-trim (replace-regexp-in-string "\"" "" raw-trade-type)))
           (kijun (cdr (assoc 'kijun vals)))
           (delta (cdr (assoc 'delta vals)))
           (atr (cdr (assoc 'atr vals)))
           (pba (cdr (assoc 'pba vals)))
           (confirm (y-or-n-p (format "Create %s trade template for %s? " trade-type ticker))))
      (when confirm
        (let* ((is-options (or (string= trade-type "call") (string= trade-type "put")))
               (trade-text
                (if is-options
                    (format
                     "*** %s\n - type: %s\n**** opening indicators\n***** daily\n - kijun\n   - value: %s\n   - direction:\n - pbar: %s\n - stoch:\n   - %%k:\n   - %%d:\n - cmf:\n   - value:\n   - direction:\n - rsi:\n   - value:\n   - direction:\n - atr: %s\n - hv:\n\n***** chain\n - delta: %s\n - gamma:\n - vega:\n - theta:\n - ivr:\n - ivx:\n\n**** open\n***** fill\n - date:\n - strike:\n - exp:\n - quantity:\n - price:\n**** close\n***** take profit 1\n - date:\n - strike:\n - exp:\n - quantity:\n - price:\n***** trade close\n - date:\n - strike:\n - exp:\n - quantity\n - price:\n\n**** lessons\n"
                     (format-time-string "%m/%d/%y") trade-type kijun pba atr delta)
                  (format
                   "*** %s\n - type: %s\n**** opening indicators\n***** daily\n - kijun\n   - value: %s\n   - direction:\n - pbar: %s\n - stoch:\n   - %%k:\n   - %%d:\n - cmf:\n   - value:\n   - direction:\n - rsi:\n   - value:\n   - direction:\n - atr: %s\n - hv:\n\n**** open\n***** fill\n - date:\n - quantity:\n - price:\n**** close\n***** take profit 1\n - date:\n - quantity:\n - price:\n***** trade close\n - date:\n - quantity\n - price:\n\n**** lessons\n"
                   (format-time-string "%m/%d/%y") trade-type kijun pba atr))))
          (with-current-buffer (find-file-noselect "~/Documents/Practice/trades.org")
            (let ((pos (my/find-or-create-ticker-heading ticker)))
              (goto-char pos)
              (unless (bolp) (insert "\n"))
              (insert trade-text)
              (save-buffer)
              (message "%s trade template created for %s" trade-type ticker)))))))))
