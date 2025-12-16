; Trading Journal
;; use test files to fix bugs
(defun my/trading-with-test-files (func)
  "Execute FUNC with test trading files."
  (let ((my-trading-trades-file "~/Documents/TradeBrain/test/trades.org")
        (my-trading-calculations-file "~/Documents/TradeBrain/test/calculate.org")
        (my-trading-summary-file "~/Documents/TradeBrain/test/summary.org"))
    (funcall func)))

;; Usage:
;; (my-trading-with-test-files #'my-trading-calculate-whatever)

;; Custom Variables
(defgroup my-trading nil
  "Trading journal workflow customization."
  :group 'convenience)

(defcustom my-trading-trades-file "~/Documents/TradeBrain/trades.org"
  "Path to the trades.org file."
  :type 'file
  :group 'my-trading)

(defcustom my-trading-calculations-file "~/Documents/TradeBrain/calculate.org"
  "Path to the calculate.org file."
  :type 'file
  :group 'my-trading)

(defcustom my-trading-summary-file "~/Documents/TradeBrain/summary.org"
  "Path to the summary.org file."
  :type 'file
  :group 'my-trading)

(defconst my/prop-open-value  
  "OPEN_VALUE")

(defconst my/prop-tp1-value   
  "TP1_VALUE")

(defconst my/prop-final-value 
  "TRADE_CLOSE")

(defconst my/prop-pnl         
  "P&L")

;; Generic Utility Functions
;;;; buffers
(defmacro my/with-trading-file (file-var &rest body)
  `(with-current-buffer (find-file-noselect ,file-var)
     (prog1 (progn ,@body)
       (save-buffer))))

(defun my/find-buffer-for-filename (file-name)
  (or (find-buffer-visiting file-name)
      (find-file-noselect file-name)))

(defun my/subtree-first-line ()
  (save-excursion
    (forward-line 1)
    (point)))

;;;; headings
(defun my/find-org-heading (level text &optional direction limit)
  (let* ((stars (format "\\*\\{%d\\}" level))           
         (name-part (if text (regexp-quote text) "\\(.*\\)"))
         (pattern (format "^%s %s" stars name-part))
         (search-func (if (eq direction 'backward) 're-search-backward 're-search-forward)))
    (save-excursion
      (when (funcall search-func pattern limit t)
        (line-beginning-position)))))

(defun my/get-org-heading-content (level &optional direction)
  (let* ((stars (format "\\*\\{%d\\}" level))
         (pattern (format "^%s \\(.*\\)$" stars))
         (search-func (if (eq direction 'forward) 're-search-forward 're-search-backward)))
    (save-excursion
      (when (funcall search-func pattern nil t)
        (string-trim (match-string 1))))))

(defun my/goto-heading-end (level)
  (save-excursion
    (forward-line 1)
    (let ((next-heading-pattern (format "^\\*\\{1,%d\\} " level)))
      (if (re-search-forward next-heading-pattern nil t)
          (line-beginning-position)
        (point-max)))))

(defun my/get-heading-end (pos level)
  (save-excursion
    (goto-char pos)
    (outline-next-heading)
    (point)))

(defun my/goto-heading (heading-path file-name)
  (let ((parts (split-string heading-path "/"))
        (buf (my/find-buffer-for-filename file-name)))
    (with-current-buffer buf
      (goto-char (point-min))
      (dolist (part parts)
        (unless (re-search-forward
                 (format org-complex-heading-regexp-format (regexp-quote part))
                 nil t)
          (error "Heading not found: %s" part)))
      (point))))

(defun my/find-current-trade-date-heading ()
  (or (when (looking-at "^\\*\\*\\* ")
        (point))
      (let ((pos (save-excursion
                   (re-search-backward "^\\*\\*\\* " nil t))))
        (if pos
            pos
          (error "Not inside a trade entry (no *** date heading found)")))))

(defun my/goto-current-trade ()
  (goto-char (my/find-current-trade-date-heading)))

(defun my/insert-heading-at-point (level name)
  (let ((stars (make-string (max 1 level) ?*))
        (start-pos nil))
    ;; Ensure we are at a line boundary for cleaner insertion.
    (unless (bolp)
      (newline))
    (setq start-pos (point))
    (insert (format "%s %s\n" stars name))
    ;; Return beginning of inserted heading line
    (goto-char start-pos)
    start-pos))

(defun my/find-or-create-heading (level text)
  (goto-char (point-min))
  (let ((heading-point (my/find-org-heading level text)))
    (unless heading-point
      (goto-char (point-max))
      (setq heading-point (my/insert-heading-at-point level text)))
    (goto-char heading-point)))

;;;; get date and year
(defun my/get-current-date ()
  "Return today's date as a string in MM/DD/YY format."
  (format-time-string "%m/%d/%y"))

(defun my/get-trade-date-string ()
  (let ((heading (my/get-org-heading-content 3)))
    (when heading
      (substring-no-properties heading))))

(defun my/parse-trade-date (date-str)
  (when (string-match "\\`\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\'" date-str)
    (let ((month (match-string 1 date-str))
          (day   (match-string 2 date-str))
          (year (match-string 3 date-str)))
      `((:month . ,month)
        (:day . ,day)
        (:year . ,year)))))

(defun my/get-month-from-trade-heading (date-str)
  (let ((date-alist (my/parse-trade-date date-str)))
    (alist-get :month date-alist)))

(defun my/month-number-to-name (month-str)
  (let ((month-num (string-to-number month-str)))
    (if (and (>= month-num 1) (<= month-num 12))
        (calendar-month-name month-num)
      (error "Invalid month string: %s" month-str))))

(defun my/get-year-from-trade-heading (date-str)
  (let ((date-alist (my/parse-trade-date date-str)))
    (alist-get :year date-alist)))

(defun my/expand-year (yy)
  (format "20%s" yy))

;; tables
;;;; find Table 
(defun my/table-exists-in-current-subtree-p ()
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t))))
      (re-search-forward org-table-any-line-regexp end t))))

(defun my/find-table-in-subtree ()
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t))))
      (when (re-search-forward org-table-any-line-regexp end t)
        (match-beginning 0)))))

(defun my/find-table-in-section (heading-path file-name)
  (let ((buf (my/find-buffer-for-filename file-name)))
    (with-current-buffer buf
      (my/goto-heading heading-path file-name)
      (let ((end (save-excursion (org-end-of-subtree t))))
        (unless (re-search-forward org-table-any-line-regexp end t)
          (error "No table found in section: %s" heading-path))
        (beginning-of-line)
        (point)))))

(defun my/get-table-end-position (heading-path file-name)
  (let ((buf (my/find-buffer-for-filename file-name)))
    (with-current-buffer buf
      (my/goto-heading heading-path file-name)
      (let ((end (save-excursion (org-end-of-subtree t))))
        (unless (re-search-forward org-table-any-line-regexp end t)
          (error "No table found in section: %s" heading-path))
        (beginning-of-line)
        (while (looking-at org-table-any-line-regexp)
          (forward-line 1))
        (point)))))

(defun my/get-table-bounds (section file)
  (let ((start (my/find-table-in-section section file))
        (end   (my/get-table-end-position section file)))
    (list start end)))

(defun my/get-table-header ()
  (let ((table (org-table-to-lisp)))
    (car table)))

(defun my/normalize-table-header (header-row)
  (mapcar (lambda (col)
            (let* ((name (downcase (string-trim col)))
                   (name (replace-regexp-in-string "[^a-z0-9]+" "_" name)))
              (intern (concat ":" name))))
          header-row))

(defun my/get-table-schema (heading-path file-name)
  (let ((buf (my/find-buffer-for-filename file-name)))
    (with-current-buffer buf
      (goto-char (my/find-table-in-section heading-path file-name))
      (my/normalize-table-header (my/get-table-header)))))

(defun my/go-to-end-of-table-at (table-start)
  (goto-char table-start)
  (while (looking-at org-table-any-line-regexp)
    (forward-line 1))
  (point))

;;;; Write to table 
(defun my/insert-table-row-at-point (row)
  (insert (concat "| " (mapconcat #'identity row " | ") " |\n"))
  (org-table-align))

(defun my/build-open-options-row (data schema)
  (mapcar (lambda (col)
            (pcase col
              (:id     (alist-get :trade_id data))
              (:type   (alist-get :direction data))
              (:init   (alist-get :init data))
              (:atr    (alist-get :atr data))
              (:risk   (alist-get :risk data))
              (:delta  (alist-get :delta data))
              (:prem   (alist-get :price data))
              (_ ""))) ;; all other columns remain empty
          schema))

(defun my/build-manage-options-row (data schema)
  "Build a row for Manage table from DATA, respecting SCHEMA.
Fills id, type, init, premium, delta, gamma. Leaves other fields blank."
  (mapcar (lambda (col)
            (pcase col
              (:id     (alist-get :trade_id data))
              (:type   (alist-get :direction data))
              (:init   (alist-get :init data))
              (:prem   (alist-get :price data))
              (:delta  (alist-get :delta data))
              (:gamma  (alist-get :gamma data))
              (_ ""))) ;; all other columns remain empty
          schema))

(defun my/build-open-stocks-row (data schema)
  "Build a row for Open/Stocks table from DATA, respecting SCHEMA."
  (mapcar (lambda (col)
            (pcase col
              (:id   (alist-get :trade_id data))
              (:type (alist-get :direction data))
              (:init (alist-get :init data))
              (:atr  (alist-get :atr data))
              (:risk (alist-get :risk data))
              (_ "")))
          schema))

(defun my/format-cell (col val)
  "Format VAL for table cell based on COL.
Direction (:type) is always quoted, numbers are raw, nil is empty."
  (cond
   ((null val) "")                             ; blank
   ((eq col :type) (format "\"%s\"" val))      ; direction quoted
   ((numberp val) (number-to-string val))      ; numbers raw
   ((stringp val) val)                         ; plain string
   (t (format "%s" val))))                     ; fallback

(defun my/write-row-to-table (row section file schema)
  "Append ROW to the table under SECTION in FILE, formatting with SCHEMA."
  (my/with-trading-file file
    (goto-char (my/get-table-end-position section file))
    (insert
     (concat "|"
             (mapconcat (lambda (pair)
                          (my/format-cell (car pair) (cdr pair)))
                        (cl-mapcar #'cons schema row)
                        "|")
             "|\n"))
    (org-table-align)))

;;;; Remove row from table
(defun my/find-row-by-trade-id (trade-id)
  (save-excursion
    ;; Ensure we start in the table
    (while (not (org-at-table-p))
      (forward-line 1))
    (let ((found nil))
      (while (and (org-at-table-p) (not found))
        (let ((row-id (string-trim (or (org-table-get-field 1) ""))))
          (if (string= row-id trade-id)
              (setq found (point))
            (forward-line 1))))
      found)))

(defun my/delete-current-table-row ()
  "Delete the current Org table row if point is inside a table row."
  (if (org-at-table-p)
      (org-table-kill-row)
    (error "Not at a table row")))

(defun my/delete-trade-from-calculate-table (file-name heading-path trade-id)
  (let ((buf (my/find-buffer-for-filename file-name)))
    (with-current-buffer buf
      (let ((table-start (my/find-table-in-section heading-path file-name))
            (table-end   (my/get-table-end-position heading-path file-name))
            (deleted nil))
        (save-excursion
          (goto-char table-start)
          (let ((row-pos (my/find-row-by-trade-id trade-id)))
            (when (and row-pos (< row-pos table-end))
              (goto-char row-pos)
              (my/delete-current-table-row)
              (setq deleted t))))
        (unless deleted
          (error "Trade %s not found in %s table" trade-id heading-path)))))) 

;; Property Drawer Utilities
(defun my/find-property-drawer-in-region (beg end)
  (save-excursion
    (goto-char beg)
    (when (re-search-forward "^:PROPERTIES:$" end t)
      (let ((pos (match-beginning 0)))
        (save-excursion
          (goto-char pos)
          (when (= (current-column) 0)
            pos))))))

(defun my/find-property-drawer-in-regionOG (beg end)
  (save-excursion
    (goto-char beg)
    (when (re-search-forward "^:PROPERTIES:" end t)
      (match-beginning 0))))

(defun my/get-property-drawer-end-in-region (drawer-start subtree-end)
  (when drawer-start
    (save-excursion
      (goto-char drawer-start)
      (when (re-search-forward "^:END:$" subtree-end t)
        (let ((pos (match-beginning 0)))
          (save-excursion
            (goto-char pos)
            (when (= (current-column) 0)
              pos)))))))

(defun my/get-property-drawer-end-in-regionOG (drawer-start subtree-end)
  (when drawer-start
    (save-excursion
      (goto-char drawer-start)
      (when (re-search-forward "^:END:" subtree-end t)
        (match-beginning 0)))))

(defun my/get-property-value-in-region (property beg end)
  (save-excursion
    (goto-char beg)
    (let ((prop (upcase property)))
      (when (re-search-forward (format "^:%s:\\s-*\\(.*\\)$" prop) end t)
        (string-trim (match-string 1))))))

(defun my/set-property-value-in-region (property value drawer-start drawer-end)
  (when (and drawer-start drawer-end)
    (save-excursion
      (goto-char drawer-start)
      (let* ((prop property)
             (prop-regex (format "^:%s:\\s-*\\(.*\\)$" prop)))
        (if (re-search-forward prop-regex drawer-end t)
            (replace-match (format ":%s: %s" prop value) t t)
          (goto-char drawer-end)
          (beginning-of-line)
          (insert (format ":%s: %s\n" prop value)))))))

(defun my/find-drawer-end-in-subtree ()
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree t))))
      (when (re-search-forward "^:END:" end t)
        (forward-line 1)
        (point)))))

(defun my/find-trade-property-drawer ()
  (save-excursion
    (let* ((start (my/find-current-trade-date-heading))
           (end   (save-excursion
                    (goto-char start)
                    (outline-next-heading)
                    (point))))
      (my/find-property-drawer-in-region start end))))

(defun my/find-trade-property-drawer-end ()
  (let ((start (my/find-trade-property-drawer)))
    (when start
      (save-excursion
        (goto-char start)
        (when (re-search-forward "^:END:" (save-excursion (outline-next-heading) (point)) t)
          (match-beginning 0))))))

(defun my/update-existing-property (property value)
  (save-excursion
    (let* ((prop (upcase property))
           (drawer-start (my/find-trade-property-drawer))
           (drawer-end   (my/find-trade-property-drawer-end)))
      (when (and drawer-start drawer-end)
        (goto-char drawer-start)
        (if (re-search-forward (format "^:%s:.*" prop) drawer-end t)
            (replace-match (format ":%s: %s" prop value) t t)
          (goto-char drawer-end)
          (beginning-of-line)
          (insert (format ":%s: %s\n" prop value)))))))

(defun my/add-new-property (property value)
  (let ((end-pos (my/find-trade-property-drawer-end)))
    (unless end-pos
      (error "No property drawer found in current trade"))
    (save-excursion
      (goto-char end-pos)
      (insert (format ":%s: %s\n" (upcase property) value)))))

(defun my/close-trade-property-drawer ()
  (let ((outcome (completing-read "Outcome (win/half/loss): "
                                  '("win" "half" "loss") nil t)))
    (my/update-existing-property "STATUS" "closed")
    (my/add-new-property "OUTCOME" outcome)))

(defun my/property-drawer-exists-p ()
  "Check if a property drawer exists at current heading."
  (save-excursion
    (forward-line 1)
    (looking-at "^:PROPERTIES:")))

(defun my/read-trade-property-value (property)
  (let ((start (my/find-trade-property-drawer))
        (end   (my/find-trade-property-drawer-end)))
    (when (and start end)
      (save-excursion
        (goto-char start)
        (when (re-search-forward
               (format "^:%s: +\\(.*\\)$" (upcase property)) end t)
          (string-trim (substring-no-properties (match-string 1))))))))

(defun my/write-property-drawer (properties)
  (save-excursion
    (end-of-line)
    (insert "\n:PROPERTIES:")
    (dolist (prop properties)
      (insert (format "\n:%s: %s" (upcase (car prop)) (cdr prop))))
    (insert "\n:END:")))

(defun my/extract-properties-data ()
  (let ((data '())
        (start-pos (my/find-current-trade-date-heading)))
    (save-excursion
      (goto-char start-pos)
      (dolist (p (org-entry-properties nil 'standard))
        (let ((k (intern (concat ":" (replace-regexp-in-string "[[:space:]]+" "-"
                                                             (downcase (car p)))))))
          (push (cons k (my/clean-extracted-value (cdr p))) data))))
    (nreverse data)))

(defun my/get-prop (name)
  (org-entry-get (point) name))

(defun my/create-property-drawer-at-date (date-pos trade-id type-value direction)
  (let* ((type-info (my/parse-trade-type type-value))
         (properties `(("TRADE_ID" . ,trade-id)
                      ("TYPE" . ,(car type-info))
                      ("DIRECTION" . ,direction)
                      ("STATUS" . "open"))))
    (save-excursion
      (goto-char date-pos)
      (my/write-property-drawer properties))))

;; Data Processing Utilities
(defun my/clean-table-value (value)
  "Clean a table value by trimming and removing quotes."
  (when value
    (string-trim (replace-regexp-in-string "\"" "" value))))

(defun my/parse-trade-type (type-value)
  "Parse trade type value and return standardized type info.
Returns (trade-category . original-type)."
  (let ((clean-type (my/clean-table-value type-value)))
    (cons (if (member clean-type '("call" "put")) "options" "stock")
          clean-type)))

(defun my/extract-trade-data ()
  "Extract key trade data from current heading context.
Returns an alist with ticker, type info, and position."
  (let* ((date-pos (my/find-current-trade-date-heading))
         (ticker (save-excursion
                  (goto-char date-pos)
                  (my/get-org-heading-content 2 'backward)))
         (type-info (my/find-and-extract-type-line date-pos))
         (risk-info (my/find-and-extract-risk-line date-pos)))
    `((date-pos . ,date-pos)
      (ticker . ,ticker)
      (type-value . ,(car type-info))
      (risk-value . ,(car risk-info))
      (type-pos . ,(cdr type-info))
      (risk-pos . ,(cdr risk-info))
      (type-info . ,(my/parse-trade-type (car type-info))))))

(defun my/find-ticker-heading-position (date-pos)
  "Find the ** ticker heading position above the given date position."
  (save-excursion
    (goto-char date-pos)
    (or (my/find-org-heading 2 nil 'backward)
        (error "Could not find ** ticker heading above current position"))))

(defun my/find-and-extract-type-line (date-pos)
  "Find the '- type:' line after the date heading. Returns (type-value . type-line-pos)."
  (save-excursion
    (goto-char date-pos)
    (forward-line 1)
    (let ((search-limit (save-excursion
			  (if (re-search-forward "^\\*\\*\\*\\*" nil t)
                              (line-beginning-position) (point-max))))
          (type-value nil)
          (type-line-pos nil))
      
      ;; Search for "- type: " line
      (while (and (< (point) search-limit) (not type-value))
        (when (looking-at "^ *- type: \\(.*\\)$")
          (setq type-value (string-trim (match-string 1)))
          (setq type-line-pos (point)))
        (forward-line 1))
      
      (unless type-value
        (error "Could not find '- type: ' line in trade entry"))
      
      (cons type-value type-line-pos))))

(defun my/find-and-extract-risk-line (date-pos)
  "Find the '- type:' line after the date heading. Returns (risk-value . risk-line-pos)."
  (save-excursion
    (goto-char date-pos)
    (forward-line 1)
    (let ((search-limit (save-excursion
			  (if (re-search-forward "^\\*\\*\\*\\*" nil t)
                              (line-beginning-position)
                            (point-max))))
          (risk-value nil)
          (risk-line-pos nil))
      
      ;; Search for "- risk: " line
      (while (and (< (point) search-limit) (not risk-value))
        (when (looking-at "^ *- risk: \\(.*\\)$")
          (setq risk-value (string-trim (match-string 1)))
          (setq risk-line-pos (point)))
        (forward-line 1))
      
      (unless risk-value
        (error "Could not find '- risk: ' line in trade entry"))
      
      (cons risk-value risk-line-pos))))

(defun my/cut-entire-ticker-section (ticker-pos)
  "Cut the entire ticker section starting at ticker-pos. Returns the cut text."
  (save-excursion
    (goto-char ticker-pos)
    (let* ((start-pos ticker-pos)
           (end-pos (my/goto-heading-end 2))
           (ticker-text (buffer-substring start-pos end-pos)))
      (delete-region start-pos end-pos)
      ticker-text)))

(defun my/find-section-in-trades (section-name)
  "Find a section (* Watch, * Open) in trades.org and return position just after the heading."
  (my/with-trading-file my-trading-trades-file
    (goto-char (point-min))
    (let ((pos (my/find-org-heading 1 section-name)))
      (unless pos
        (error "Could not find * %s section in trades.org" section-name))
      (goto-char pos)
      (forward-line 1)
      (point))))

(defun my/paste-ticker-in-section (section-name ticker-text)
  "Paste ticker section into the specified section in trades.org as the first underlying."
  (my/with-trading-file my-trading-trades-file
    (let ((insert-pos (my/find-section-in-trades section-name)))
      (goto-char insert-pos)
      (unless (or (bolp) (looking-at "^\\s-*$"))
        (insert "\n"))
      (insert ticker-text)
      (unless (bolp) (insert "\n")))))

(defun my/remove-type-line (type-line-pos)
  "Remove the type line at the given position."
  (save-excursion
    (goto-char type-line-pos)
    (beginning-of-line)
    (kill-line 1)))

(defun my/remove-risk-line (risk-line-pos)
  "Remove the risk line at the given position."
  (save-excursion
    (goto-char risk-line-pos)
    (beginning-of-line)
    (kill-line 1)))

(defun my/find-or-create-ticker-heading (ticker)
  "Find or create a ** ticker heading in trades.org under Watch section.
Returns position after the heading."
  (my/with-trading-file my-trading-trades-file
    (goto-char (point-min))
    (let ((watch-pos (my/find-org-heading 1 "Watch")))
      (unless watch-pos
        (error "Could not find * Watch section"))
      
      ;; Look for existing ticker heading
      (goto-char watch-pos)
      (let ((ticker-pos (my/find-org-heading 2 ticker 'forward 
                                            (my/goto-heading-end 1))))
        (if ticker-pos
            (progn (goto-char ticker-pos) (forward-line 1) (point))
          ;; Create new ticker heading
          (goto-char watch-pos)
          (forward-line 1)
          (insert (format "** %s\n" ticker))
          (point))))))

(defun my/extract-trade-date ()
  (save-excursion
    (goto-char (my/find-current-trade-date-heading))
    (let ((line (thing-at-point 'line t)))
      ;; Extract everything after the stars and space
      (if (string-match "^\\*\\*\\* \\(.*\\)$" line)
          (string-trim (match-string 1 line))
        (error "Could not extract date from heading")))))

(defun my/extract-ticker-data ()
  (save-excursion
    (goto-char (my/find-ticker-heading-position (my/find-current-trade-date-heading)))
    (let ((line (thing-at-point 'line t)))
      ;; Extract everything after the stars and space
      (if (string-match "^\\*\\* \\(.*\\)$" line)
          (string-trim (match-string 1 line))
        (error "Could not extractte ticker")))))

(defun my/extract-sections-data (&optional exclude-sections start-pos trade-end)
  (setq start-pos (or start-pos (my/find-current-trade-date-heading)))
  (setq trade-end (or trade-end
                      (save-excursion
                        (goto-char start-pos)
                        (my/goto-heading-end 3))))
  (setq exclude-sections (or exclude-sections '("close" "lessons")))
  
  (let ((data '()))
    (save-excursion
      (goto-char start-pos)
      (while (re-search-forward "^\\*\\*\\*\\* \\(.*\\)$" trade-end t)
        (let* ((section (downcase (string-trim (match-string 1))))
               (sec-beg (match-beginning 0))
               (sec-end (save-excursion
                          (goto-char sec-beg)
                          (my/goto-heading-end 4))))
          (unless (member section exclude-sections)
            (save-excursion
              (goto-char (1+ sec-beg))
              (while (re-search-forward "^[ \t]*- " sec-end t)
                (beginning-of-line)
                (setq data
                      (append
                       (mapcar (lambda (cons)
                                 (cons (car cons)
                                       (my/clean-extracted-value (cdr cons))))
                               (my/parse-indented-list-for-extractor sec-end))
                       data))))))))
    data))

(defun my/get-section-text (level name &optional limit)
  (let ((pos (my/find-org-heading level name nil limit)))
    (when pos
      (save-excursion
        (let* ((beg (progn (goto-char pos)
                           (forward-line 1)
                           (point)))
               (end (my/get-heading-end pos level)))
          (buffer-substring-no-properties beg end))))))

;;;; extract data from list
(defun my/get-field (data key)
  (alist-get key data nil nil #'eq))

;;;; get open and close dates for a trade
(defun my/get-open-date ()
  (my/get-field
   (my/extract-sections-data '("opening indicators" "close" "lessons"))
   :date))

(defun my/get-close-date ()
  (my/get-field
   (my/extract-sections-data '("opening indicators" "fill" "lessons"))
   :date))

;;;; calculate.org data extraction
(defun my/extract-values-from-current-calculations-row ()
  "Extract needed values from the current row in calculations.org table."
  (list
   (cons 'ticker (org-table-get-field 1))
   (cons 'type (org-table-get-field 2))
   (cons 'kijun (org-table-get-field 4))
   (cons 'delta (org-table-get-field 13))
   (cons 'atr (org-table-get-field 5))
   (cons 'pba (org-table-get-field 6))
   (cons 'risk (org-table-get-field 7))))

(defun my/extract-and-clean-calculations-data ()
  "Extract and clean values from the current calculations table row."
  (let ((vals (my/extract-values-from-current-calculations-row)))
    (mapcar (lambda (pair)
              (cons (car pair) (my/clean-table-value (cdr pair))))
            `((ticker . ,(cdr (assoc 'ticker vals)))
              (trade-type . ,(cdr (assoc 'type vals)))
              (kijun . ,(cdr (assoc 'kijun vals)))
              (delta . ,(cdr (assoc 'delta vals)))
              (atr . ,(cdr (assoc 'atr vals)))
              (pba . ,(cdr (assoc 'pba vals)))
	      (risk . ,(cdr (assoc 'risk vals)))))))

;; Template Generation
(defun my/generate-trade-template (trade-type risk kijun pba atr delta)
  "Generate the trade template text based on trade type and indicators."
  (let* ((type-info (my/parse-trade-type trade-type))
         (is-options (string= (car type-info) "options"))
         (date-str (format-time-string "%m/%d/%y"))
         (base-template 
          (format "*** %s\n- type: %s\n- risk: %s\n**** opening indicators\n***** daily\n- kijun-value: %s\n- kijun-direction:\n- pbar: %s\n- stoch-%%k:\n- stoch-%%d:\n- cmf-value:\n- cmf-direction:\n- rsi-value:\n- rsi-direction:\n- atr: %s\n- hv:\n\n"
                  date-str trade-type risk kijun pba atr)))
    (concat base-template
            (if is-options
                (format "***** chain\n- delta: %s\n- gamma:\n- vega:\n- theta:\n- ivr:\n- ivx:\n\n**** fill\n- date:\n- strike:\n- exp:\n- quantity:\n- price:\n**** close\n***** take profit 1\n- date:\n- strike:\n- exp:\n- quantity:\n- price:\n***** trade close\n- date:\n- strike:\n- exp:\n- quantity\n- price:\n\n**** lessons\n" delta)
              "**** fill\n- date:\n- quantity:\n- price:\n**** close\n***** take profit 1\n- date:\n- quantity:\n- price:\n***** trade close\n- date:\n- quantity\n- price:\n\n**** lessons\n"))))

(defun my/insert-trade-template-in-trades-file (ticker trade-text)
  "Insert the trade template in the trades.org file under the appropriate ticker."
  (my/with-trading-file my-trading-trades-file
    (let ((pos (my/find-or-create-ticker-heading ticker)))
      (goto-char pos)
      (unless (bolp) (insert "\n"))
      (insert trade-text))))

;; Create trade
(defun my/move-trade-watch-to-open ()
  (let* ((date-pos (my/find-current-trade-date-heading))
         (ticker-pos (my/find-ticker-heading-position date-pos))
         (ticker-text (my/cut-entire-ticker-section ticker-pos)))
    (my/paste-ticker-in-section "Open" ticker-text)
    (message "Trade moved from Watch to Open")))

(defun my/trade-property-drawer (date-pos type-line-pos risk-line-pos trade-id direction init-price trade-type risk)
   (let* ((dir (when direction (downcase (string-trim direction)))) 
         (properties `(("TRADE_ID" . ,trade-id)
                       ("TYPE"     . ,trade-type)
                       ("DIRECTION". ,dir)
                       ("INIT"     . ,init-price)
		       ("RISK"     . ,(string-to-number risk))
                       ("STATUS"   . "open"))))
    ;; Remove original "- type:" line first (if provided)
    (when risk-line-pos
      (my/remove-risk-line risk-line-pos))
    (when type-line-pos
      (my/remove-type-line type-line-pos))
    ;; Write the property drawer at the date heading
    (save-excursion
      (goto-char date-pos)
      (my/write-property-drawer properties))
    (message "Trade property drawer created: %s (TYPE=%s DIRECTION=%s INIT=%s)"
             trade-id trade-type dir init-price)))

;; Trade Fill
(defun my/find-fill-section ()
  (let ((date-pos (my/find-current-trade-date-heading))
        fill-pos)
    (save-excursion
      (goto-char date-pos)
      ;; search forward for **** fill in the trade subtree
      (when (re-search-forward "^\\*\\*\\*\\* fill" nil t)
        (setq fill-pos (line-beginning-position))))
    (unless fill-pos
      (error "Could not find **** fill section in current trade"))
    fill-pos))

(defun my/update-fill-field (field-name value)
  (let ((fill-pos (my/find-fill-section))
        field-regex)
    (save-excursion
      (goto-char fill-pos)
      (setq field-regex (format "^\\- %s:\\(.*\\)$" (regexp-quote field-name)))
      (if (re-search-forward field-regex (my/goto-heading-end 4) t)
          (replace-match (format "- %s: %s" field-name value) t t)
        (error "Could not find field '%s' in fill section" field-name)))))

(defun my/prompt-stock-fill-data ()
  "Prompt user for stock fill details (quantity, price) and return as an alist.
Automatically includes the trade date."
  (let* ((date (my/extract-trade-date))
         (quantity (read-string "Quantity: "))
         (price (read-string "Price: ")))
    `((date . ,date)
      (quantity . ,quantity)
      (price . ,price))))

(defun my/prompt-options-fill-data ()
  "Prompt user for options fill details (strike, exp, quantity, price).
Automatically includes the trade date. Returns an alist."
  (let* ((date (my/extract-trade-date))
         (strike (read-string "Strike: "))
         (exp (read-string "Expiration: "))
         (quantity (read-string "Quantity: "))
         (price (read-string "Price: ")))
    `((date . ,date)
      (strike . ,strike)
      (exp . ,exp)
      (quantity . ,quantity)
      (price . ,price))))

(defun my/fill-stock-section (data)
  "Fill the **** fill section of the current trade with stock DATA.
DATA should be an alist from `my/prompt-stock-fill-data`."
  (my/update-fill-field "date" (alist-get 'date data))
  (my/update-fill-field "quantity" (alist-get 'quantity data))
  (my/update-fill-field "price" (alist-get 'price data)))

(defun my/fill-options-section (data)
  "Fill the **** fill section of the current trade with options DATA.
DATA should be an alist from `my/prompt-options-fill-data`."
  (my/update-fill-field "date" (alist-get 'date data))
  (my/update-fill-field "strike" (alist-get 'strike data))
  (my/update-fill-field "exp" (alist-get 'exp data))
  (my/update-fill-field "quantity" (alist-get 'quantity data))
  (my/update-fill-field "price" (alist-get 'price data)))

;; calculations.org population
(defun my/parse-key-value-line ()
  "Parse the current line of the form '- key: value' and return (key . value)."
  (when (looking-at "^[ \t]*- \\([^:]+\\):?\\(.*\\)")
    (let ((key (string-trim (match-string 1)))
          (val (string-trim (match-string 2))))
      (cons key val))))

(defun my/make-keyword-symbol (key &optional parent)
  "Convert KEY into a clean keyword symbol.  
If PARENT is provided, the final key is 'parent-key-child-key'.  
All spaces replaced with dashes and lowercase."
  (let ((clean-key (downcase (replace-regexp-in-string "[[:space:]]+" "-" key))))
    (if parent
        (intern (concat ":" (downcase (replace-regexp-in-string "[[:space:]]+" "-" parent))
                        "-" clean-key))
      (intern (concat ":" clean-key)))))

(defun my/parse-indented-children (parent-key parent-indent limit)
  "Parse lines indented more than PARENT-INDENT up to LIMIT as children of PARENT-KEY."
  (let ((children '()))
    (while (and (< (point) limit)
                (> (current-indentation) parent-indent)
                (looking-at "^[ \t]*- \\([^:]+\\):?\\(.*\\)"))
      (let* ((child (my/parse-key-value-line))
             (key (car child))
             (val (cdr child))
             (sym (my/make-keyword-symbol key parent-key)))
        (when val
          (push (cons sym val) children)))
      (forward-line 1))
    (nreverse children)))

(defun my/parse-indented-list-for-extractor (limit)
  "Parse '- key: value' lines up to LIMIT, including one-level children.  
Parent lines with empty values are skipped; children always get prefixed by parent."
  (let ((results '()))
    (while (and (< (point) limit)
                (looking-at "^[ \t]*- \\([^:]+\\):?\\(.*\\)"))
      (let* ((line (my/parse-key-value-line))
             (key (car line))
             (val (cdr line))
             (line-indent (current-indentation)))
        ;; Push parent if it has a value
        (when (and val (not (string-empty-p val)))
          (push (cons (my/make-keyword-symbol key) val) results))
        (forward-line 1)
        ;; Always append children lines with parent-key prefix
        (setq results
              (append results
                      (my/parse-indented-children key line-indent limit)))))
    (nreverse results)))

(defun my/clean-extracted-value (val)
  "Strip text properties from VAL and convert numeric-looking strings to numbers."
  (let ((plain (if (stringp val) (string-trim (substring-no-properties val)) val)))
    (if (and (stringp plain)
             (string-match-p "\\`[0-9.]+\\'" plain))
        (string-to-number plain)
      plain)))

(defun my/get-trade-properties (&optional heading-pos)
  "Return an alist of all properties from the Org property drawer at HEADING-POS.
If HEADING-POS is nil, use the current heading."
  (save-excursion
    (when heading-pos
      (goto-char heading-pos))
    ;; Ensure we are at the beginning of a heading
    (unless (org-at-heading-p)
      (org-back-to-heading t))
    ;; Get all properties from the drawer
    (let ((props (org-entry-properties nil 'all))
          (data '()))
      (dolist (p props)
        (let ((k (intern (concat ":" (replace-regexp-in-string "[[:space:]]+" "-"
                                                             (downcase (car p)))))))
          (push (cons k (cdr p)) data)))
      (nreverse data))))

(defun my/write-open-options (data &optional file-name)
  "Append DATA as a new row to the Open/Options table."
  (let* ((file (or file-name my-trading-calculations-file))
         (schema (my/get-table-schema "Open/Options" file))
         (row (my/build-open-options-row data schema)))
    (my/write-row-to-table row "Open/Options" file schema)))

(defun my/write-manage-options (data &optional file-name)
  "Append DATA as a new row to the Manage table."
  (let* ((file (or file-name my-trading-calculations-file))
         (schema (my/get-table-schema "Manage" file))
         (row (my/build-manage-options-row data schema)))
  (my/write-row-to-table row "Manage" file schema)))

(defun my/write-open-stocks (data &optional file-name)
  "Append DATA as a new row to the Open/Stocks table."
  (let* ((file (or file-name my-trading-calculations-file))
         (schema (my/get-table-schema "Open/Stocks" file))
         (row (my/build-open-stocks-row data schema)))
    (my/write-row-to-table row "Open/Stocks" file schema)))

;; trade close in trades.org
(defun my/move-trade-open-to-watch ()
  "Move the current trade's ticker from Watch section to Open section in trades.org."
  (let* ((date-pos (my/find-current-trade-date-heading))
         (ticker-pos (my/find-ticker-heading-position date-pos))
         (ticker-text (my/cut-entire-ticker-section ticker-pos)))
    (my/paste-ticker-in-section "Watch" ticker-text)
    (message "Trade moved from Open to Watch")))

(defun my/find-trade-close-subsection ()
  (let ((date-pos (my/find-current-trade-date-heading))
        close-pos)
    (save-excursion
      (goto-char date-pos)
      ;; limit search to the trade subtree (end of level-3 heading)
      (when (re-search-forward "^\\*\\*\\*\\*\\* +trade close\\>" (my/goto-heading-end 3) t)
        (setq close-pos (line-beginning-position))))
    (unless close-pos
      (error "Could not find ***** trade close subsection in current trade"))
    close-pos))

(defun my/update-trade-close-field (field-name value)
  (let ((close-pos (my/find-trade-close-subsection))
        field-regex)
    (save-excursion
      (goto-char close-pos)
      (setq field-regex (format "^\\-[ \t]*%s:?[ \t]*\\(.*\\)$"
                                 (regexp-quote field-name)))
      (if (re-search-forward field-regex (my/goto-heading-end 5) t)
          (replace-match (format "- %s: %s" field-name value) t t)
        (error "Could not find field '%s' in trade close section" field-name)))))

(defun my/prompt-stock-close-data ()
  (let* ((date (my/get-current-date))
         (quantity (read-string "Close quantity: "))
         (price (read-string "Close price: ")))
    `((date . ,date)
      (quantity . ,quantity)
      (price . ,price))))

(defun my/prompt-options-close-data ()
  (let* ((data (my/extract-trade-data-clean))
         (date (my/get-current-date))
         (quantity (read-string "Close quantity: "))
         (price (read-string "Close price: "))
         (exp (alist-get :exp data))
         (strike (alist-get :strike data)))
    `((date . ,date)
      (quantity . ,quantity)
      (price . ,price)
      (exp . ,exp)
      (strike . ,strike))))

(defun my/stock-close-section ()
  (let ((data (my/prompt-stock-close-data)))
    (my/update-trade-close-field "date" (alist-get 'date data))
    (my/update-trade-close-field "quantity" (alist-get 'quantity data))
    (my/update-trade-close-field "price" (alist-get 'price data))))

(defun my/options-close-section ()
  (let ((data (my/prompt-options-close-data)))
    (my/update-trade-close-field "date" (alist-get 'date data))
    (my/update-trade-close-field "quantity" (alist-get 'quantity data))
    (my/update-trade-close-field "price" (alist-get 'price data))
    (my/update-trade-close-field "exp" (alist-get 'exp data))
    (my/update-trade-close-field "strike" (alist-get 'strike data))))

(defun my/normalize-open-data (open-data)
  `((:open . ,(my/normalize-single-block open-data))))

(defun my/normalize-close-data (close-data)
  (let ((chunks '())
        (current '()))
    
    (dolist (item close-data)
      ;; if current is non-empty and we see a new :date, start a new chunk
      (when (and current (eq (car item) :date))
        (push (nreverse current) chunks)
        (setq current '()))
      (push item current))
    
    ;; push the last chunk
    (when current
      (push (nreverse current) chunks))
    
    (let* ((chunks (nreverse chunks))
           (final (nth 0 chunks))
           (tp1   (nth 1 chunks)))
      `((:final . ,(and final (list final)))
        (:tp1   . ,(and tp1 (list tp1)))))))

(defun my/normalize-section (sections section)
  (pcase section
    (:open  (my/normalize-open-data sections))
    (:close (my/normalize-close-data sections))
    (_ (error "Unknown section keyword: %S" section))))

(defun my/normalize-single-block (block)
  (when block
    (list block)))

(defun my/get-value-blocks (normalized section)
  (pcase section
    (:open
     (let* ((blk (alist-get :open normalized))
            (values (my/get-price-and-quantity normalized :open)))
       (list values)))

    (:close
     (let* ((final  (alist-get :final normalized))
            (tp1    (alist-get :tp1 normalized))
            (blocks '()))
        (when (and tp1 (listp tp1) (> (length tp1) 0))
         (push (my/get-price-and-quantity normalized :tp1) blocks))
        (when final
         (push (my/get-price-and-quantity normalized :final) blocks))
      (nreverse blocks)))
    (_ (error "Unknown section: %S" section))))

(defun my/get-value-block (normalized section which)
  (let ((blocks (my/get-value-blocks normalized section)))
    (pcase which
      (:open
       (car blocks))

      (:tp1
       (when (my/has-tp1-p normalized)
         (car blocks)))  

      (:final
       (if (my/has-tp1-p normalized)
           (nth 1 blocks)  
         (car blocks)))    

      (_ (error "Unknown block selector: %S" which)))))

;; calculate p&l
(defun my/calc-value (price quantity trade-type)
  (let* ((price (if (stringp price) (string-to-number price) price))
         (qty  (if (stringp quantity)   (string-to-number quantity)   quantity))
         (mult (if (string= trade-type "options") 100 1)))
    (* price qty mult)))

(defun my/has-tp1-p (close-sections)
  (let ((tp1 (alist-get :tp1 close-sections)))
    (and tp1 (listp tp1) (> (length tp1) 0))))

(defun my/get-price-and-quantity (close-sections section-key)
  (let* ((section (alist-get section-key close-sections))
         (block    (car section)))  
    (list :price    (alist-get :price block)
          :quantity (alist-get :quantity block))))

(defun my/format-money (number)
  (format "%.2f" number))

(defun my/read-money (prop)
  (string-to-number (my/read-trade-property-value prop)))

(defun my/get-open-block ()
  (let ((norm (my/normalize-section
               (my/extract-sections-data '("close" "lessons" "opening indicators"))
               :open)))
    (my/get-value-block norm :open :open)))

(defun my/get-final-block ()
  (let ((norm (my/normalize-section
               (my/extract-sections-data '("fill" "lessons" "opening indicators"))
               :close)))
    (my/get-value-block norm :close :final)))

(defun my/get-tp1-block ()
  (let ((norm (my/normalize-section
               (my/extract-sections-data '("fill" "lessons" "opening indicators"))
               :close)))
    (my/get-value-block norm :close :tp1)))

(defun my/calc-value-block (block trade-type)
  (if block
      (let ((price (plist-get block :price))
            (qty   (plist-get block :quantity)))
        (my/calc-value price qty trade-type))
    0))

(defun my/calc-trade-pnl ()
  (let ((open  (my/read-money my/prop-open-value))
        (tp1   (my/read-money my/prop-tp1-value))
        (close (my/read-money my/prop-final-value)))
    (- (+ tp1 close) open)))

(defun my/calc-updated-pnl (old-value new-value)
  (let* ((old-num (if (and old-value (stringp old-value) (not (string-empty-p old-value)))
                      (string-to-number old-value)
                    0)))
    (+ old-num new-value)))

;; move info to summary.org
(defun my/full-date-to-mmdd (date-str)
  (let* ((parts (split-string date-str "/"))
         (mm (nth 0 parts))
         (dd (nth 1 parts)))
    (format "%s/%s" mm dd)))

(defun my/get-date-range ()
  (let* ((open-date  (my/full-date-to-mmdd (my/get-open-date)))
         (close-date (my/full-date-to-mmdd (my/get-close-date))))
    (format "%s - %s" open-date close-date)))

(defun my/get-trade-ID ()
  (my/get-prop "TRADE_ID")
  )

(defun my/get-trade-type ()
  (my/get-prop "DIRECTION")
  )

(defun my/get-p&l ()
  (my/get-prop my/prop-pnl)
  )

(defun my/get-lessons ()
  (string-trim (my/get-section-text 4 "lessons")))

(defun my/build-trade-row ()
  (list
   (my/get-trade-ID)
   (my/get-trade-type)
   (my/get-date-range)
   (my/get-p&l)
   (my/get-lessons)))

(defun my/get-trade-metadata ()
  (save-excursion
    (let ((drawer (my/find-trade-property-drawer)))
      (unless drawer (error "No property drawer found for this trade"))
      (goto-char drawer)

      (let* ((trade-row  (my/build-trade-row))
             (date-str   (my/get-trade-date-string))
             (month-str  (my/get-month-from-trade-heading date-str))
             (year-str   (my/get-year-from-trade-heading date-str))
             (month-name (my/month-number-to-name month-str))
             (full-year  (my/expand-year year-str)))
        `((row        . ,trade-row)
          (date       . ,date-str)
          (year       . ,full-year)
          (month      . ,month-name)
          (raw-month  . ,month-str)
          (raw-year   . ,year-str))))))

(defun my/ensure-year-and-month (year month)
  (goto-char (point-min))
  (let ((year-point (my/find-org-heading 1 year)))
    (unless year-point
      (goto-char (point-max))
      (setq year-point (my/insert-heading-at-point 1 year)))
    (goto-char year-point)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (let ((month-point (my/find-org-heading 2 month nil subtree-end)))
        (unless month-point
          (goto-char subtree-end)
          (setq month-point (my/insert-heading-at-point 2 month)))
        (goto-char month-point)))))

(defun my/create-p&l-drawer (pos initial-pnl)
  (save-excursion
    (goto-char pos)
    (my/write-property-drawer `(("P&L" . ,(number-to-string initial-pnl))))))

(defun my/update-month-pnl (trade-pnl)
  (let* ((month-pos (point))
         (month-beg month-pos)
         (month-end (save-excursion
                      (goto-char month-beg)
                      (org-end-of-subtree t)))
         (drawer-start (my/find-property-drawer-in-region month-beg month-end))
         (drawer-end (my/get-property-drawer-end-in-region drawer-start month-end)))
    
    (if (and drawer-start drawer-end)
        (let* ((existing-pnl-str (my/get-property-value-in-region
                                  "P&L"
                                  drawer-start
                                  drawer-end))
               (existing-pnl (if existing-pnl-str
                                 (string-to-number existing-pnl-str)
                               0))
               (new-total (+ existing-pnl trade-pnl)))
          (my/set-property-value-in-region
           "P&L"
           (number-to-string new-total)
           drawer-start
           drawer-end))
      
      (my/create-p&l-drawer month-pos trade-pnl))))

(defun my/update-year-pnl (trade-pnl)
  (let* ((year-pos (point))
         (year-beg year-pos)
         (year-end (save-excursion
                     (goto-char year-beg)
                     (forward-line 4) 
                     (point)))
         (drawer-start (my/find-property-drawer-in-region year-beg year-end))
         (drawer-end   (my/get-property-drawer-end-in-region drawer-start year-end)))
    (if (and drawer-start drawer-end)
        (let* ((existing-pnl-str (my/get-property-value-in-region
                                  "P&L"
                                  drawer-start
                                  drawer-end))
               (existing-pnl (if existing-pnl-str
                                 (string-to-number existing-pnl-str)
                               0))
               (new-total (+ existing-pnl trade-pnl)))
          (my/set-property-value-in-region
           "P&L"
           (number-to-string new-total)
           drawer-start
           drawer-end))
      (my/create-p&l-drawer year-pos trade-pnl))))

(defun my/update-year-pnlAPI (trade-pnl)
  (save-excursion
    (org-back-to-heading t)
    (beginning-of-line)
    (let* ((existing-pnl-str (org-entry-get nil "P&L"))
           (existing-pnl (if existing-pnl-str
                             (string-to-number existing-pnl-str)
                           0))
           (new-total (+ existing-pnl trade-pnl)))
      (org-entry-put nil "P&L" (number-to-string new-total)))))

(defun my/get-table-insert-point ()
  (or (my/find-table-in-subtree)
      (my/find-drawer-end-in-subtree)
      (my/subtree-first-line)))

(defun my/insert-summary-table ()
  (insert "| id | type | range | p&l | lesson |\n")
  (insert "|----+------+-------+-----+--------|\n") 
  (org-table-align))

(defun my/insert-table-in-current-subtree ()
  (save-excursion
    (goto-char (my/get-table-insert-point))
    (my/insert-summary-table)))

(defun my/ensure-month-table-and-append-row (trade-row month-beg month-end)
  (save-excursion
    (goto-char month-beg)

    (let ((table-pos (my/find-table-in-subtree)))
      (unless table-pos
        (goto-char (my/get-table-insert-point))
        (my/insert-summary-table)
        (setq table-pos (point)))  

      (goto-char table-pos)
      (while (looking-at org-table-any-line-regexp)
        (forward-line 1))

      (insert (concat "| "
                      (mapconcat #'identity trade-row " | ")
                      " |\n"))
      (org-table-align))))

(defun my/update-summary-with-trade (trade-metadata)
  (let* ((trade-row  (alist-get 'row trade-metadata))
         (full-year  (alist-get 'year trade-metadata))
         (month-name (alist-get 'month trade-metadata))
         (pnl  (my/calc-trade-pnl)))
    
    (with-current-buffer (find-file-noselect my-trading-summary-file)      
      (my/ensure-year-and-month full-year month-name) 
      (let ((month-beg (point))
            (month-end (save-excursion
                         (goto-char (point))
                         (org-end-of-subtree t)))) 
        (my/update-month-pnl pnl) 
        (my/ensure-month-table-and-append-row trade-row month-beg month-end))
      (org-up-heading-safe)
      (my/update-year-pnl pnl))))

;; Main Interactive Functions 
(defun my/extract-trade-data-clean (&optional exclude-sections date-pos)
  (interactive)
  (let* ((start-pos (or date-pos (my/find-current-trade-date-heading)))
         (trade-end (save-excursion (goto-char start-pos)
                                    (my/goto-heading-end 3)))
         (exclude-sections (or exclude-sections '("close" "lessons")))
         (data '()))
    (save-excursion
      ;; 1) ticker
      (push (cons :ticker
                  (my/clean-extracted-value (my/extract-ticker-data)))
            data)

      ;; 2) date
      (push (cons :date
                  (my/clean-extracted-value (my/extract-trade-date)))
            data)

      ;; 3) properties 
      (setq data (append (my/extract-properties-data) data)))

    ;; 4) sections 
    (setq data (append (my/extract-sections-data exclude-sections start-pos trade-end)
                       data))

    (setq data (nreverse data))
    (when (called-interactively-p 'any)
      (message "%S" data))
    data))

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
           (pba (cdr (assoc 'pba data)))
	   (risk (cdr (assoc 'risk data))))
      
      (when (y-or-n-p (format "Create %s trade template for %s? " trade-type ticker))
        (let ((trade-text (my/generate-trade-template trade-type risk kijun pba atr delta)))
          (my/insert-trade-template-in-trades-file ticker trade-text)
          (message "%s trade template created for %s" trade-type ticker))))))

(defun my/orchestrate-trade-open () 
  ;; Step 1: Prompt for INIT and create the property drawer
  (interactive)
  (let* ((trade-data (my/extract-trade-data))
         (date-pos (cdr (assoc 'date-pos trade-data)))
         (type-line-pos (cdr (assoc 'type-pos trade-data)))
	 (risk-line-pos (cdr (assoc 'risk-pos trade-data)))
         (ticker (cdr (assoc 'ticker trade-data)))
         (type-value (cdr (assoc 'type-value trade-data))) 
	 (risk-value (cdr (assoc 'risk-value trade-data)))
         (direction (and type-value (downcase (string-trim type-value))))
	 (trade-type (if (member direction '("call" "put")) "options" "stock"))
         (init-price (read-string "Enter initial price of underlying (INIT): "))
         (timestamp (format-time-string "%y%m%d"))
         (trade-id (format "%s-%s" ticker timestamp)))
  (my/trade-property-drawer date-pos type-line-pos risk-line-pos trade-id direction init-price trade-type risk-value)
  
  ;; Step 2: Enter trade fill information
  (cond
    ((string= trade-type "stock")
     (my/fill-stock-section (my/prompt-stock-fill-data)))
    ((string= trade-type "options")
     (my/fill-options-section (my/prompt-options-fill-data))))
 
  ;; Step 3: Write to calculate.org
  (let ((final-data (my/extract-trade-data-clean)))
    (cond
      ((string= trade-type "stock")
        (my/write-open-stocks final-data))
      ((string= trade-type "options")
        (my/write-open-options final-data)
        (my/write-manage-options final-data))))
  
  ;; Step 4: Move trade from Watch → Open
  (my/move-trade-watch-to-open)))

(defun my/orchestrate-trade-close ()
  (interactive)
  (save-excursion
    ;; Step 1: Fill trade close subsection
    (let* ((trade-data (my/extract-trade-data-clean))
           (type (alist-get :type trade-data nil nil #'string=))
           (trade-id (alist-get :trade_id trade-data)))

      (cond
       ((string= type "stock")
        (my/stock-close-section))
       ((string= type "options")
        (my/options-close-section)))

      ;; Step 2: Update property drawer in trades.org
      (my/close-trade-property-drawer)

      (my/add-new-property
       my/prop-open-value
       (my/format-money (my/calc-value-block (my/get-open-block) type)))

      (my/add-new-property
       my/prop-tp1-value
       (my/format-money (my/calc-value-block (my/get-tp1-block) type)))

      (my/add-new-property
       my/prop-final-value
       (my/format-money (my/calc-value-block (my/get-final-block) type)))

      (my/add-new-property
       my/prop-pnl
       (my/format-money (my/calc-trade-pnl)))
	
      ;; Step 3: extract trade data to pass to summary.org
      (setq trade-metadata (my/get-trade-metadata))

      ;; Step 4: Move trade from Open → Watch in trades.org
      (my/move-trade-open-to-watch)

      ;; Step 5: Update summary.org month/year
      (my/update-summary-with-trade trade-metadata)
      
      ;; Step 6: Delete corresponding rows from calculate.org tables
      (cond
         ((string= type "stock")
          (my/delete-trade-from-calculate-table
           my-trading-calculations-file "Open/Stocks" trade-id))
         ((string= type "options")
          (my/delete-trade-from-calculate-table
           my-trading-calculations-file "Open/Options" trade-id)
          (my/delete-trade-from-calculate-table
           my-trading-calculations-file "Manage" trade-id)))
        )))

(provide 'my-trading-workflow)
