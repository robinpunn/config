;;; Trading Journal

;; Custom Variables
(defgroup my-trading nil
  "Trading journal workflow customization."
  :group 'convenience)

(defcustom my-trading-trades-file "~/Documents/Practice/trades.org"
  "Path to the trades.org file."
  :type 'file
  :group 'my-trading)

(defcustom my-trading-calculations-file "~/Documents/Practice/calculate.org"
  "Path to the calculate.org file."
  :type 'file
  :group 'my-trading)

(defcustom my-trading-summary-file "~/Documents/Practice/summary.org"
  "Path to the summary.org file."
  :type 'file
  :group 'my-trading)

;; Generic Utility Functions
(defun my/find-org-heading (level text &optional direction limit)
  "Find an org heading of LEVEL with TEXT content.
LEVEL should be 1 for *, 2 for **, 3 for ***, etc.
DIRECTION can be 'backward or 'forward (default forward).
LIMIT is the search boundary (nil for no limit).
Returns the position of the heading or nil if not found."
  (let* ((stars (format "\\*\\{%d\\}" level))           ; exact count of stars
         (name-part (if text (regexp-quote text) "\\(.*\\)"))
         (pattern (format "^%s %s" stars name-part))
         (search-func (if (eq direction 'backward) 're-search-backward 're-search-forward)))
    (save-excursion
      (when (funcall search-func pattern limit t)
        (line-beginning-position)))))

(defun my/get-org-heading-content (level &optional direction)
  "Get the content text of an org heading at LEVEL.
DIRECTION can be 'backward or 'forward (default backward for context)."
  (let* ((stars (format "\\*\\{%d\\}" level))
         (pattern (format "^%s \\(.*\\)$" stars))
         (search-func (if (eq direction 'forward) 're-search-forward 're-search-backward)))
    (save-excursion
      (when (funcall search-func pattern nil t)
        (string-trim (match-string 1))))))

(defmacro my/with-trading-file (file-var &rest body)
  "Execute BODY with FILE-VAR buffer current, saving afterwards."
  `(with-current-buffer (find-file-noselect ,file-var)
     (prog1 (progn ,@body)
       (save-buffer))))

(defun my/goto-heading-end (level)
  "Move point to the end of current heading section at LEVEL.
Returns the end position."
  (save-excursion
    (forward-line 1)
    (let ((next-heading-pattern (format "^\\*\\{1,%d\\} " level)))
      (if (re-search-forward next-heading-pattern nil t)
          (line-beginning-position)
        (point-max)))))

(defun my/find-buffer-for-filename (file-name)
  "Return the buffer visiting `file-name`.
If it isn't already open, open it."
  (or (find-buffer-visiting file-name)
      (find-file-noselect file-name)))

(defun my/goto-heading (heading-path file-name)
  "Navigate to a heading specified by HEADING-PATH in calculate.org.
HEADING-PATH is a string like \"Open/Options\". Signals an error if not found."
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

;; find Table 
(defun my/find-table-in-section (heading-path file-name)
  "Return buffer position of the first Org table inside the section HEADING-PATH in FILE-NAME.
Signals an error if no table is found."
  (let ((buf (my/find-buffer-for-filename file-name)))
    (with-current-buffer buf
      ;; Go to the heading
      (my/goto-heading heading-path file-name)
      ;; Search for the table in this subtree
      (let ((end (save-excursion (org-end-of-subtree t))))
        (unless (re-search-forward org-table-dataline-regexp end t)
          (error "No table found in section: %s" heading-path))
        (beginning-of-line)
        (point)))))

(defun my/get-table-header ()
  "Return the header row of the current org table as a list of strings."
  (let ((table (org-table-to-lisp)))
    (car table)))

(defun my/normalize-table-header (header-row)
  "Convert HEADER-ROW (a list of strings) into a list of keywords.
Downcases, trims, and replaces non-alphanumerics with underscores."
  (mapcar (lambda (col)
            (let* ((name (downcase (string-trim col)))
                   (name (replace-regexp-in-string "[^a-z0-9]+" "_" name)))
              (intern (concat ":" name))))
          header-row))

(defun my/get-table-schema (heading-path file-name)
  "Return a schema (list of keywords) from the header row of the
table under HEADING-PATH in FILE-NAME."
  (let ((buf (my/find-buffer-for-filename file-name)))
    (with-current-buffer buf
      (goto-char (my/find-table-in-section heading-path file-name))
      (my/normalize-table-header (my/get-table-header)))))

(defun my/get-table-end-position (section file)
  "Return the buffer position at the end of the first table in SECTION of FILE.
The returned position is the first line **after** the table."
  (save-excursion
    (my/with-trading-file file
      (my/goto-heading section file)
      (when (re-search-forward org-table-any-line-regexp nil t)
        (beginning-of-line)
        ;; Walk forward until we're past the last table line
        (while (looking-at org-table-any-line-regexp)
          (forward-line 1))
        ;; Return position just after table
        (point)))))

;; Write to table 
(defun my/build-open-options-row (data schema)
  "Build a row for Open/Options table from DATA, respecting SCHEMA.
Fills id, type, init, atr, risk, delta, premium. Leaves other fields blank."
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

;; Property Drawer Utilities
(defun my/property-drawer-exists-p ()
  "Check if a property drawer exists at current heading."
  (save-excursion
    (forward-line 1)
    (looking-at "^:PROPERTIES:")))

(defun my/read-property-value (property)
  "Read the value of PROPERTY from current heading's property drawer."
  "Need to be at heading for this to work"
  (save-excursion
    (when (re-search-forward (format "^:%s: +\\(.*\\)$" (upcase property)) nil t)
      (string-trim (match-string 1)))))

(defun my/write-property-drawer (properties)
  "Write a property drawer with PROPERTIES alist at current heading.
PROPERTIES should be an alist of (property . value) pairs."
  (save-excursion
    (end-of-line)
    (insert "\n:PROPERTIES:")
    (dolist (prop properties)
      (insert (format "\n:%s: %s" (upcase (car prop)) (cdr prop))))
    (insert "\n:END:")))

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
                              (line-beginning-position)
                            (point-max))))
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

(defun my/create-property-drawer-at-date (date-pos trade-id type-value direction)
  "Create a property drawer after the date heading at the given position."
  (let* ((type-info (my/parse-trade-type type-value))
         (properties `(("TRADE_ID" . ,trade-id)
                      ("TYPE" . ,(car type-info))
                      ("DIRECTION" . ,direction)
                      ("STATUS" . "open"))))
    (save-excursion
      (goto-char date-pos)
      (my/write-property-drawer properties))))

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

;; Calculations Data Extraction 
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
  "Move the current trade's ticker from Watch section to Open section in trades.org."
  (let* ((date-pos (my/find-current-trade-date-heading))
         (ticker-pos (my/find-ticker-heading-position date-pos))
         (ticker-text (my/cut-entire-ticker-section ticker-pos)))
    (my/paste-ticker-in-section "Open" ticker-text)
    (message "Trade moved from Watch to Open")))

(defun trade-property-drawer (date-pos type-line-pos risk-line-pos trade-id direction init-price trade-type risk)
  "Write a property drawer at DATE-POS with TRADE-ID, DIRECTION and INIT-PRICE.
TYPE is derived from DIRECTION: call/put -> \"options\", otherwise -> \"stock\".
TYPE-LINE-POS (if non-nil) is removed before writing the drawer."
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
(defun my/find-current-trade-date-heading ()
  "Return the position of the current trade's *** date heading.
If point is already on a *** date heading, returns that position.
Otherwise searches backward for the nearest *** heading."
  (or (when (looking-at "^\\*\\*\\* ")
        (point))
      (let ((pos (save-excursion
                   (re-search-backward "^\\*\\*\\* " nil t))))
        (if pos
            pos
          (error "Not inside a trade entry (no *** date heading found)")))))

(defun my/find-fill-section ()
  "Return the position of the **** fill heading in the current trade.
Assumes point is anywhere inside the trade.
Errors if no **** fill section is found."
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

(defun my/extract-trade-date ()
  "Return the date string from the current trade's *** date heading.
Assumes point is anywhere inside the trade."
  (save-excursion
    (goto-char (my/find-current-trade-date-heading))
    (let ((line (thing-at-point 'line t)))
      ;; Extract everything after the stars and space
      (if (string-match "^\\*\\*\\* \\(.*\\)$" line)
          (string-trim (match-string 1 line))
        (error "Could not extract date from heading")))))

(defun my/update-fill-field (field-name value)
  "Update FIELD-NAME in the **** fill section of the current trade with VALUE.
Assumes point is anywhere inside the trade.
FIELD-NAME should be the string without the leading dash, e.g., \"date\" or \"price\"."
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

;; Main Interactive Functions 
(defun my/extract-trade-data-clean (&optional date-pos)
  "Extract all trade data for the trade rooted at DATE-POS (*** date heading).
Returns a flat alist of keyword symbols to cleaned values (numbers or strings).
Skips 'close' and 'lessons' sections."
  (interactive)
  (let* ((start-pos (or date-pos (my/find-current-trade-date-heading)))
         (trade-end (save-excursion (goto-char start-pos) (my/goto-heading-end 3)))
         (data '()))
    (save-excursion
      ;; 1) ticker
      (let ((ticker-pos (my/find-ticker-heading-position start-pos)))
        (when ticker-pos
          (save-excursion
            (goto-char ticker-pos)
            (when (looking-at "^\\*\\* \\(.*\\)$")
              (push (cons :ticker (my/clean-extracted-value (match-string 1))) data)))))

      ;; 2) date
      (push (cons :date (my/clean-extracted-value (my/extract-trade-date))) data)

      ;; 3) properties
      (goto-char start-pos)
      (dolist (p (org-entry-properties nil 'standard))
        (let ((k (intern (concat ":" (replace-regexp-in-string "[[:space:]]+" "-"
                                                             (downcase (car p)))))))
          (push (cons k (my/clean-extracted-value (cdr p))) data))))

      ;; 4) sections
      (goto-char start-pos)
      (while (re-search-forward "^\\*\\*\\*\\* \\(.*\\)$" trade-end t)
        (let* ((section (downcase (string-trim (match-string 1))))
               (sec-beg (match-beginning 0))
               (sec-end (save-excursion (goto-char sec-beg) (my/goto-heading-end 4))))
          (unless (member section '("close" "lessons"))
            (goto-char sec-beg)
            (while (re-search-forward "^[ \t]*- " sec-end t)
              (beginning-of-line)
              (setq data
                    (append (mapcar (lambda (cons)
                                      (cons (car cons)
                                            (my/clean-extracted-value (cdr cons))))
                                    (my/parse-indented-list-for-extractor sec-end))
                            data))))))
    ;; Return results
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

(defun my/orchestrate-trade ()
  "Orchestrator (open trade): prompt for INIT, generate trade-id, and write the property drawer.
Call this while inside the trade's *** date heading."
  "Step 1: Prompt for INIT and create the property drawer."
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
  (trade-property-drawer date-pos type-line-pos risk-line-pos trade-id direction init-price trade-type risk-value)
  
  "Step 2: Enter trade fill information."
  (cond
    ((string= trade-type "stock")
     (my/fill-stock-section (my/prompt-stock-fill-data)))
    ((string= trade-type "options")
     (my/fill-options-section (my/prompt-options-fill-data))))

  "Step 3: Move trade from Watch → Open."
  (my/move-trade-watch-to-open)))

(provide 'my-trading-workflow)
