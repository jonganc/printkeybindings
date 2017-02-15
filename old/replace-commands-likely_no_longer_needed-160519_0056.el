(defun query-replace-regexp (regexp to-string &optional delimited start end backward)
  ""
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query replace"
		   (if current-prefix-arg
		       (if (eq current-prefix-arg '-) " backward" " word")
		     "")
		   " regexp"
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end))
	   (nth 3 common))))
  (perform-replace regexp to-string t t delimited nil nil start end backward))



(defun query-replace-read-args (prompt regexp-flag &optional noerror)
  (unless noerror
    (barf-if-buffer-read-only))
  (let* ((from (query-replace-read-from prompt regexp-flag))
	 (to (if (consp from) (prog1 (cdr from) (setq from (car from)))
	       (query-replace-read-to from prompt regexp-flag))))
    (list from to
	  (and current-prefix-arg (not (eq current-prefix-arg '-)))
	  (and current-prefix-arg (eq current-prefix-arg '-)))))

(defun query-replace-read-from (prompt regexp-flag)
  "Query and return the `from' argument of a query-replace operation.
The return value can also be a pair (FROM . TO) indicating that the user
wants to replace FROM with TO."
  (if query-replace-interactive
      (car (if regexp-flag regexp-search-ring search-ring))
    (let* ((history-add-new-input nil)
	   (prompt
	    (if query-replace-defaults
		(format "%s (default %s -> %s): " prompt
			(query-replace-descr (car query-replace-defaults))
			(query-replace-descr (cdr query-replace-defaults)))
	      (format "%s: " prompt)))
	   (from
	    ;; The save-excursion here is in case the user marks and copies
	    ;; a region in order to specify the minibuffer input.
	    ;; That should not clobber the region for the query-replace itself.
	    (save-excursion
	      (if regexp-flag
		  (read-regexp prompt nil query-replace-from-history-variable)
		(read-from-minibuffer
		 prompt nil nil nil query-replace-from-history-variable
		 (car (if regexp-flag regexp-search-ring search-ring)) t)))))
      (if (and (zerop (length from)) query-replace-defaults)
	  (cons (car query-replace-defaults)
		(query-replace-compile-replacement
		 (cdr query-replace-defaults) regexp-flag))
	(add-to-history query-replace-from-history-variable from nil t)
	;; Warn if user types \n or \t, but don't reject the input.
	(and regexp-flag
	     (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
	     (let ((match (match-string 3 from)))
	       (cond
		((string= match "\\n")
		 (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
		((string= match "\\t")
		 (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
	       (sit-for 2)))
	from))))

(defun query-replace-descr (string)
  (mapconcat 'isearch-text-char-description string ""))

(defun query-replace-read-to (from prompt regexp-flag)
  "Query and return the `to' argument of a query-replace operation."
  (query-replace-compile-replacement
   (save-excursion
     (let* ((history-add-new-input nil)
	    (to (read-from-minibuffer
		 (format "%s %s with: " prompt (query-replace-descr from))
		 nil nil nil
		 query-replace-to-history-variable from t)))
       (add-to-history query-replace-to-history-variable to nil t)
       (setq query-replace-defaults (cons from to))
       to))
   regexp-flag))

(defun query-replace-compile-replacement (to regexp-flag)
  "Maybe convert a regexp replacement TO to Lisp.
Returns a list suitable for `perform-replace' if necessary,
the original string if not."
  (if (and regexp-flag
	   (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to))
      (let (pos list char)
	(while
	    (progn
	      (setq pos (match-end 0))
	      (push (substring to 0 (- pos 2)) list)
	      (setq char (aref to (1- pos))
		    to (substring to pos))
	      (cond ((eq char ?\#)
		     (push '(number-to-string replace-count) list))
		    ((eq char ?\,)
		     (setq pos (read-from-string to))
		     (push `(replace-quote ,(car pos)) list)
		     (let ((end
			    ;; Swallow a space after a symbol
			    ;; if there is a space.
			    (if (and (or (symbolp (car pos))
					 ;; Swallow a space after 'foo
					 ;; but not after (quote foo).
					 (and (eq (car-safe (car pos)) 'quote)
					      (not (= ?\( (aref to 0)))))
				     (eq (string-match " " to (cdr pos))
					 (cdr pos)))
				(1+ (cdr pos))
			      (cdr pos))))
		       (setq to (substring to end)))))
	      (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\[,#]" to)))
	(setq to (nreverse (delete "" (cons to list))))
	(replace-match-string-symbols to)
	(cons 'replace-eval-replacement
	      (if (cdr to)
		  (cons 'concat to)
		(car to))))
    to))

(defun read-regexp (prompt &optional defaults history)
  "Read and return a regular expression as a string.
Prompt with the string PROMPT.  If PROMPT ends in \":\" (followed by
optional whitespace), use it as-is.  Otherwise, add \": \" to the end,
possibly preceded by the default result (see below).

The optional argument DEFAULTS can be either: nil, a string, a list
of strings, or a symbol.  We use DEFAULTS to construct the default
return value in case of empty input.

If DEFAULTS is a string, we use it as-is.

If DEFAULTS is a list of strings, the first element is the
default return value, but all the elements are accessible
using the history command \\<minibuffer-local-map>\\[next-history-element].

If DEFAULTS is a non-nil symbol, then if `read-regexp-defaults-function'
is non-nil, we use that in place of DEFAULTS in the following:
  If DEFAULTS is the symbol `regexp-history-last', we use the first
  element of HISTORY (if specified) or `regexp-history'.
  If DEFAULTS is a function, we call it with no arguments and use
  what it returns, which should be either nil, a string, or a list of strings.

We append the standard values from `read-regexp-suggestions' to DEFAULTS
before using it.

If the first element of DEFAULTS is non-nil (and if PROMPT does not end
in \":\", followed by optional whitespace), we add it to the prompt.

The optional argument HISTORY is a symbol to use for the history list.
If nil, uses `regexp-history'."
  (let* ((defaults
	   (if (and defaults (symbolp defaults))
	       (cond
		((eq (or read-regexp-defaults-function defaults)
		     'regexp-history-last)
		 (car (symbol-value (or history 'regexp-history))))
		((functionp (or read-regexp-defaults-function defaults))
		 (funcall (or read-regexp-defaults-function defaults))))
	     defaults))
	 (default     (if (consp defaults) (car defaults) defaults))
	 (suggestions (if (listp defaults) defaults (list defaults)))
	 (suggestions (append suggestions (read-regexp-suggestions)))
	 (suggestions (delete-dups (delq nil (delete "" suggestions))))
	 ;; Do not automatically add default to the history for empty input.
	 (history-add-new-input nil)
	 (input (read-from-minibuffer
		 (cond ((string-match-p ":[ \t]*\\'" prompt)
			prompt)
		       ((and default (> (length default) 0))
			 (format "%s (default %s): " prompt
				 (query-replace-descr default)))
		       (t
			(format "%s: " prompt)))
		 nil nil nil (or history 'regexp-history) suggestions t)))
    (if (equal input "")
	;; Return the default value when the user enters empty input.
	(prog1 (or default input)
	  (when default
	    (add-to-history (or history 'regexp-history) default)))
      ;; Otherwise, add non-empty input to the history and return input.
      (prog1 input
	(add-to-history (or history 'regexp-history) input)))))

(defun replace-eval-replacement (expression count)
  (let* ((replace-count count)
         err
         (replacement
          (condition-case err
              (eval expression)
            (error
             (error "Error evaluating replacement expression: %S" err)))))
    (if (stringp replacement)
        replacement
      (prin1-to-string replacement t))))

(defun replace-quote (replacement)
  "Quote a replacement string.
This just doubles all backslashes in REPLACEMENT and
returns the resulting string.  If REPLACEMENT is not
a string, it is first passed through `prin1-to-string'
with the `noescape' argument set.

`match-data' is preserved across the call."
  (save-match-data
    (replace-regexp-in-string "\\\\" "\\\\"
			      (if (stringp replacement)
				  replacement
				(prin1-to-string replacement t))
			      t t)))

(replace-quote "\\\"")


