;; ESCAPE-HTML

(in-pr (escape-html "<<>>&&"))
"&lt;&lt;&gt;&gt;&amp;&amp;"

;; PKB-HTML-SECT-HEAD

(defun tst-me ()
  (let ((list-to-test
	 '("abc" ((outp-str . "ghj")) ((outp-str . "ghj") (other . t)) )))
    (insert
     "("
     (mapconcat
      (lambda (to-test)
	(concat 
	 "("
	 (prin1-to-string to-test)
	 " -> "
	 (prin1-to-string (pkb-html-sect-head to-test))
	 ")"))
      list-to-test
      "\n ")
     ")"))
)

(tst-me)


;; PKB-HTML-INS-FULL

(defun tst-me()
  (let ((remaining-bks-w-binds
	 '((55 nil (nil bind09) ((meta) bind07))
	   (next nil (nil bind14))
	   (57 nil (nil bind15) ((meta) bind08))
	   (91 nil (nil bind10)
	       ((control meta) bind01)
	       ((control)
		#1=(keymap (57 . bind08) (55 . bind07) (27 . bind01))))
	   (97 nil ((control) bind03) (nil bind02))
	   (102 nil (nil bind05))
	   (mouse-1 nil ((control click) bind12) ((down) bind13))
	   (mouse-2 nil ((control click) bind06) ((down) bind11))
	   (113 nil ((control) bind04))
	   (escape nil (nil #1#) ((meta) bind01))
	   ((49 . 54) nil (nil bind09))))
	(group1
	 '(full-gr
	   ("Group 1" opt1 opt2)
	   ;; BLOCKS-PRELIM
	   ;; ONE-BLOCK-PRELIM
	   ((nil "abc"
		 (nil nil . "bind1")
		 ((control meta) nil (outp-str . "bind2")))
	    (nil "qrd"
		 ((control) nil . "bind3")
		 (nil nil (outp-str . "bind4"))))
	   ;; ONE-BLOCK-PRELIM
	   ((nil "t"
		 (nil nil . "bind5")))))
	(pk-options
	 '((outp-str . "abc") (anchor-name . "ght"))))
    (pkb-html-int-pr-bks remaining-bks-w-binds nil nil nil nil nil)
    (pkb-html-ins-full (list remaining-bks-w-binds group1) pk-options 3)
    )
)

(tst-me)

;; PKB-HTML-INS-COMPACT

(let ((bks-w-binds 
       '((55 nil (nil bind09) ((meta) bind07))
	 (next nil (nil bind14))
	 (57 nil (nil bind15) ((meta) bind08))
	 (91 nil (nil bind10)
	     ((control meta) bind01)
	     ((control)
	      #1=(keymap (57 . bind08) (55 . bind07) (27 . bind01))))
	 (97 nil ((control) bind03) (nil bind02))
	 (102 nil (nil bind05))
	 (mouse-1 nil ((control click) bind12) ((down) bind13))
	 (mouse-2 nil ((control click) bind06) ((down) bind11))
	 (113 nil ((control) bind04))
	 (escape nil (nil #1#) ((meta) bind01))
	 ((49 . 54) nil (nil bind09)))))
  (pkb-html-int-pr-bks bks-w-binds nil nil nil nil nil)
  (pkb-html-ins-compact bks-w-binds "Compact test" 6)
)


;; PKB-HTML-INS-SIMPLE

(pkb-html-ins-simple
 '( (nil "1" nil . "bind1") (nil "2" nil . "bind2") (nil "3" nil . "bind3")
    (nil "21" nil . "2bind1") (nil "22" nil . "2bind2")
    (nil "23" nil . "2bind3")
    (nil "a" nil . "b1") (nil "fj" nil . "ty") (nil "fjas" nil . "(sdf"))
 "Simple"
 2
)

;; PKB-HTML-PR-BK-TABLE

(insert
 (pkb-html-pr-bk-table
 '(
    ;; ONE-BLOCK-FOR-OUTPUT-1-1
    ((bks (nil . "a") (nil . "b") (nil . "t"))
     (nil (nil . "bind1") (nil . "bind2") (nil . "bind3"))
     ((control meta) (nil . "bindcm1") (nil . "bindcm2") (nil . "bindcm3")))
    ;; ONE-BLOCK-FOR-OUTPUT-1-2
    ((bks (nil . "g") (nil . "$") (nil . "&amp;"))
     (nil (nil . "bind4") (nil . "bind5") (nil . "bind6"))
     ((meta) (nil . "bindm1") (nil . "bindm2") (nil . "bindm3")) ))))

;; PKB-HTML-PR-PAIRS-COLUMNS

(insert
 (pkb-html-pr-pairs-columns
  '((nil "1" nil . "bind1") (nil "2" nil . "bind2") (nil "3" nil . "bind3")
    (nil "21" nil . "2bind1") (nil "22" nil . "2bind2")
    (nil "23" nil . "2bind3") (nil "a" nil . "b1") (nil "fj" nil . "ty")
    (nil "fjas" nil . "(sdf"))
  3))

;; PKB-HTML-SAVE-KEYMAP

(let (km)
  (dolist (minor-mode '(iswitchb-mode visual-line-mode))
    (setq km (append km (cdr (assq minor-mode minor-mode-map-alist)))))
  (setq km (append km (current-global-map)))
  (pkb-html-save-keymap
   km
   (concat keyboard-proj-dir "testing/a.html")
   "Global map and some minor-maps" nil '([f1]) pkb-key-groups nil)
)

(let ((km (append overriding-terminal-local-map
		  overriding-local-map
		  (get-char-code-property (point) 'keymap)
		  emulation-mode-map-alists)))
  (dolist (minor-mode '(TeX-fold-mode reftex-mode LaTeX-math-mode))
    (setq km (append km (cdr (assq minor-mode minor-mode-map-alist)))))
  (setq km (append km
		   (get-char-code-property (point) 'local-map)
		   (current-local-map)))
  (pkb-html-save-keymap
   km
   (concat keyboard-proj-dir "testing/b.html")
   "Tex mode maps" nil nil pkb-key-groups nil)
)


(let ((km (append overriding-terminal-local-map
		  overriding-local-map
		  (get-char-code-property (point) 'keymap)
		  emulation-mode-map-alists)))
  (dolist (minor-mode '(TeX-fold-mode reftex-mode LaTeX-math-mode))
    (setq km (append km (cdr (assq minor-mode minor-mode-map-alist)))))
  (dolist (minor-mode '(iswitchb-mode visual-line-mode))
    (setq km (append km (cdr (assq minor-mode minor-mode-map-alist)))))
  (setq km (append km
		   (get-char-code-property (point) 'local-map)
		   (current-local-map)))
  (setq km (append km (current-global-map)))
  (pkb-html-save-keymap
   km
   (concat keyboard-proj-dir "testing/c.html")
   "Global + Tex mode maps" nil '([f1]) pkb-key-groups)
)

(in-pr
 (symbol-function '2C-command))
(keymap (115 . 2C-split) (98 . 2C-associate-buffer) (f2 . 2C-two-columns) (50 . 2C-two-columns))

;; PKB-HTML-INT-UPDATE-EVENT-DESC

(defun tst-me ()
  (let (matched-replace
	(event-replace-bk '((a "rep-a" ("a!" . "to-rep-a") t)
			    (mouse-2 "M2")
			    (q "rep-q" ("q?" . "rq") t)
			    (g "rep-g")
			    (r "rep-r" nil t)
			    (97 "super-a" ("s97" . "super") t)
			    (prior "pri")))
	(event-replace-regexp '((".-a" "2" (".-a" . "2s"))
				("123" "454")
				("\\(.\\)-g" "\\1" nil t)
				("rep-q" "new-rep-q" ("rep-qs" "new-q") t)))
	(list-to-test
	 '(?a ?b ?\C-w ?\e (?g . ?l) prior next right C-mouse-2 a
	      C-a)))
    (insert "((")
    (pkb-dolist-cons (elem-cons list-to-test)
      (insert
       (concat 
	(unless (eq elem-cons list-to-test)
	  "  ")
	"(" (single-key-description (car elem-cons)) " -> "
	(prin1-to-string
	 (pkb-html-int-update-event-desc
	  (car elem-cons) nil `matched-replace
	  event-replace-bk event-replace-regexp))
	")"
	(when (cdr elem-cons)
	  "\n"))))
    (insert ")\n(" (prin1-to-string matched-replace) "))"))
)

(tst-me)

;; PKB-HTML-INT-UPDATE-BIND-DESC

(defun tst-me ()
  (let (matched-replace
	(bind-replace-obj '((a "rep-a" ("a!" . "to-rep-a"))
			     (q "rep-q" ("q?" . "rq") t)
			     (g "rep-g")
			     (r "rep-r" nil t)
			     (97 "super-a" ("s97" . "super") t)
			     (prior "pri")))
	(bind-replace-regexp '((".-a" "2" (".-a" . "2s"))
				("123" "454")
				("\\(.\\)-g" "\\1" nil t)
				("rep-q" "new-rep-q" ("rep-qs" "new-q") t)))
	(list-to-test
	 '(a b c d e f (keymap (?a . 4) (?b . 5)))))

    (insert "((")
    (pkb-dolist-cons (elem-cons list-to-test)
      (insert
       (concat 
	(unless (eq elem-cons list-to-test)
	  "  ")
	"(" (single-key-description (car elem-cons)) " -> "
	(prin1-to-string
	 (pkb-html-int-update-bind-desc
	  (car elem-cons) '((nuthin . "abc") (replace-fl t) ) `matched-replace
	  bind-replace-obj bind-replace-regexp))
	")"
	(when (cdr elem-cons)
	  "\n"))))
    (insert ")\n(" (prin1-to-string matched-replace) "))"))
)

(tst-me)


;; PKB-HTML-INT-PR-BKS

(let (matched-replace
      (event-replace-bk
       '((55  "seven" ("7" . "sevensies"))
	 (mouse-1 "MOUS1" ("mouse-1" "mouseie") t)))
      (bind-replace-obj
       '((bind07 "bindthis")
	 (bind08 "ohnoes" ("bind008" "0HNO"))))
      (replace-regexp '((".-a" "2" (".-a" . "2s"))
			("123" "454")
			("\\(.\\)-g" "\\1" nil t)
			("rep-q" "new-rep-q" ("rep-qs" "new-q") t)))
      (bks-w-binds
       '((55 nil (nil bind09) ((meta) bind07))
	 (next nil (nil bind09))
	 (57 nil (nil bind09) ((meta) bind08))
	 (91 nil (nil bind10)
	     ((control meta) bind01)
	     ((control)
	      #1=(keymap (57 . bind08) (55 . bind07) (27 . bind01))))
	 (97 nil ((control) bind03) (nil bind02))
	 (102 nil (nil bind05))
	 (mouse-1 nil ((control click) bind12) ((down) bind13))
	 (mouse-2 nil ((control click) bind06) ((down) bind11))
	 (113 nil ((control) bind04))
	 (escape nil (nil #1#) ((meta) bind01))
	 ((49 . 54) nil (nil bind09)))))
  (in-pr
   (pkb-html-int-pr-bks
    bks-w-binds 'matched-replace
    event-replace-bk replace-regexp bind-replace-obj replace-regexp))
  (insert "\n")
  (in-pr matched-replace)
)
((55 "seven" (nil bind09 . "bind09") ((meta) bind07 . "bindthis"))
 (next "&lt;next&gt;" (nil bind09 . "bind09"))
 (57 "9" (nil bind09 . "bind09") ((meta) bind08 . "ohnoes"))
 (91 "[" (nil bind10 . "bind10") ((control meta) bind01 . "bind01") ((control) #1=(keymap (57 . bind08) (55 . bind07) (27 . bind01)) . "keymap"))
 (97 "a" ((control) bind03 . "bind03") (nil bind02 . "bind02"))
 (102 "f" (nil bind05 . "bind05"))
 (mouse-1 "MOUS1" ((control click) bind12 . "bind12") ((down) bind13 . "bind13"))
 (mouse-2 "&lt;mouse-2&gt;" ((control click) bind06 . "bind06") ((down) bind11 . "bind11"))
 (113 "q" ((control) bind04 . "bind04"))
 (escape "&lt;escape&gt;" (nil #1# . "keymap") ((meta) bind01 . "bind01"))
 ((49 . 54) "1 ... 6)" (nil bind09 . "bind09")))
((mouse-1 "MOUS1" ("mouse-1" "mouseie") t)
 (bind08 "ohnoes" ("bind008" "0HNO"))
 (bind07 "bindthis")
 (55 "seven" ("7" . "sevensies")))
