(setq a (print-keymap (current-local-map) (current-buffer)))
((?\C-c . (keymap (100 . duplicate-line)))
 (menu-bar . (keymap (emacs-lisp #4="Emacs-Lisp" keymap (indent-line menu-item "Indent Line" lisp-indent-line) (indent-region menu-item "Indent Region" indent-region :help "Indent each nonblank line in the region" . #1=(:enable mark-active)) (comment-region menu-item "Comment Out Region" comment-region :help "Comment or uncomment each line in the region" . #1#) (separator-format #2="--") (eval-sexp menu-item "Evaluate Last S-expression" eval-last-sexp :help "Evaluate sexp before point; print value in echo area") (eval-region menu-item "Evaluate Region" eval-region :help "Execute the region as Lisp code" . #1#) (eval-buffer menu-item "Evaluate Buffer" eval-buffer :help "Execute the current buffer as Lisp code") (ielm menu-item "Interactive Expression Evaluation" ielm :help "Interactively evaluate Emacs Lisp expressions") (separator-eval #2#) (byte-compile menu-item "Byte-compile This File" emacs-lisp-byte-compile :help "Byte compile the file containing the current buffer") (emacs-byte-compile-and-load menu-item "Byte-compile and Load" emacs-lisp-byte-compile-and-load :help "Byte-compile the current file (if it has changed), then load compiled code") (byte-recompile menu-item "Byte-recompile Directory..." byte-recompile-directory :help "Recompile every `.el' file in DIRECTORY that needs recompilation") (disas menu-item "Disassemble Byte Compiled Object..." disassemble :help "Print disassembled code for OBJECT in a buffer") (separator-byte #2#) (edebug-defun menu-item "Instrument Function for Debugging" edebug-defun :help "Evaluate the top level form point is in, stepping through with Edebug" :keys "C-u C-M-x") (lint "Linting" keymap (lint-d menu-item "Lint Defun" elint-defun :help "Lint the function at point") (lint-b menu-item "Lint Buffer" elint-current-buffer :help "Lint the current buffer") (lint-f menu-item "Lint File..." elint-file :help "Lint a file") (lint-di menu-item "Lint Directory..." elint-directory :help "Lint a directory")) (profiling "Profiling" keymap (prof-natprof-start menu-item "Start Native Profiler..." profiler-start :help "Start recording profiling information") (prof-natprof-report menu-item "Show Profiler Report" profiler-report :help "Show the current profiler report" . #3=(:enable (and (featurep (quote profiler)) (profiler-running-p)))) (prof-natprof-stop menu-item "Stop Native Profiler" profiler-stop :help "Stop recording profiling information" . #3#) (sep-natprof #2#) (prof-func menu-item "Instrument Function..." elp-instrument-function :help "Instrument a function for profiling") (prof-pack menu-item "Instrument Package..." elp-instrument-package :help "Instrument for profiling all function that start with a prefix") (prof-res menu-item "Show Profiling Results" elp-results :help "Display current profiling results") (prof-resfunc menu-item "Reset Counters for Function..." elp-reset-function :help "Reset the profiling information for a function") (prof-resall menu-item "Reset Counters for All Functions" elp-reset-all :help "Reset the profiling information for all functions being profiled") (sep-rem #2#) (prof-restfunc menu-item "Remove Instrumentation for Function..." elp-restore-function :help "Restore an instrumented function to its original definition") (prof-restall menu-item "Remove Instrumentation for All Functions" elp-restore-all :help "Restore the original definitions of all functions being profiled")) (tracing "Tracing" keymap (tr-f menu-item "Trace Function..." trace-function :help "Trace the function given as an argument") (tr-q menu-item "Trace Function Quietly..." trace-function-background :help "Trace the function with trace output going quietly to a buffer") (tr-sep #2#) (tr-uf menu-item "Untrace Function..." untrace-function :help "Untrace function, and possibly activate all remaining advice") (tr-a menu-item "Untrace All" untrace-all :help "Untrace all currently traced functions")) (re-builder menu-item "Construct Regexp" re-builder :help "Construct a regexp interactively") (checkdoc menu-item "Check Documentation Strings" checkdoc :help "Check documentation strings for style requirements") (eldoc menu-item "Auto-Display Documentation Strings" eldoc-mode :button (:toggle bound-and-true-p eldoc-mode) :help "Display the documentation string for the item under cursor") #4#)))
 (?\e . (keymap (17 . indent-pp-sexp) (24 . eval-defun) (9 . completion-at-point)))
 (?\d . backward-delete-char-untabify)
 (?\e . (keymap (17 . indent-sexp)))
 (?\e . (keymap (17 . prog-indent-sexp))))

(pptcb a)
((3 keymap
    (100 . duplicate-line))
 (menu-bar keymap
	   (lisp-interaction #1="Lisp-Interaction" keymap
			     (complete-symbol menu-item "Complete Lisp Symbol" completion-at-point :help "Perform completion on Lisp symbol preceding point")
			     (indent-pp-sexp menu-item "Indent or Pretty-Print" indent-pp-sexp :help "Indent each line of the list starting just after point, or prettyprint it")
			     (edebug-defun-lisp-interaction menu-item "Instrument Function for Debugging" edebug-defun :help "Evaluate the top level form point is in, stepping through with Edebug" :keys "C-u C-M-x")
			     (eval-print-last-sexp menu-item "Evaluate and Print" eval-print-last-sexp :help "Evaluate sexp before point; print value into current buffer")
			     (eval-defun menu-item "Evaluate Defun" eval-defun :help "Evaluate the top-level form containing point, or after point")
			     #1#))
 (10 . eval-print-last-sexp)
 (27 keymap
     (9 . completion-at-point)
     (17 . indent-pp-sexp)
     (24 . eval-defun))
 (127 . backward-delete-char-untabify)
 (27 keymap
     (17 . indent-sexp))
 (27 keymap
     (17 . prog-indent-sexp)))





(let ((km (make-keymap)))
  (define-key km [?\M-a] 'blah)
  ;; (ptcb km)
  ;; (ptcb (lookup-key km [?\e ?\a]))
  ;; (map-keymap (lambda (key event)))
  (ptcb-map 'map-keymap km)
  )
((27 keymap
     (97 . blah)))


(let ((km (make-keymap)))
  (define-key km [?\M-a] `defn1)
  (setf km (list 'keymap '(?\M-a . defn2) '(?\M-b . defn3) (nth 1 km)))
  (use-local-map km)
  (ptcb-map 'map-keymap km))



(let ((km (make-keymap)))
  (define-key km [?\M-a] `defn1)
  (setf km (list 'keymap '(?\M-a . defn2) '(?\M-b . defn3) (nth 1 km)))
  ;; (ptcb km)
  ;; (setf (car))
  ;; (push-end '(?\M-a . blah) km)
  ;; ;; (ptcb km)
  ;; ;; (ptcb (lookup-key km [?\e ?\a]))
  ;; ;; (map-keymap (lambda (key event)))
  (ptcb-map 'map-keymap km)
  )
((134217825 . defn2)
 (134217826 . defn3)
 (27 keymap
     (97 . defn1)))


(let ((km-par (make-keymap))
      (km-child (make-keymap)))
  (define-key km-child [?\M-a] `defn-child)
  (define-key km-par [?\M-a] `defn-par)
  (use-local-map km)
  (pptcb km)
  (insert "\n")
  (ptcb-map 'map-keymap km)
  )

(let ((km-par (make-keymap))
      (km-child (make-keymap)))
  (define-key km-child [?\M-a] `defn-child)
  (define-key km-par [?\M-a] `defn-par)
  (use-local-map km-child)
  ;; (pptcb km)
  ;; (insert "\n")
  ;; (ptcb-map 'map-keymap km)
  )

(let ((km-par (make-sparse-keymap))
      (km-child (make-keymap)))
  (set-keymap-parent km-child km-par)
  (define-key km-child [?\M-a] `defn-child)
  (define-key km-par [?\M-a] `defn-par)
  (use-local-map km-child)
  (pptcb km-par)
  ;; (insert "\n")
  ;; (ptcb-map 'map-keymap km)
  )

(progn
  (setq km-par (make-keymap))
  (setq km-child (make-keymap))
  (set-keymap-parent km-child km-par)
  (define-key km-child [?\C-a] `defn-child)
  (define-key km-par [?\C-a] `defn-par)
  (define-key km-child [?\C-a] nil)
  (use-local-map km-child)
  (pptcb km-child)
  (insert "\n")
  (pptcb km-par)
  ;; (ptcb-map 'map-keymap km)
  )

(progn
  (setq km-par (make-keymap))
  (setq km-child (make-keymap))
  (set-keymap-parent km-child km-par)
  (define-key km-child [?\M-a] `defn-a-child)
  (define-key km-par [?\M-b] `defn-b-par)
  ;; (define-key km-par [?\M-a] `defn-par)
  (map-keymap (lambda (event defn) (ptcb (cons event defn)) (insert "\n")) km-child)
  ;; (use-local-map km-child)
  ;; (pptcb km-child)
  ;; (insert "\n")
  ;; (pptcb km-par)
  ;; (ptcb-map 'map-keymap km)
  )
(27 keymap (97 . defn-a-child))
(27 keymap (98 . defn-b-child))


(defun pkb-remap-esc-prefix-to-meta (keymap)
  "If ESC is a prefix-key in KEYMAP, (non-destuctively) remap all events in that prefix-key that fit in a char-table to an appropriate meta event in KEYMAP."
  (let ((keymap-copied (copy-keymap keymap))
	(esc-defn (lookup-key keymap [?\e])) meta-key)
    (when (keymapp esc-defn)
      (define-key keymap ?\e (copy-keymap esc-defn)))
    (map-keymap
     (lambda (event defn)
       (when (pkb-char-event-p event)
	 ;;  remember that ?\C-@ = 0, so that ?\M-\C-@ is the meta bit alone.
	 ;;  `logior' below could simply be + but using `logior' makes the
	 ;;  intent more clear
	 (setf meta-key (logior ?\M-\C-@ event))
	 
	 (define-key (event-convert-list))))
     esc-defn)))



(ptcb(current-local-map))

(pkb-map-keymap-meta-chars-in-esc-prefix
 )

(funcall (lambda (a standard-output) (prin1 a)) 'r (current-buffer))

(pkb-print-keymap (current-local-map))



(with-output-to-current-buffer (pkb-print-keymap (current-local-map)))

(let ((standard-output (current-buffer)))
(pkb-print-keymap (current-local-map)))

(pkb-print-keymap (current-local-map) (current-buffer))
nil

(map-keymap (lambda (event defn) (ptcb (cons event defn))) (current-global-map))


(pptcb temp)
(lambda
  (event defn indent stream)
  (if
      (keymapp defn)
      (pkb-print-keymap
       (defn stream indent
	 (vconcat prefix-key
		  (vector event))
	 events-as-escape-sequences-flag key-sequence-filter-fcn nil sort-event-fcn copy-esc-to-meta-flag))
    (prin1 defn)
    defn))


(pptcb temp)
((menu-bar keymap
	   (text "Text" keymap
		 (center-line menu-item "Center Line" center-line :help "Center the current line")
		 (center-paragraph menu-item "Center Paragraph" center-paragraph :help "Center the current paragraph")
		 (center-region menu-item "Center Region" center-region :help "Center the marked region" :enable
				(region-active-p))
		 (sep "--")
		 (paragraph-indent-minor-mode menu-item "Paragraph Indent" paragraph-indent-minor-mode :button
					      (:toggle bound-and-true-p paragraph-indent-minor-mode)
					      :help "Toggle paragraph indent minor mode")
		 (toggle-text-mode-auto-fill menu-item "Auto Fill" toggle-text-mode-auto-fill :button
					     (:toggle memq 'turn-on-auto-fill text-mode-hook)
					     :help "Automatically fill text while typing in text modes (Auto Fill mode)")
		 "Text"))
 (27 keymap
     (9 . ispell-complete-word))
 (134217737 . ispell-complete-word))


(pptcb (pkb-make-test-keymap :include-menu-bar nil :include-tool-bar nil :include-remap nil))


(setq test-keymaps (pkb-make-test-keymap))
(setq test-keymap (nth 0 test-keymaps))


(wotcb (pkb-print-keymap (lookup-key test-small-keymap [?\e])))
((?c . km3-shadowed-M-c)
 (?\S-b . km3-only-M-S-b)
 (?b . km3-only-M-b)
 (?\S-a . km3-defn-M-S-a)
 (?a . km3-defn-M-a))

(wotcb (pkb-print-keymap test-small-keymap))
(pptcb test-small-keymap)

(wotcb
 (pkb-print-keymap test-keymap standard-output
		   :key-sequence-filter-fcn
		   (lambda (seq) (not (member (elt seq 0) '(menu-bar tool-bar remap))))))



(cl-defun take-fcn (&key fcn)
  (funcall fcn))

(take-fcn :fcn (lambda () (insert "123")))


(setq test-keymaps
      (pkb-make-test-keymap :include-menu-bar nil :include-tool-bar nil :include-remap nil))
(setq test-keymaps
      (pkb-make-test-keymap))
(setq test-keymap (nth 0 test-keymaps))
(setq test-small-keymap (nth 2 test-keymaps))  

(pptcb (current-global-map))
(keymap
 #^[nil nil keymap #1=
	#^^[3 0 set-mark-command move-beginning-of-line backward-char mode-specific-command-prefix delete-char move-end-of-line forward-char keyboard-quit help-command indent-for-tab-command electric-newline-and-maybe-indent kill-line recenter-top-bottom newline next-line open-line previous-line quoted-insert isearch-backward isearch-forward transpose-chars universal-argument scroll-up-command kill-region Control-X-prefix yank t ESC-prefix toggle-input-method abort-recursive-edit nil undo self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command delete-backward-char]
	#^^[1 0
	      #^^[2 0 #1# self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command]
	      self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command]
	self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command]
 (remap keymap
	(find-file-other-window)
	(find-file))
 (s-f1 lambda nil
       (interactive)
       (set-frame-width
	(selected-frame)
	90))
 (f6 . imenu)
 (f5 keymap
     (51 . bookmark-menu-list)
     (50 . bookmark-jump)
     (49 . bookmark-set))
 (event)
 (s-down-mouse-3 . imenu)
 (8388670 . other-frame-back)
 (8388713 . overwrite-mode)
 (8388668 . other-frame)
 (S-mouse-3 . kmacro-end-call-mouse)
 (C-wheel-down . mwheel-scroll)
 (S-wheel-down . mwheel-scroll)
 (wheel-down . mwheel-scroll)
 (C-wheel-up . mwheel-scroll)
 (S-wheel-up . mwheel-scroll)
 (wheel-up . mwheel-scroll)
 (C-M-drag-n-drop . ns-drag-n-drop-as-text-other-frame)
 (M-drag-n-drop . ns-drag-n-drop-as-text)
 (C-drag-n-drop . ns-drag-n-drop-other-frame)
 (drag-n-drop . ns-drag-n-drop)
 (mac-change-input-method . mac-change-input-method)
 (ns-show-prefs . customize)
 (ns-toggle-toolbar . ns-toggle-toolbar)
 (ns-new-frame . make-frame)
 (ns-spi-service-call . ns-spi-service-call)
 (ns-open-file-line . ns-open-file-select-line)
 (ns-change-font . ns-respond-to-change-font)
 (ns-open-temp-file .
		    [ns-open-file])
 (ns-open-file . ns-find-file)
 (ns-power-off . save-buffers-kill-emacs)
 (S-mouse-1 . mouse-save-then-kill)
 (kp-next . scroll-up-command)
 (kp-prior . scroll-down-command)
 (kp-end . end-of-buffer)
 (kp-home . beginning-of-buffer)
 (s-left . ns-prev-frame)
 (s-right . ns-next-frame)
 (s-kp-bar . shell-command-on-region)
 (8388732 . shell-command-on-region)
 (8388730 . undo)
 (8388729 . ns-paste-secondary)
 (8388728 . kill-region)
 (8388727 . delete-frame)
 (8388726 . yank)
 (8388725 . revert-buffer)
 (8388724 . ns-popup-font-panel)
 (8388723 . save-buffer)
 (8388721)
 (8388720 . ns-print-buffer)
 (8388719 . ns-open-file-using-panel)
 (8388718 . make-frame)
 (8388717)
 (8388716 . goto-line)
 (8388715 . kill-this-buffer)
 (8388714 . exchange-point-and-mark)
 (8388680 . ns-do-hide-others)
 (8388712 . ns-do-hide-emacs)
 (8388711)
 (8388710 . isearch-forward)
 (8388709)
 (8388708)
 (8388707 . ns-copy-including-secondary)
 (8388705 . mark-whole-buffer)
 (8388691 . ns-write-file-using-panel)
 (8388685 . manual-entry)
 (8388684 . shell-command)
 (8388677 . edit-abbrevs)
 (8388676 . dired)
 (8388675 . ns-popup-color-panel)
 (8388646 . kill-this-buffer)
 (8388702 . kill-some-buffers)
 (8388671 . info)
 (8388666 . ispell)
 (8388653 . center-line)
 (8388734 . ns-prev-frame)
 (8388704 . other-frame)
 (8388647 . next-multiframe-window)
 (8388652 . customize)
 (tool-bar menu-item "tool bar" ignore :filter tool-bar-make-keymap)
 (f10 . menu-bar-open)
 (bottom-divider keymap
		 (down-mouse-1 . mouse-drag-mode-line))
 (right-divider keymap
		(down-mouse-1 . mouse-drag-vertical-line))
 (vertical-line keymap
		(mouse-1 . mouse-select-window)
		(down-mouse-1 . mouse-drag-vertical-line)
		(C-mouse-2 . mouse-split-window-vertically))
 (vertical-scroll-bar keymap
		      (drag-mouse-1)
		      (down-mouse-1 . ns-handle-scroll-bar-event)
		      (mouse-1)
		      (C-mouse-2 . mouse-split-window-vertically))
 (header-line keymap
	      (mouse-1 . mouse-select-window)
	      (down-mouse-1 . mouse-drag-header-line))
 (mode-line keymap
	    (C-mouse-2 . mouse-split-window-horizontally)
	    (mouse-3 . mouse-delete-window)
	    (mouse-2 . mouse-delete-other-windows)
	    (down-mouse-1 . mouse-drag-mode-line)
	    (drag-mouse-1 . mouse-select-window)
	    (mouse-1 . mouse-select-window))
 (C-down-mouse-3 menu-item "Menu Bar" ignore :filter
		 (lambda
		   (_)
		   (if
		       (zerop
			(or
			 (frame-parameter nil 'menu-bar-lines)
			 0))
		       (mouse-menu-bar-map)
		     (mouse-menu-major-mode-map))))
 (S-down-mouse-1)
 (C-down-mouse-1 . mouse-buffer-menu)
 (mouse-3 . mouse-save-then-kill)
 (mouse-2 . mouse-yank-primary)
 (triple-mouse-1 . mouse-set-point)
 (double-mouse-1 . mouse-set-point)
 (drag-mouse-1 . mouse-set-region)
 (mouse-1 . mouse-set-point)
 (down-mouse-1 . mouse-drag-region)
 (M-mouse-2 . mouse-yank-secondary)
 (M-mouse-3 . mouse-secondary-save-then-kill)
 (M-down-mouse-1 . mouse-drag-secondary)
 (M-drag-mouse-1 . mouse-set-secondary)
 (M-mouse-1 . mouse-start-secondary)
 (C-down-mouse-2 . facemenu-menu)
 (M-f10 . toggle-frame-maximized)
 (f11 . toggle-frame-fullscreen)
 (compose-last-chars . compose-last-chars)
 (f1 . help-command)
 (help . help-command)
 (f2 . 2C-command)
 (menu-bar keymap
	   (help-menu #9="Help" keymap
		      (emacs-tutorial menu-item "Emacs Tutorial" help-with-tutorial :help "Learn how to use Emacs")
		      (emacs-tutorial-language-specific menu-item "Emacs Tutorial (choose language)..." help-with-tutorial-spec-language :help "Learn how to use Emacs (choose a language)")
		      (emacs-faq menu-item "Emacs FAQ" view-emacs-FAQ :help "Frequently asked (and answered) questions about Emacs")
		      (emacs-news menu-item "Emacs News" view-emacs-news :help "New features of this version")
		      (emacs-known-problems menu-item "Emacs Known Problems" view-emacs-problems :help "Read about known problems with Emacs")
		      (emacs-manual-bug menu-item "How to Report a Bug" info-emacs-bug :help "Read about how to report an Emacs bug")
		      (send-emacs-bug-report menu-item "Send Bug Report..." report-emacs-bug :help "Send e-mail to Emacs maintainers")
		      (emacs-psychotherapist menu-item "Emacs Psychotherapist" doctor :help "Our doctor will help you feel better")
		      (sep1 #2="--")
		      (search-documentation menu-item #3="Search Documentation"
					    (keymap
					     (emacs-terminology menu-item "Emacs Terminology" search-emacs-glossary :help "Display the Glossary section of the Emacs manual")
					     (lookup-subject-in-emacs-manual menu-item "Look Up Subject in User Manual..." emacs-index-search :help "Find description of a subject in Emacs User manual")
					     (lookup-subject-in-elisp-manual menu-item "Look Up Subject in ELisp Manual..." elisp-index-search :help "Find description of a subject in Emacs Lisp manual")
					     (lookup-key-in-manual menu-item "Look Up Key in User Manual..." Info-goto-emacs-key-command-node :help "Display manual section that describes a key")
					     (lookup-command-in-manual menu-item "Look Up Command in User Manual..." Info-goto-emacs-command-node :help "Display manual section that describes a command")
					     (sep1 #2#)
					     (find-commands-by-name menu-item "Find Commands by Name..." apropos-command :help "Find commands whose names match a regexp")
					     (find-options-by-name menu-item "Find Options by Name..." apropos-user-option :help "Find user options whose names match a regexp")
					     (find-option-by-value menu-item "Find Options by Value..." apropos-value :help "Find variables whose values match a regexp")
					     (find-any-object-by-name menu-item "Find Any Object by Name..." apropos :help "Find symbols of any kind whose names match a regexp")
					     (search-documentation-strings menu-item "Search Documentation Strings..." apropos-documentation :help "Find functions and variables whose doc strings match a regexp")
					     #3#))
		      (describe menu-item #7="Describe"
				(keymap
				 (describe-mode menu-item "Describe Buffer Modes" describe-mode :help "Describe this buffer's major and minor mode")
				 (describe-key-1 menu-item "Describe Key or Mouse Operation..." describe-key :help "Display documentation of command bound to a key, a click, or a menu-item")
				 (describe-function menu-item "Describe Function..." describe-function :help "Display documentation of function/command")
				 (describe-variable menu-item "Describe Variable..." describe-variable :help "Display documentation of variable/option")
				 (describe-face menu-item "Describe Face..." describe-face :help "Display the properties of a face")
				 (describe-package menu-item "Describe Package..." describe-package :help "Display documentation of a Lisp package")
				 (describe-current-display-table menu-item "Describe Display Table" describe-current-display-table :help "Describe the current display table")
				 (list-keybindings menu-item "List Key Bindings" describe-bindings :help "Display all current key bindings (keyboard shortcuts)")
				 (separator-desc-mule #2#)
				 (describe-language-environment menu-item #4="Describe Language Environment" #80=(keymap
														  (Default menu-item #49="Default" describe-specified-language-support)
														  #4#
														  (Chinese #51="Chinese" . describe-chinese-environment-map)
														  (Cyrillic #52="Cyrillic" . describe-cyrillic-environment-map)
														  (Indian #53="Indian" . describe-indian-environment-map)
														  (Sinhala #54="Sinhala" . describe-specified-language-support)
														  (English #55="English" . describe-specified-language-support)
														  (ASCII #56="ASCII" . describe-specified-language-support)
														  (Ethiopic #57="Ethiopic" . describe-specified-language-support)
														  (European #58="European" . describe-european-environment-map)
														  (Turkish #59="Turkish" . describe-specified-language-support)
														  (Greek #60="Greek" . describe-specified-language-support)
														  (Hebrew #61="Hebrew" . describe-specified-language-support)
														  (Windows-1255 #62="Windows-1255" . describe-specified-language-support)
														  (Japanese #63="Japanese" . describe-specified-language-support)
														  (Korean #64="Korean" . describe-specified-language-support)
														  (Lao #65="Lao" . describe-specified-language-support)
														  (TaiViet #66="TaiViet" . describe-specified-language-support)
														  (Thai #67="Thai" . describe-specified-language-support)
														  (Tibetan #68="Tibetan" . describe-specified-language-support)
														  (Vietnamese #69="Vietnamese" . describe-specified-language-support)
														  (IPA #70="IPA" . describe-specified-language-support)
														  (Arabic #71="Arabic" . describe-specified-language-support)
														  (Persian #72="Persian" . describe-specified-language-support)
														  (UTF-8 #73="UTF-8" . describe-specified-language-support)
														  (Khmer #74="Khmer" . describe-specified-language-support)
														  (Burmese #75="Burmese" . describe-specified-language-support)))
				 (describe-input-method menu-item "Describe Input Method..." describe-input-method :visible #5=(default-value 'enable-multibyte-characters)
							:help "Keyboard layout for specific input method")
				 (describe-coding-system menu-item #81="Describe Coding System..." describe-coding-system . #6=(:visible . #79=(#5#)))
				 (describe-coding-system-briefly menu-item "Describe Coding System (Briefly)" describe-current-coding-system-briefly . #6#)
				 (mule-diag menu-item "Show All of Mule Status" mule-diag :visible #5# . #82=(:help "Display multilingual environment settings"))
				 #7#))
		      (emacs-manual menu-item "Read the Emacs Manual" info-emacs-manual :help "Full documentation of Emacs features")
		      (more-manuals menu-item #8="More Manuals"
				    (keymap
				     (emacs-lisp-intro menu-item "Introduction to Emacs Lisp" menu-bar-read-lispintro :help "Read the Introduction to Emacs Lisp Programming")
				     (emacs-lisp-reference menu-item "Emacs Lisp Reference" menu-bar-read-lispref :help "Read the Emacs Lisp Reference manual")
				     (other-manuals menu-item "All Other Manuals (Info)" Info-directory :help "Read any of the installed manuals")
				     (lookup-subject-in-all-manuals menu-item "Lookup Subject in all Manuals..." info-apropos :help "Find description of a subject in all installed manuals")
				     (order-emacs-manuals menu-item "Ordering Manuals" view-order-manuals :help "How to order manuals from the Free Software Foundation")
				     (sep2 #2#)
				     (man menu-item "Read Man Page..." manual-entry :help "Man-page docs for external commands and libraries")
				     #8#))
		      (find-emacs-packages menu-item "Search Built-in Packages" finder-by-keyword :help "Find built-in packages and features by keyword")
		      (external-packages menu-item "Finding Extra Packages" view-external-packages :help "How to get more Lisp packages for use in Emacs")
		      (sep2 #2#)
		      (getting-new-versions menu-item "Getting New Versions" describe-distribution :help "How to get the latest version of Emacs")
		      (describe-copying menu-item "Copying Conditions" describe-copying :help "Show the Emacs license (GPL)")
		      (describe-no-warranty menu-item "(Non)Warranty" describe-no-warranty :help "Explain that Emacs has NO WARRANTY")
		      (sep4 #2#)
		      (about-emacs menu-item "About Emacs" about-emacs :help "Display version number, copyright info, and basic help")
		      (about-gnu-project menu-item "About GNU" describe-gnu-project :help "About the GNU System, GNU Project, and GNU/Linux")
		      #9#)
	   (file #17="File" keymap
		 (new-file menu-item "Visit New File..." find-file :enable #10=(menu-bar-non-minibuffer-window-p)
			   :help "Specify a new file's name, to edit the file")
		 (open-file menu-item "Open File..." menu-find-file-existing :enable #10# :help "Read an existing file into an Emacs buffer")
		 (dired menu-item "Open Directory..." dired :enable #10# :help "Read a directory, to operate on its files")
		 (insert-file menu-item "Insert File..." insert-file :enable #10# :help "Insert another file into current buffer")
		 (kill-buffer menu-item "Close" kill-this-buffer :enable
			      (kill-this-buffer-enabled-p)
			      :help "Discard (kill) current buffer")
		 (separator-save #2#)
		 (save-buffer menu-item "Save" save-buffer :enable
			      (and #12=(buffer-modified-p)
				   (buffer-file-name)
				   . #11=(#10#))
			      :help "Save current buffer to its file")
		 (write-file menu-item "Save As..." write-file :enable #16=(and #15=(menu-bar-menu-frame-live-and-visible-p)
										. #11#)
			     :help "Write current buffer to another file")
		 (revert-buffer menu-item "Revert Buffer" revert-buffer :enable
				(or
				 (not
				  (eq revert-buffer-function 'revert-buffer--default))
				 (not
				  (eq revert-buffer-insert-file-contents-function 'revert-buffer-insert-file-contents--default-function))
				 (and buffer-file-number
				      (or #12#
					  (not
					   (verify-visited-file-modtime . #78=((current-buffer)))))))
				:help "Re-read current buffer from its file")
		 (recover-session menu-item "Recover Crashed Session" recover-session :enable
				  (and auto-save-list-file-prefix
				       (file-directory-p #13=(file-name-directory . #14=(auto-save-list-file-prefix)))
				       (directory-files #13# nil
							(concat "\\`"
								(regexp-quote
								 (file-name-nondirectory . #14#)))
							t))
				  :help "Recover edits from a crashed session")
		 (separator-print #2#)
		 (print-buffer menu-item "Print Buffer" print-buffer :enable #15# :help "Print current buffer with page headings")
		 (print-region menu-item "Print Region" print-region :enable mark-active :help "Print region between mark and current position")
		 (ps-print-buffer-faces menu-item "PostScript Print Buffer" ps-print-buffer-with-faces :enable #15# :help "Pretty-print current buffer to PostScript printer")
		 (ps-print-region-faces menu-item "PostScript Print Region" ps-print-region-with-faces :enable mark-active :help "Pretty-print marked region to PostScript printer")
		 (ps-print-buffer menu-item "PostScript Print Buffer (B+W)" ps-print-buffer :enable #15# :help "Pretty-print current buffer in black and white to PostScript printer")
		 (ps-print-region menu-item "PostScript Print Region (B+W)" ps-print-region :enable mark-active :help "Pretty-print marked region in black and white to PostScript printer")
		 (separator-window #2#)
		 (new-window-below menu-item "New Window Below" split-window-below :enable #16# :help "Make new window below selected one")
		 (new-window-on-right menu-item "New Window on Right" split-window-right :enable #16# :help "Make new window on right of selected one")
		 (one-window menu-item "Remove Other Windows" delete-other-windows :enable
			     (not
			      (one-window-p t . #85=(nil)))
			     :help "Make selected window fill whole frame")
		 (separator-frame #2#)
		 (make-frame menu-item "New Frame" make-frame-command :visible
			     (fboundp 'make-frame-command)
			     :help "Open a new frame")
		 (make-frame-on-display menu-item "New Frame on Display..." make-frame-on-display :visible
					(fboundp 'make-frame-on-display)
					:help "Open a new frame on another display")
		 (delete-this-frame menu-item "Delete Frame" delete-frame :visible
				    (fboundp 'delete-frame)
				    :enable
				    (delete-frame-enabled-p)
				    :help "Delete currently selected frame")
		 (separator-exit #2#)
		 (exit-emacs menu-item "Quit" save-buffers-kill-terminal :help "Save unsaved buffers, then exit")
		 #17#)
	   (edit #30="Edit" keymap
		 (undo menu-item "Undo" undo :enable
		       (and #19=(not buffer-read-only)
			    (not
			     (eq t . #18=(buffer-undo-list)))
			    (if
				(eq last-command 'undo)
				(listp pending-undo-list)
			      (consp . #18#)))
		       :help "Undo last operation")
		 (separator-undo #2#)
		 (cut menu-item "Cut" kill-region :enable #22=(and mark-active . #20=(#19#))
		      :help "Cut (kill) text in region between mark and current position")
		 (copy menu-item "Copy" ns-copy-including-secondary :enable mark-active :help "Copy text in region between mark and current position" :keys "\\[ns-copy-including-secondary]")
		 (paste menu-item "Paste" yank :enable
			(and
			 (or
			  (and
			   (fboundp 'x-selection-exists-p)
			   (x-selection-exists-p 'CLIPBOARD))
			  (if
			      (featurep 'ns)
			      #21=(cdr yank-menu)
			      kill-ring))
			 . #20#)
			:help "Paste (yank) text most recently cut/copied")
		 (select-paste menu-item "Select and Paste" yank-menu :enable
			       (and #21# . #20#)
			       :help "Choose a string from the kill ring and paste it")
		 (clear menu-item "Clear" delete-region :enable #22# :help "Delete the text in region between mark and current position")
		 (mark-whole-buffer menu-item "Select All" mark-whole-buffer :help "Mark the whole buffer for a subsequent cut/copy")
		 (separator-search #2#)
		 (search menu-item #25="Search"
			 (keymap
			  (search-forward menu-item "String Forward..." nonincremental-search-forward :help "Search forward for a string")
			  (search-backward menu-item "String Backwards..." nonincremental-search-backward :help "Search backwards for a string")
			  (re-search-forward menu-item "Regexp Forward..." nonincremental-re-search-forward :help "Search forward for a regular expression")
			  (re-search-backward menu-item "Regexp Backwards..." nonincremental-re-search-backward :help "Search backwards for a regular expression")
			  (separator-repeat-search #2#)
			  (repeat-search-fwd menu-item "Repeat Forward" nonincremental-repeat-search-forward :enable #23=(or
															  (and
															   (eq menu-bar-last-search-type 'string)
															   search-ring)
															  (and
															   (eq menu-bar-last-search-type 'regexp)
															   regexp-search-ring))
					     :help "Repeat last search forward")
			  (repeat-search-back menu-item "Repeat Backwards" nonincremental-repeat-search-backward :enable #23# :help "Repeat last search backwards")
			  (separator-tag-search #2#)
			  (tags-srch menu-item "Search Tagged Files..." tags-search :help "Search for a regexp in all tagged files")
			  (tags-continue menu-item "Continue Tags Search" tags-loop-continue :help "Continue last tags search operation")
			  (separator-tag-isearch #2#)
			  (i-search menu-item #24="Incremental Search"
				    (keymap
				     (isearch-forward menu-item "Forward String..." isearch-forward :help "Search forward for a string as you type it")
				     (isearch-backward menu-item "Backward String..." isearch-backward :help "Search backwards for a string as you type it")
				     (isearch-forward-regexp menu-item "Forward Regexp..." isearch-forward-regexp :help "Search forward for a regular expression as you type it")
				     (isearch-backward-regexp menu-item "Backward Regexp..." isearch-backward-regexp :help "Search backwards for a regular expression as you type it")
				     #24#))
			  #25#))
		 (replace menu-item #26="Replace"
			  (keymap
			   (query-replace menu-item "Replace String..." query-replace :enable #19# :help "Replace string interactively, ask about each occurrence")
			   (query-replace-regexp menu-item "Replace Regexp..." query-replace-regexp :enable #19# :help "Replace regular expression interactively, ask about each occurrence")
			   (separator-replace-tags #2#)
			   (tags-repl menu-item "Replace in Tagged Files..." tags-query-replace :help "Interactively replace a regexp in all tagged files")
			   (tags-repl-continue menu-item "Continue Replace" tags-loop-continue :help "Continue last tags replace operation")
			   #26#))
		 (goto menu-item #29="Go To"
		       (keymap
			(go-to-line menu-item "Goto Line..." goto-line :help "Read a line number and go to that line")
			(go-to-pos menu-item "Goto Buffer Position..." goto-char :help "Read a number N and go to buffer position N")
			(beg-of-buf menu-item "Goto Beginning of Buffer" beginning-of-buffer)
			(end-of-buf menu-item "Goto End of Buffer" end-of-buffer)
			(separator-tags #2#)
			(find-tag menu-item "Find Tag..." find-tag :help "Find definition of function or variable")
			(find-tag-otherw menu-item "Find Tag in Other Window..." find-tag-other-window :help "Find function/variable definition in another window")
			(next-tag menu-item "Find Next Tag" menu-bar-next-tag :enable #28=(and
											   (boundp 'tags-location-ring)
											   (not
											    (ring-empty-p . #27=(tags-location-ring))))
				  :help "Find next function/variable matching last tag name")
			(next-tag-otherw menu-item "Next Tag in Other Window" menu-bar-next-tag-other-window :enable #28# :help "Find next function/variable matching last tag name in another window")
			(apropos-tags menu-item "Tags Apropos..." tags-apropos :help "Find function/variables whose names match regexp")
			(separator-tag-file #2#)
			(set-tags-name menu-item "Set Tags File Name..." visit-tags-table :help "Tell Tags commands which tag table file to use")
			#29#))
		 (bookmark menu-item "Bookmarks" menu-bar-bookmark-map)
		 (separator-bookmark #2#)
		 (fill menu-item "Fill" fill-region :enable #22# :help "Fill text in region to fit between left and right margin")
		 (spell menu-item "Spell" ispell-menu-map)
		 (props menu-item "Text Properties" facemenu-menu)
		 #30#)
	   (options #108="Options" keymap
		    (transient-mark-mode menu-item "Highlight Active Region" transient-mark-mode :enable
					 (not . #40=(cua-mode))
					 :help "Make text in active region stand out in color (Transient Mark mode)" :button
					 (:toggle and
						  (default-boundp . #31=('transient-mark-mode))
						  (default-value . #31#)))
		    (highlight-paren-mode menu-item "Highlight Matching Parentheses" show-paren-mode :help "Highlight matching/mismatched parentheses at cursor (Show Paren mode)" :button
					  (:toggle and
						   (default-boundp . #32=('show-paren-mode))
						   (default-value . #32#)))
		    (highlight-separator #2#)
		    (line-wrapping menu-item "Line Wrapping in This Buffer"
				   (keymap
				    (window-wrap menu-item "Wrap at Window Edge"
						 #[nil "\203 \300\303!\210\304\n\205 \305\306!\207"
						       [visual-line-mode word-wrap truncate-lines 0 nil toggle-truncate-lines -1]
						       2 nil nil]
						 :help "Wrap long lines at window edge" :button
						 (:radio and #36=(null truncate-lines)
							 #33=(not . #34=((truncated-partial-width-window-p)))
							 (not . #37=(word-wrap)))
						 . #35=(:visible #15# :enable #33#))
				    (truncate menu-item "Truncate Long Lines"
					      #[nil "\203 \300\302!\210\303\304\305!\207"
						    [visual-line-mode word-wrap 0 nil toggle-truncate-lines 1]
						    2 nil nil]
					      :help "Truncate long lines at window edge" :button
					      (:radio or truncate-lines . #34#)
					      . #35#)
				    (word-wrap menu-item "Word Wrap (Visual Line mode)"
					       #[nil "\204 \300\301!\210\302\303!\207"
						     [visual-line-mode 1 message "Visual-Line mode enabled"]
						     2 nil nil]
					       :help "Wrap long lines at word boundaries" :button
					       (:radio and #36# #33# . #37#)
					       :visible #15#)
				    "Line Wrapping"))
		    (case-fold-search menu-item "Ignore Case for Search" toggle-case-fold-search :help "Ignore letter-case in search commands" :button
				      (:toggle and
					       (default-boundp . #38=('case-fold-search))
					       (default-value . #38#)))
		    (cua-emulation-mode menu-item "Shift movement mark region (CUA)" cua-mode :visible
					(and #42=(boundp 'cua-enable-cua-keys)
					     (not . #39=(cua-enable-cua-keys)))
					:help "Use shifted movement keys to set and extend the region" . #43=(:button
													      (:toggle and
														       (default-boundp . #41=('cua-mode))
														       (default-value . #41#))))
		    (cua-mode menu-item "Use CUA Keys (Cut/Paste with C-x/C-c/C-v)" cua-mode :visible
			      (or
			       (not #42#)
			       . #39#)
			      :help "Use C-z/C-x/C-c/C-v keys for undo/cut/copy/paste" . #43#)
		    (edit-options-separator #2#)
		    (uniquify menu-item "Use Directory Names in Buffer Names" toggle-uniquify-buffer-names :help "Uniquify buffer names by adding parent directory names" :button
			      (:toggle and
				       (default-boundp . #44=('uniquify-buffer-name-style))
				       (default-value . #44#)))
		    (save-place menu-item "Save Place in Files between Sessions" toggle-save-place-globally :help "Visit files of previous session when restarting Emacs" :button
				(:toggle and
					 (default-boundp . #45=('save-place))
					 (default-value . #45#)))
		    (cursor-separator #2#)
		    (blink-cursor-mode menu-item "Blink Cursor" blink-cursor-mode :help "Whether the cursor blinks (Blink Cursor mode)" :button
				       (:toggle and
						(default-boundp . #46=('blink-cursor-mode))
						(default-value . #46#)))
		    (debugger-separator #2#)
		    (debug-on-error menu-item "Enter Debugger on Error" toggle-debug-on-error :help "Enter Lisp debugger when an error is signaled" :button
				    (:toggle and
					     (default-boundp . #47=('debug-on-error))
					     (default-value . #47#)))
		    (debug-on-quit menu-item "Enter Debugger on Quit/C-g" toggle-debug-on-quit :help "Enter Lisp debugger when C-g is pressed" :button
				   (:toggle and
					    (default-boundp . #48=('debug-on-quit))
					    (default-value . #48#)))
		    (mule-separator #2#)
		    (mule menu-item "Multilingual Environment"
			  (keymap
			   (set-language-environment menu-item #50="Set Language Environment"
						     (keymap
						      (Default menu-item #49# setup-specified-language-environment)
						      #50#
						      (Chinese #51# . setup-chinese-environment-map)
						      (Cyrillic #52# . setup-cyrillic-environment-map)
						      (Indian #53# . setup-indian-environment-map)
						      (Sinhala #54# . setup-specified-language-environment)
						      (English #55# . setup-specified-language-environment)
						      (ASCII #56# . setup-specified-language-environment)
						      (Ethiopic #57# . setup-specified-language-environment)
						      (European #58# . setup-european-environment-map)
						      (Turkish #59# . setup-specified-language-environment)
						      (Greek #60# . setup-specified-language-environment)
						      (Hebrew #61# . setup-specified-language-environment)
						      (Windows-1255 #62# . setup-specified-language-environment)
						      (Japanese #63# . setup-specified-language-environment)
						      (Korean #64# . setup-specified-language-environment)
						      (Lao #65# . setup-specified-language-environment)
						      (TaiViet #66# . setup-specified-language-environment)
						      (Thai #67# . setup-specified-language-environment)
						      (Tibetan #68# . setup-specified-language-environment)
						      (Vietnamese #69# . setup-specified-language-environment)
						      (IPA #70# . setup-specified-language-environment)
						      (Arabic #71# . setup-specified-language-environment)
						      (Persian #72# . setup-specified-language-environment)
						      (UTF-8 #73# . setup-specified-language-environment)
						      (Khmer #74# . setup-specified-language-environment)
						      (Burmese #75# . setup-specified-language-environment)
						      (Cham "Cham" . setup-specified-language-environment)))
			   (separator-mule #2#)
			   (toggle-input-method menu-item "Toggle Input Method" toggle-input-method)
			   (set-input-method menu-item "Select Input Method..." set-input-method)
			   (separator-input-method #2#)
			   (set-various-coding-system menu-item "Set Coding Systems"
						      (keymap
						       (universal-coding-system-argument menu-item "For Next Command" universal-coding-system-argument :help "Coding system to be used by next command")
						       (separator-1 #2#)
						       (set-buffer-file-coding-system menu-item "For Saving This Buffer" set-buffer-file-coding-system :help "How to encode this buffer when saved")
						       (revert-buffer-with-coding-system menu-item "For Reverting This File Now" revert-buffer-with-coding-system :enable buffer-file-name :help "Revisit this file immediately using specified coding system")
						       (set-file-name-coding-system menu-item "For File Name" set-file-name-coding-system :help "How to decode/encode file names")
						       (separator-2 #2#)
						       (set-keyboard-coding-system menu-item "For Keyboard" set-keyboard-coding-system :help "How to decode keyboard input")
						       (set-terminal-coding-system menu-item "For Terminal" set-terminal-coding-system :enable
										   (null
										    (memq initial-window-system
											  '(x w32 . #76=(ns))))
										   :help "How to encode terminal output")
						       (separator-3 #2#)
						       (set-selection-coding-system menu-item "For X Selections/Clipboard" set-selection-coding-system :visible #77=(display-selections-p)
										    :help "How to en/decode data to/from selection/clipboard")
						       (set-next-selection-coding-system menu-item "For Next X Selection" set-next-selection-coding-system :visible #77# :help "How to en/decode next selection/clipboard operation")
						       (set-buffer-process-coding-system menu-item "For I/O with Subprocess" set-buffer-process-coding-system :visible
											 (fboundp 'start-process)
											 :enable
											 (get-buffer-process . #78#)
											 :help "How to en/decode I/O from/to subprocess connected to this buffer")
						       "Set Coding System")
						      :enable . #79#)
			   (view-hello-file menu-item "Show Multilingual Sample Text" view-hello-file :enable
					    (file-readable-p
					     (expand-file-name "HELLO" data-directory))
					    :help "Demonstrate various character sets")
			   (separator-coding-system #2#)
			   (describe-language-environment menu-item #4# #80# :help "Show multilingual settings for a specific language")
			   (describe-input-method menu-item "Describe Input Method" describe-input-method)
			   (describe-coding-system menu-item #81# describe-coding-system)
			   (list-character-sets menu-item "List Character Sets" list-character-sets :help "Show table of available character sets")
			   (mule-diag menu-item "Show All Multilingual Settings" mule-diag . #82#)
			   "Mule (Multilingual Environment)"))
		    (showhide-separator #2#)
		    (showhide menu-item #107="Show/Hide"
			      (keymap
			       (showhide-tool-bar menu-item "Tool-bar" toggle-tool-bar-mode-from-frame :help "Turn tool-bar on/off" :visible #84=(display-graphic-p)
						  :button
						  (:toggle menu-bar-positive-p
							   (frame-parameter #83=(menu-bar-frame-for-menubar)
									    'tool-bar-lines)))
			       (menu-bar-mode menu-item "Menu-bar" toggle-menu-bar-mode-from-frame :help "Turn menu-bar on/off" :button
					      (:toggle menu-bar-positive-p
						       (frame-parameter #83# 'menu-bar-lines)))
			       (showhide-tooltip-mode menu-item "Tooltips" tooltip-mode :help "Turn tooltips on/off" :visible
						      (and #84#
							   (fboundp 'x-show-tip))
						      :button
						      (:toggle . tooltip-mode))
			       (showhide-scroll-bar menu-item #87="Scroll-bar"
						    (keymap
						     (none menu-item #88="None" menu-bar-no-scroll-bar :help "Turn off scroll-bar" :visible #84# :button
							   (:radio eq #86=(cdr
									   (assq 'vertical-scroll-bars
										 (frame-parameters)))
								   . #85#))
						     (left menu-item #89="On the Left" menu-bar-left-scroll-bar :help "Scroll-bar on the left side" :visible #84# :button
							   (:radio eq #86# . #94=('left)))
						     (right menu-item #90="On the Right" menu-bar-right-scroll-bar :help "Scroll-bar on the right side" :visible #84# :button
							    (:radio eq #86# . #95=('right)))
						     #87#)
						    . #92=(:visible #84#))
			       (showhide-fringe menu-item #99="Fringe"
						(keymap
						 (none menu-item #88# menu-bar-showhide-fringe-menu-customize-disable :help "Turn off fringe" :visible #84# :button
						       (:radio eq fringe-mode . #91=(0)))
						 (left menu-item #89# menu-bar-showhide-fringe-menu-customize-left :help "Fringe only on the left side" :visible #84# :button
						       (:radio equal fringe-mode
							       '(nil . 0)))
						 (right menu-item #90# menu-bar-showhide-fringe-menu-customize-right :help "Fringe only on the right side" :visible #84# :button
							(:radio equal fringe-mode '#91#))
						 (default menu-item #49# menu-bar-showhide-fringe-menu-customize-reset :help "Default width fringe on both left and right side" :visible #84# :button
						   (:radio eq fringe-mode . #85#))
						 (customize menu-item "Customize Fringe" menu-bar-showhide-fringe-menu-customize :help "Detailed customization of fringe" . #92#)
						 (indicate-empty-lines menu-item "Empty Line Indicators" toggle-indicate-empty-lines :help "Indicate trailing empty lines in fringe, globally" :button
								       (:toggle and
										(default-boundp . #93=('indicate-empty-lines))
										(default-value . #93#)))
						 (showhide-fringe-ind menu-item "Buffer Boundaries"
								      (keymap
								       (none menu-item "No Indicators" menu-bar-showhide-fringe-ind-none :help "Hide all buffer boundary indicators and arrows" :visible #84# :button
									     (:radio eq indicate-buffer-boundaries . #85#))
								       (left menu-item "In Left Fringe" menu-bar-showhide-fringe-ind-left :help "Show buffer boundaries and arrows in left fringe" :visible #84# :button
									     (:radio eq indicate-buffer-boundaries . #94#))
								       (right menu-item "In Right Fringe" menu-bar-showhide-fringe-ind-right :help "Show buffer boundaries and arrows in right fringe" :visible #84# :button
									      (:radio eq indicate-buffer-boundaries . #95#))
								       (box menu-item "Opposite, No Arrows" menu-bar-showhide-fringe-ind-box :help "Show top/bottom indicators in opposite fringes, no arrows" :visible #84# :button
									    (:radio equal indicate-buffer-boundaries '#97=(#96=(top . left)
															       (bottom . right))))
								       (mixed menu-item "Opposite, Arrows Right" menu-bar-showhide-fringe-ind-mixed :help "Show top/bottom indicators in opposite fringes, arrows in right" :visible #84# :button
									      (:radio equal indicate-buffer-boundaries
										      '((t . right)
											#96#)))
								       (customize menu-item "Other (Customize)" menu-bar-showhide-fringe-ind-customize :help "Additional choices available through Custom buffer" :visible #84# :button
										  (:radio not
											  (member indicate-buffer-boundaries
												  '(nil left right #97# . #98=(((t . right)
																#96#))))))
								       "Buffer boundaries")
								      :visible #84# :help "Indicate buffer boundaries in fringe")
						 #99#)
						. #92#)
			       (showhide-speedbar menu-item "Speedbar" speedbar-frame-mode :help "Display a Speedbar quick-navigation frame" :button
						  (:toggle and
							   (boundp . #100=('speedbar-frame))
							   (frame-live-p . #101=((symbol-value . #100#)))
							   (frame-visible-p . #101#)))
			       (datetime-separator #2#)
			       (showhide-date-time menu-item "Time, Load and Mail" display-time-mode :help "Display time, system load averages and mail status in mode line" :button
						   (:toggle and
							    (default-boundp . #102=('display-time-mode))
							    (default-value . #102#)))
			       (showhide-battery menu-item "Battery Status" display-battery-mode :help "Display battery status information in mode line" :button
						 (:toggle and
							  (default-boundp . #103=('display-battery-mode))
							  (default-value . #103#)))
			       (linecolumn-separator #2#)
			       (size-indication-mode menu-item "Size Indication" size-indication-mode :help "Show the size of the buffer in the mode line" :button
						     (:toggle and
							      (default-boundp . #104=('size-indication-mode))
							      (default-value . #104#)))
			       (line-number-mode menu-item "Line Numbers" line-number-mode :help "Show the current line number in the mode line" :button
						 (:toggle and
							  (default-boundp . #105=('line-number-mode))
							  (default-value . #105#)))
			       (column-number-mode menu-item "Column Numbers" column-number-mode :help "Show the current column number in the mode line" :button
						   (:toggle and
							    (default-boundp . #106=('column-number-mode))
							    (default-value . #106#)))
			       #107#))
		    (menu-set-font menu-item "Set Default Font..." menu-set-font :visible
				   (display-multi-font-p)
				   :help "Select a default font")
		    (custom-separator #2#)
		    (save menu-item "Save Options" menu-bar-options-save :help "Save options set from the menu above")
		    (package menu-item "Manage Emacs Packages" package-list-packages :help "Install or uninstall additional Emacs packages")
		    (customize menu-item "Customize Emacs"
			       (keymap
				(customize-themes menu-item "Custom Themes" customize-themes :help "Choose a pre-defined customization theme")
				(customize menu-item "Top-level Customization Group" customize :help "The master group called `Emacs'")
				(customize-browse menu-item "Browse Customization Groups" customize-browse :help "Browse all customization groups")
				(separator-3 #2#)
				(customize-saved menu-item "Saved Options" customize-saved :help "Customize previously saved options")
				(customize-changed-options menu-item "New Options..." customize-changed-options :help "Options added or changed in recent Emacs versions")
				(separator-2 #2#)
				(customize-option menu-item "Specific Option..." customize-option :help "Customize value of specific option")
				(customize-face menu-item "Specific Face..." customize-face :help "Customize attributes of specific face")
				(customize-group menu-item "Specific Group..." customize-group :help "Customize settings of specific group")
				(separator-1 #2#)
				(customize-apropos menu-item "All Settings Matching..." customize-apropos :help "Browse customizable settings matching a regexp or word list")
				(customize-apropos-options menu-item "Options Matching..." customize-apropos-options :help "Browse options matching a regexp or word list")
				(customize-apropos-faces menu-item "Faces Matching..." customize-apropos-faces :help "Browse faces matching a regexp or word list")
				"Customize"))
		    #108#)
	   (buffer #109="Buffers" keymap #109#
		   [("temp.el  *"
		     (nil)
		     lambda nil #110=(interactive)
		     (funcall menu-bar-select-buffer-function #<buffer temp.el>))
		    ("pkb.el  "
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer pkb.el>))
		    ("debug-pkb.el  "
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer debug-pkb.el>))
		    ("my-programming-...ings-to_edit.el  "
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer my-programming-settings-to_edit.el>))
		    ("*Help*  %"
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer *Help*>))
		    ("*scratch*  "
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer *scratch*>))
		    ("*Messages*  *%"
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer *Messages*>))]
		   (frames-separator . #111=(#2#))
		   (frames menu-item "Frames"
			   (keymap "Select Frame"
				   [("temp.el - emacs@mac-lapland"
				     (nil)
				     lambda nil #110#
				     (menu-bar-select-frame #<frame temp.el - emacs@mac-lapland 0x1033f9de8>))
				    ("pkb.el - emacs@mac-lapland"
				     (nil)
				     lambda nil #110#
				     (menu-bar-select-frame #<frame pkb.el - emacs@mac-lapland 0x103025c48>))]))
		   (command-separator . #111#)
		   (next-buffer menu-item "Next Buffer" next-buffer :help "Switch to the \"next\" buffer in a cyclic order")
		   (previous-buffer menu-item "Previous Buffer" previous-buffer :help "Switch to the \"previous\" buffer in a cyclic order")
		   (select-named-buffer menu-item "Select Named Buffer..." switch-to-buffer :help "Prompt for a buffer name, and select that buffer in the current window")
		   (list-all-buffers menu-item "List All Buffers" list-buffers :help "Pop up a window listing all Emacs buffers"))
	   (tools #115="Tools" keymap
		  (grep menu-item "Search Files (Grep)..." grep :help "Search files for strings or regexps (with Grep)")
		  (compile menu-item "Compile..." compile :help "Invoke compiler or Make, view compilation errors")
		  (shell menu-item "Shell Command..." shell-command :help "Invoke a shell command and catch its output")
		  (shell-on-region menu-item "Shell Command on Region..." shell-command-on-region :enable mark-active :help "Pass marked region to a shell command")
		  (gdb menu-item "Debugger (GDB)..." gdb :help "Debug a program from within Emacs with GDB")
		  (ede menu-item "Project Support (EDE)" global-ede-mode :help "Toggle the Emacs Development Environment (Global EDE mode)" :button
		       (:toggle bound-and-true-p global-ede-mode))
		  (semantic menu-item "Source Code Parsers (Semantic)" semantic-mode :help "Toggle automatic parsing in source code buffers (Semantic mode)" :button
			    (:toggle bound-and-true-p semantic-mode))
		  (separator-prog #2#)
		  (spell menu-item "Spell Checking" ispell-menu-map)
		  (separator-spell #2#)
		  (compare menu-item "Compare (Ediff)" menu-bar-ediff-menu)
		  (ediff-merge menu-item "Merge" menu-bar-ediff-merge-menu)
		  (epatch menu-item "Apply Patch" menu-bar-epatch-menu)
		  (separator-compare #2#)
		  (vc menu-item "Version Control" vc-menu-map :filter vc-menu-map-filter)
		  (separator-vc #2#)
		  (gnus menu-item "Read Net News" gnus :help "Read network news groups")
		  (rmail menu-item "Read Mail" menu-bar-read-mail :visible
			 (and read-mail-command
			      (not
			       (eq read-mail-command . #112=('ignore))))
			 :help "Read your mail")
		  (compose-mail menu-item "Compose New Mail" compose-mail :visible
				(and mail-user-agent
				     (not
				      (eq mail-user-agent . #112#)))
				:help "Start writing a new mail message")
		  (directory-search menu-item "Directory Search" eudc-tools-menu)
		  (browse-web menu-item "Browse the Web..." browse-web)
		  (separator-net #2#)
		  (calendar menu-item "Calendar" calendar :help "Invoke the Emacs built-in calendar")
		  (calc menu-item "Programmable Calculator" calc :help "Invoke the Emacs built-in full scientific calculator")
		  (simple-calculator menu-item "Simple Calculator" calculator :help "Invoke the Emacs built-in quick calculator")
		  (separator-encryption-decryption #2#)
		  (encryption-decryption menu-item #113="Encryption/Decryption"
					 (keymap
					  (decrypt-file menu-item "Decrypt File..." epa-decrypt-file :help "Decrypt a file")
					  (encrypt-file menu-item "Encrypt File..." epa-encrypt-file :help "Encrypt a file")
					  (verify-file menu-item "Verify File..." epa-verify-file :help "Verify digital signature of a file")
					  (sign-file menu-item "Sign File..." epa-sign-file :help "Create digital signature of a file")
					  (separator-file #2#)
					  (decrypt-region menu-item "Decrypt Region" epa-decrypt-region :help "Decrypt the current region")
					  (encrypt-region menu-item "Encrypt Region" epa-encrypt-region :help "Encrypt the current region")
					  (verify-region menu-item "Verify Region" epa-verify-region :help "Verify digital signature of the current region")
					  (sign-region menu-item "Sign Region" epa-sign-region :help "Create digital signature of the current region")
					  (separator-keys #2#)
					  (list-keys menu-item "List Keys" epa-list-keys :help "Browse your public keyring")
					  (import-keys menu-item "Import Keys from File..." epa-import-keys :help "Import public keys from a file")
					  (import-keys-region menu-item "Import Keys from Region" epa-import-keys-region :help "Import public keys from the current region")
					  (export-keys menu-item "Export Keys" epa-export-keys :help "Export public keys to a file")
					  (insert-keys menu-item "Insert Keys" epa-insert-keys :help "Insert public keys after the current point")
					  #113#))
		  (separator-games #2#)
		  (games menu-item #114="Games"
			 (keymap
			  (5x5 menu-item "5x5" 5x5 :help "Fill in all the squares on a 5x5 board")
			  (adventure menu-item "Adventure" dunnet :help "Dunnet, a text Adventure game for Emacs")
			  (black-box menu-item "Blackbox" blackbox :help "Find balls in a black box by shooting rays")
			  (bubbles menu-item "Bubbles" bubbles :help "Remove all bubbles using the fewest moves")
			  (gomoku menu-item "Gomoku" gomoku :help "Mark 5 contiguous squares (like tic-tac-toe)")
			  (hanoi menu-item "Towers of Hanoi" hanoi :help "Watch Towers-of-Hanoi puzzle solved by Emacs")
			  (land menu-item "Landmark" landmark :help "Watch a neural-network robot learn landmarks")
			  (life menu-item "Life" life :help "Watch how John Conway's cellular automaton evolves")
			  (mult menu-item "Multiplication Puzzle" mpuz :help "Exercise brain with multiplication")
			  (pong menu-item "Pong" pong :help "Bounce the ball to your opponent")
			  (snake menu-item "Snake" snake :help "Move snake around avoiding collisions")
			  (solitaire menu-item "Solitaire" solitaire :help "Get rid of all the stones")
			  (tetris menu-item "Tetris" tetris :help "Falling blocks game")
			  (zone menu-item "Zone Out" zone :help "Play tricks with Emacs display when Emacs is idle")
			  #114#))
		  #115#)
	   (mouse-1 . tmm-menubar-mouse))
 (f4 . kmacro-end-or-call-macro)
 (f3 . kmacro-start-macro-or-insert-counter)
 (C-M-end . end-of-defun)
 (C-M-home . beginning-of-defun)
 (C-M-down . down-list)
 (C-M-up . backward-up-list)
 (C-M-right . forward-sexp)
 (C-M-left . backward-sexp)
 (S-delete . kill-region)
 (C-backspace . backward-kill-word)
 (C-delete . kill-word)
 (C-left . left-word)
 (C-right . right-word)
 (M-left . left-word)
 (M-right . right-word)
 (mouse-movement . ignore)
 (deletechar . delete-forward-char)
 (deleteline . kill-line)
 (open . find-file)
 (redo . repeat-complex-command)
 (undo . undo)
 (S-insert . yank)
 (C-insert . kill-ring-save)
 (insert . overwrite-mode)
 (M-end . end-of-buffer-other-window)
 (C-end . end-of-buffer)
 (end . end-of-buffer)
 (M-prior . scroll-other-window-down)
 (M-next . scroll-other-window)
 (C-next . scroll-left)
 (C-prior . scroll-right)
 (C-down . forward-paragraph)
 (C-up . backward-paragraph)
 (next . scroll-up-command)
 (prior . scroll-down-command)
 (down . next-line)
 (right . right-char)
 (up . previous-line)
 (left . left-char)
 (M-home . beginning-of-buffer-other-window)
 (C-home . beginning-of-buffer)
 (home . beginning-of-buffer)
 (C-S-backspace . kill-whole-line)
 (menu . execute-extended-command)
 (67108896 . set-mark-command)
 (67108909 . negative-argument)
 (67108921 . digit-argument)
 (67108920 . digit-argument)
 (67108919 . digit-argument)
 (67108918 . digit-argument)
 (67108917 . digit-argument)
 (67108916 . digit-argument)
 (67108915 . digit-argument)
 (67108914 . digit-argument)
 (67108913 . digit-argument)
 (67108912 . digit-argument)
 (XF86Back . previous-buffer)
 (XF86Forward . next-buffer)
 (67108911 . undo)
 (make-frame-visible . ignore-event)
 (iconify-frame . ignore-event)
 (delete-frame . handle-delete-frame)
 (select-window . handle-select-window)
 (switch-frame . handle-switch-frame))






  
  For all components lists, except 
  If the last element of CPN-I is not a 
  
  Except in the last component, 

  The last element of the list may instead be an empty list, a list with one
    integer or a vector with one integer. These indicate where the next keymap
    is with respect to the event binding at the location given by the earlier
    part of the list. If the last component is:


 (an empty list is the same as `(0)')

(pptcb (current-local-map))
(keymap
 (3 keymap
    (100 . duplicate-line))
 (menu-bar keymap
	   (emacs-lisp #4="Emacs-Lisp" keymap
		       (indent-line menu-item "Indent Line" lisp-indent-line)
		       (indent-region menu-item "Indent Region" indent-region :help "Indent each nonblank line in the region" . #1=(:enable mark-active))
		       (comment-region menu-item "Comment Out Region" comment-region :help "Comment or uncomment each line in the region" . #1#)
		       (separator-format #2="--")
		       (eval-sexp menu-item "Evaluate Last S-expression" eval-last-sexp :help "Evaluate sexp before point; print value in echo area")
		       (eval-region menu-item "Evaluate Region" eval-region :help "Execute the region as Lisp code" . #1#)
		       (eval-buffer menu-item "Evaluate Buffer" eval-buffer :help "Execute the current buffer as Lisp code")
		       (ielm menu-item "Interactive Expression Evaluation" ielm :help "Interactively evaluate Emacs Lisp expressions")
		       (separator-eval #2#)
		       (byte-compile menu-item "Byte-compile This File" emacs-lisp-byte-compile :help "Byte compile the file containing the current buffer")
		       (emacs-byte-compile-and-load menu-item "Byte-compile and Load" emacs-lisp-byte-compile-and-load :help "Byte-compile the current file (if it has changed), then load compiled code")
		       (byte-recompile menu-item "Byte-recompile Directory..." byte-recompile-directory :help "Recompile every `.el' file in DIRECTORY that needs recompilation")
		       (disas menu-item "Disassemble Byte Compiled Object..." disassemble :help "Print disassembled code for OBJECT in a buffer")
		       (separator-byte #2#)
		       (edebug-defun menu-item "Instrument Function for Debugging" edebug-defun :help "Evaluate the top level form point is in, stepping through with Edebug" :keys "C-u C-M-x")
		       (lint "Linting" keymap
			     (lint-d menu-item "Lint Defun" elint-defun :help "Lint the function at point")
			     (lint-b menu-item "Lint Buffer" elint-current-buffer :help "Lint the current buffer")
			     (lint-f menu-item "Lint File..." elint-file :help "Lint a file")
			     (lint-di menu-item "Lint Directory..." elint-directory :help "Lint a directory"))
		       (profiling "Profiling" keymap
				  (prof-natprof-start menu-item "Start Native Profiler..." profiler-start :help "Start recording profiling information")
				  (prof-natprof-report menu-item "Show Profiler Report" profiler-report :help "Show the current profiler report" . #3=(:enable
																		       (and
																			(featurep 'profiler)
																			(profiler-running-p))))
				  (prof-natprof-stop menu-item "Stop Native Profiler" profiler-stop :help "Stop recording profiling information" . #3#)
				  (sep-natprof #2#)
				  (prof-func menu-item "Instrument Function..." elp-instrument-function :help "Instrument a function for profiling")
				  (prof-pack menu-item "Instrument Package..." elp-instrument-package :help "Instrument for profiling all function that start with a prefix")
				  (prof-res menu-item "Show Profiling Results" elp-results :help "Display current profiling results")
				  (prof-resfunc menu-item "Reset Counters for Function..." elp-reset-function :help "Reset the profiling information for a function")
				  (prof-resall menu-item "Reset Counters for All Functions" elp-reset-all :help "Reset the profiling information for all functions being profiled")
				  (sep-rem #2#)
				  (prof-restfunc menu-item "Remove Instrumentation for Function..." elp-restore-function :help "Restore an instrumented function to its original definition")
				  (prof-restall menu-item "Remove Instrumentation for All Functions" elp-restore-all :help "Restore the original definitions of all functions being profiled"))
		       (tracing "Tracing" keymap
				(tr-f menu-item "Trace Function..." trace-function :help "Trace the function given as an argument")
				(tr-q menu-item "Trace Function Quietly..." trace-function-background :help "Trace the function with trace output going quietly to a buffer")
				(tr-sep #2#)
				(tr-uf menu-item "Untrace Function..." untrace-function :help "Untrace function, and possibly activate all remaining advice")
				(tr-a menu-item "Untrace All" untrace-all :help "Untrace all currently traced functions"))
		       (re-builder menu-item "Construct Regexp" re-builder :help "Construct a regexp interactively")
		       (checkdoc menu-item "Check Documentation Strings" checkdoc :help "Check documentation strings for style requirements")
		       (eldoc menu-item "Auto-Display Documentation Strings" eldoc-mode :button
			      (:toggle bound-and-true-p eldoc-mode)
			      :help "Display the documentation string for the item under cursor")
		       #4#))
 (27 keymap
     (17 . indent-pp-sexp)
     (24 . eval-defun)
     (9 . completion-at-point))
 #4# keymap
 (127 . backward-delete-char-untabify)
 (27 keymap
     (17 . indent-sexp))
 keymap
 (27 keymap
     (17 . prog-indent-sexp)))


(keymap
 (3 keymap
    (100 . duplicate-line))
 (27 keymap
     (17 . indent-pp-sexp)
     (24 . eval-defun)
     (9 . completion-at-point))
 #4# keymap
 (127 . backward-delete-char-untabify)
 (27 keymap
     (17 . indent-sexp))
 keymap
 (27 keymap
     (17 . prog-indent-sexp)) . query-replace-map)

(setq seq [1 2 3 4 5 5])
(setq my-list (list 1 2 3 4 5 5))

(setf (elt seq (1- (length seq))) 8)
(setf (last my-list) 8)
seq


(cl-symbol-macrolet ((last-elt (elt my-list (1- (length my-list)))))
  (setf last-elt 2))

(setf (aref seq 9) 2)

(pptcb (current-global-map))
(keymap
 #^[nil nil keymap #1=
	#^^[3 0 set-mark-command move-beginning-of-line backward-char mode-specific-command-prefix delete-char move-end-of-line forward-char keyboard-quit help-command indent-for-tab-command electric-newline-and-maybe-indent kill-line recenter-top-bottom newline next-line open-line previous-line quoted-insert isearch-backward isearch-forward transpose-chars universal-argument scroll-up-command kill-region Control-X-prefix yank t ESC-prefix toggle-input-method abort-recursive-edit nil undo self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command delete-backward-char]
	#^^[1 0
	      #^^[2 0 #1# self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command]
	      self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command]
	self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command self-insert-command]
 (remap keymap
	(find-file-other-window)
	(find-file))
 (s-f1 lambda nil
       (interactive)
       (set-frame-width
	(selected-frame)
	90))
 (f6 . imenu)
 (f5 keymap
     (51 . bookmark-menu-list)
     (50 . bookmark-jump)
     (49 . bookmark-set))
 (event)
 (s-down-mouse-3 . imenu)
 (8388670 . other-frame-back)
 (8388713 . overwrite-mode)
 (8388668 . other-frame)
 (S-mouse-3 . kmacro-end-call-mouse)
 (C-wheel-down . mwheel-scroll)
 (S-wheel-down . mwheel-scroll)
 (wheel-down . mwheel-scroll)
 (C-wheel-up . mwheel-scroll)
 (S-wheel-up . mwheel-scroll)
 (wheel-up . mwheel-scroll)
 (C-M-drag-n-drop . ns-drag-n-drop-as-text-other-frame)
 (M-drag-n-drop . ns-drag-n-drop-as-text)
 (C-drag-n-drop . ns-drag-n-drop-other-frame)
 (drag-n-drop . ns-drag-n-drop)
 (mac-change-input-method . mac-change-input-method)
 (ns-show-prefs . customize)
 (ns-toggle-toolbar . ns-toggle-toolbar)
 (ns-new-frame . make-frame)
 (ns-spi-service-call . ns-spi-service-call)
 (ns-open-file-line . ns-open-file-select-line)
 (ns-change-font . ns-respond-to-change-font)
 (ns-open-temp-file .
		    [ns-open-file])
 (ns-open-file . ns-find-file)
 (ns-power-off . save-buffers-kill-emacs)
 (S-mouse-1 . mouse-save-then-kill)
 (kp-next . scroll-up-command)
 (kp-prior . scroll-down-command)
 (kp-end . end-of-buffer)
 (kp-home . beginning-of-buffer)
 (s-left . ns-prev-frame)
 (s-right . ns-next-frame)
 (s-kp-bar . shell-command-on-region)
 (8388732 . shell-command-on-region)
 (8388730 . undo)
 (8388729 . ns-paste-secondary)
 (8388728 . kill-region)
 (8388727 . delete-frame)
 (8388726 . yank)
 (8388725 . revert-buffer)
 (8388724 . ns-popup-font-panel)
 (8388723 . save-buffer)
 (8388721)
 (8388720 . ns-print-buffer)
 (8388719 . ns-open-file-using-panel)
 (8388718 . make-frame)
 (8388717)
 (8388716 . goto-line)
 (8388715 . kill-this-buffer)
 (8388714 . exchange-point-and-mark)
 (8388680 . ns-do-hide-others)
 (8388712 . ns-do-hide-emacs)
 (8388711)
 (8388710 . isearch-forward)
 (8388709)
 (8388708)
 (8388707 . ns-copy-including-secondary)
 (8388705 . mark-whole-buffer)
 (8388691 . ns-write-file-using-panel)
 (8388685 . manual-entry)
 (8388684 . shell-command)
 (8388677 . edit-abbrevs)
 (8388676 . dired)
 (8388675 . ns-popup-color-panel)
 (8388646 . kill-this-buffer)
 (8388702 . kill-some-buffers)
 (8388671 . info)
 (8388666 . ispell)
 (8388653 . center-line)
 (8388734 . ns-prev-frame)
 (8388704 . other-frame)
 (8388647 . next-multiframe-window)
 (8388652 . customize)
 (tool-bar menu-item "tool bar" ignore :filter tool-bar-make-keymap)
 (f10 . menu-bar-open)
 (bottom-divider keymap
		 (down-mouse-1 . mouse-drag-mode-line))
 (right-divider keymap
		(down-mouse-1 . mouse-drag-vertical-line))
 (vertical-line keymap
		(mouse-1 . mouse-select-window)
		(down-mouse-1 . mouse-drag-vertical-line)
		(C-mouse-2 . mouse-split-window-vertically))
 (vertical-scroll-bar keymap
		      (drag-mouse-1)
		      (down-mouse-1 . ns-handle-scroll-bar-event)
		      (mouse-1)
		      (C-mouse-2 . mouse-split-window-vertically))
 (header-line keymap
	      (mouse-1 . mouse-select-window)
	      (down-mouse-1 . mouse-drag-header-line))
 (mode-line keymap
	    (C-mouse-2 . mouse-split-window-horizontally)
	    (mouse-3 . mouse-delete-window)
	    (mouse-2 . mouse-delete-other-windows)
	    (down-mouse-1 . mouse-drag-mode-line)
	    (drag-mouse-1 . mouse-select-window)
	    (mouse-1 . mouse-select-window))
 (C-down-mouse-3 menu-item "Menu Bar" ignore :filter
		 (lambda
		   (_)
		   (if
		       (zerop
			(or
			 (frame-parameter nil 'menu-bar-lines)
			 0))
		       (mouse-menu-bar-map)
		     (mouse-menu-major-mode-map))))
 (S-down-mouse-1)
 (C-down-mouse-1 . mouse-buffer-menu)
 (mouse-3 . mouse-save-then-kill)
 (mouse-2 . mouse-yank-primary)
 (triple-mouse-1 . mouse-set-point)
 (double-mouse-1 . mouse-set-point)
 (drag-mouse-1 . mouse-set-region)
 (mouse-1 . mouse-set-point)
 (down-mouse-1 . mouse-drag-region)
 (M-mouse-2 . mouse-yank-secondary)
 (M-mouse-3 . mouse-secondary-save-then-kill)
 (M-down-mouse-1 . mouse-drag-secondary)
 (M-drag-mouse-1 . mouse-set-secondary)
 (M-mouse-1 . mouse-start-secondary)
 (C-down-mouse-2 . facemenu-menu)
 (M-f10 . toggle-frame-maximized)
 (f11 . toggle-frame-fullscreen)
 (compose-last-chars . compose-last-chars)
 (f1 . help-command)
 (help . help-command)
 (f2 . 2C-command)
 (menu-bar keymap
	   (help-menu #9="Help" keymap
		      (emacs-tutorial menu-item "Emacs Tutorial" help-with-tutorial :help "Learn how to use Emacs")
		      (emacs-tutorial-language-specific menu-item "Emacs Tutorial (choose language)..." help-with-tutorial-spec-language :help "Learn how to use Emacs (choose a language)")
		      (emacs-faq menu-item "Emacs FAQ" view-emacs-FAQ :help "Frequently asked (and answered) questions about Emacs")
		      (emacs-news menu-item "Emacs News" view-emacs-news :help "New features of this version")
		      (emacs-known-problems menu-item "Emacs Known Problems" view-emacs-problems :help "Read about known problems with Emacs")
		      (emacs-manual-bug menu-item "How to Report a Bug" info-emacs-bug :help "Read about how to report an Emacs bug")
		      (send-emacs-bug-report menu-item "Send Bug Report..." report-emacs-bug :help "Send e-mail to Emacs maintainers")
		      (emacs-psychotherapist menu-item "Emacs Psychotherapist" doctor :help "Our doctor will help you feel better")
		      (sep1 #2="--")
		      (search-documentation menu-item #3="Search Documentation"
					    (keymap
					     (emacs-terminology menu-item "Emacs Terminology" search-emacs-glossary :help "Display the Glossary section of the Emacs manual")
					     (lookup-subject-in-emacs-manual menu-item "Look Up Subject in User Manual..." emacs-index-search :help "Find description of a subject in Emacs User manual")
					     (lookup-subject-in-elisp-manual menu-item "Look Up Subject in ELisp Manual..." elisp-index-search :help "Find description of a subject in Emacs Lisp manual")
					     (lookup-key-in-manual menu-item "Look Up Key in User Manual..." Info-goto-emacs-key-command-node :help "Display manual section that describes a key")
					     (lookup-command-in-manual menu-item "Look Up Command in User Manual..." Info-goto-emacs-command-node :help "Display manual section that describes a command")
					     (sep1 #2#)
					     (find-commands-by-name menu-item "Find Commands by Name..." apropos-command :help "Find commands whose names match a regexp")
					     (find-options-by-name menu-item "Find Options by Name..." apropos-user-option :help "Find user options whose names match a regexp")
					     (find-option-by-value menu-item "Find Options by Value..." apropos-value :help "Find variables whose values match a regexp")
					     (find-any-object-by-name menu-item "Find Any Object by Name..." apropos :help "Find symbols of any kind whose names match a regexp")
					     (search-documentation-strings menu-item "Search Documentation Strings..." apropos-documentation :help "Find functions and variables whose doc strings match a regexp")
					     #3#))
		      (describe menu-item #7="Describe"
				(keymap
				 (describe-mode menu-item "Describe Buffer Modes" describe-mode :help "Describe this buffer's major and minor mode")
				 (describe-key-1 menu-item "Describe Key or Mouse Operation..." describe-key :help "Display documentation of command bound to a key, a click, or a menu-item")
				 (describe-function menu-item "Describe Function..." describe-function :help "Display documentation of function/command")
				 (describe-variable menu-item "Describe Variable..." describe-variable :help "Display documentation of variable/option")
				 (describe-face menu-item "Describe Face..." describe-face :help "Display the properties of a face")
				 (describe-package menu-item "Describe Package..." describe-package :help "Display documentation of a Lisp package")
				 (describe-current-display-table menu-item "Describe Display Table" describe-current-display-table :help "Describe the current display table")
				 (list-keybindings menu-item "List Key Bindings" describe-bindings :help "Display all current key bindings (keyboard shortcuts)")
				 (separator-desc-mule #2#)
				 (describe-language-environment menu-item #4="Describe Language Environment" #80=(keymap
														  (Default menu-item #49="Default" describe-specified-language-support)
														  #4#
														  (Chinese #51="Chinese" . describe-chinese-environment-map)
														  (Cyrillic #52="Cyrillic" . describe-cyrillic-environment-map)
														  (Indian #53="Indian" . describe-indian-environment-map)
														  (Sinhala #54="Sinhala" . describe-specified-language-support)
														  (English #55="English" . describe-specified-language-support)
														  (ASCII #56="ASCII" . describe-specified-language-support)
														  (Ethiopic #57="Ethiopic" . describe-specified-language-support)
														  (European #58="European" . describe-european-environment-map)
														  (Turkish #59="Turkish" . describe-specified-language-support)
														  (Greek #60="Greek" . describe-specified-language-support)
														  (Hebrew #61="Hebrew" . describe-specified-language-support)
														  (Windows-1255 #62="Windows-1255" . describe-specified-language-support)
														  (Japanese #63="Japanese" . describe-specified-language-support)
														  (Korean #64="Korean" . describe-specified-language-support)
														  (Lao #65="Lao" . describe-specified-language-support)
														  (TaiViet #66="TaiViet" . describe-specified-language-support)
														  (Thai #67="Thai" . describe-specified-language-support)
														  (Tibetan #68="Tibetan" . describe-specified-language-support)
														  (Vietnamese #69="Vietnamese" . describe-specified-language-support)
														  (IPA #70="IPA" . describe-specified-language-support)
														  (Arabic #71="Arabic" . describe-specified-language-support)
														  (Persian #72="Persian" . describe-specified-language-support)
														  (UTF-8 #73="UTF-8" . describe-specified-language-support)
														  (Khmer #74="Khmer" . describe-specified-language-support)
														  (Burmese #75="Burmese" . describe-specified-language-support)))
				 (describe-input-method menu-item "Describe Input Method..." describe-input-method :visible #5=(default-value 'enable-multibyte-characters)
							:help "Keyboard layout for specific input method")
				 (describe-coding-system menu-item #81="Describe Coding System..." describe-coding-system . #6=(:visible . #79=(#5#)))
				 (describe-coding-system-briefly menu-item "Describe Coding System (Briefly)" describe-current-coding-system-briefly . #6#)
				 (mule-diag menu-item "Show All of Mule Status" mule-diag :visible #5# . #82=(:help "Display multilingual environment settings"))
				 #7#))
		      (emacs-manual menu-item "Read the Emacs Manual" info-emacs-manual :help "Full documentation of Emacs features")
		      (more-manuals menu-item #8="More Manuals"
				    (keymap
				     (emacs-lisp-intro menu-item "Introduction to Emacs Lisp" menu-bar-read-lispintro :help "Read the Introduction to Emacs Lisp Programming")
				     (emacs-lisp-reference menu-item "Emacs Lisp Reference" menu-bar-read-lispref :help "Read the Emacs Lisp Reference manual")
				     (other-manuals menu-item "All Other Manuals (Info)" Info-directory :help "Read any of the installed manuals")
				     (lookup-subject-in-all-manuals menu-item "Lookup Subject in all Manuals..." info-apropos :help "Find description of a subject in all installed manuals")
				     (order-emacs-manuals menu-item "Ordering Manuals" view-order-manuals :help "How to order manuals from the Free Software Foundation")
				     (sep2 #2#)
				     (man menu-item "Read Man Page..." manual-entry :help "Man-page docs for external commands and libraries")
				     #8#))
		      (find-emacs-packages menu-item "Search Built-in Packages" finder-by-keyword :help "Find built-in packages and features by keyword")
		      (external-packages menu-item "Finding Extra Packages" view-external-packages :help "How to get more Lisp packages for use in Emacs")
		      (sep2 #2#)
		      (getting-new-versions menu-item "Getting New Versions" describe-distribution :help "How to get the latest version of Emacs")
		      (describe-copying menu-item "Copying Conditions" describe-copying :help "Show the Emacs license (GPL)")
		      (describe-no-warranty menu-item "(Non)Warranty" describe-no-warranty :help "Explain that Emacs has NO WARRANTY")
		      (sep4 #2#)
		      (about-emacs menu-item "About Emacs" about-emacs :help "Display version number, copyright info, and basic help")
		      (about-gnu-project menu-item "About GNU" describe-gnu-project :help "About the GNU System, GNU Project, and GNU/Linux")
		      #9#)
	   (file #17="File" keymap
		 (new-file menu-item "Visit New File..." find-file :enable #10=(menu-bar-non-minibuffer-window-p)
			   :help "Specify a new file's name, to edit the file")
		 (open-file menu-item "Open File..." menu-find-file-existing :enable #10# :help "Read an existing file into an Emacs buffer")
		 (dired menu-item "Open Directory..." dired :enable #10# :help "Read a directory, to operate on its files")
		 (insert-file menu-item "Insert File..." insert-file :enable #10# :help "Insert another file into current buffer")
		 (kill-buffer menu-item "Close" kill-this-buffer :enable
			      (kill-this-buffer-enabled-p)
			      :help "Discard (kill) current buffer")
		 (separator-save #2#)
		 (save-buffer menu-item "Save" save-buffer :enable
			      (and #12=(buffer-modified-p)
				   (buffer-file-name)
				   . #11=(#10#))
			      :help "Save current buffer to its file")
		 (write-file menu-item "Save As..." write-file :enable #16=(and #15=(menu-bar-menu-frame-live-and-visible-p)
										. #11#)
			     :help "Write current buffer to another file")
		 (revert-buffer menu-item "Revert Buffer" revert-buffer :enable
				(or
				 (not
				  (eq revert-buffer-function 'revert-buffer--default))
				 (not
				  (eq revert-buffer-insert-file-contents-function 'revert-buffer-insert-file-contents--default-function))
				 (and buffer-file-number
				      (or #12#
					  (not
					   (verify-visited-file-modtime . #78=((current-buffer)))))))
				:help "Re-read current buffer from its file")
		 (recover-session menu-item "Recover Crashed Session" recover-session :enable
				  (and auto-save-list-file-prefix
				       (file-directory-p #13=(file-name-directory . #14=(auto-save-list-file-prefix)))
				       (directory-files #13# nil
							(concat "\\`"
								(regexp-quote
								 (file-name-nondirectory . #14#)))
							t))
				  :help "Recover edits from a crashed session")
		 (separator-print #2#)
		 (print-buffer menu-item "Print Buffer" print-buffer :enable #15# :help "Print current buffer with page headings")
		 (print-region menu-item "Print Region" print-region :enable mark-active :help "Print region between mark and current position")
		 (ps-print-buffer-faces menu-item "PostScript Print Buffer" ps-print-buffer-with-faces :enable #15# :help "Pretty-print current buffer to PostScript printer")
		 (ps-print-region-faces menu-item "PostScript Print Region" ps-print-region-with-faces :enable mark-active :help "Pretty-print marked region to PostScript printer")
		 (ps-print-buffer menu-item "PostScript Print Buffer (B+W)" ps-print-buffer :enable #15# :help "Pretty-print current buffer in black and white to PostScript printer")
		 (ps-print-region menu-item "PostScript Print Region (B+W)" ps-print-region :enable mark-active :help "Pretty-print marked region in black and white to PostScript printer")
		 (separator-window #2#)
		 (new-window-below menu-item "New Window Below" split-window-below :enable #16# :help "Make new window below selected one")
		 (new-window-on-right menu-item "New Window on Right" split-window-right :enable #16# :help "Make new window on right of selected one")
		 (one-window menu-item "Remove Other Windows" delete-other-windows :enable
			     (not
			      (one-window-p t . #85=(nil)))
			     :help "Make selected window fill whole frame")
		 (separator-frame #2#)
		 (make-frame menu-item "New Frame" make-frame-command :visible
			     (fboundp 'make-frame-command)
			     :help "Open a new frame")
		 (make-frame-on-display menu-item "New Frame on Display..." make-frame-on-display :visible
					(fboundp 'make-frame-on-display)
					:help "Open a new frame on another display")
		 (delete-this-frame menu-item "Delete Frame" delete-frame :visible
				    (fboundp 'delete-frame)
				    :enable
				    (delete-frame-enabled-p)
				    :help "Delete currently selected frame")
		 (separator-exit #2#)
		 (exit-emacs menu-item "Quit" save-buffers-kill-terminal :help "Save unsaved buffers, then exit")
		 #17#)
	   (edit #30="Edit" keymap
		 (undo menu-item "Undo" undo :enable
		       (and #19=(not buffer-read-only)
			    (not
			     (eq t . #18=(buffer-undo-list)))
			    (if
				(eq last-command 'undo)
				(listp pending-undo-list)
			      (consp . #18#)))
		       :help "Undo last operation")
		 (separator-undo #2#)
		 (cut menu-item "Cut" kill-region :enable #22=(and mark-active . #20=(#19#))
		      :help "Cut (kill) text in region between mark and current position")
		 (copy menu-item "Copy" ns-copy-including-secondary :enable mark-active :help "Copy text in region between mark and current position" :keys "\\[ns-copy-including-secondary]")
		 (paste menu-item "Paste" yank :enable
			(and
			 (or
			  (and
			   (fboundp 'x-selection-exists-p)
			   (x-selection-exists-p 'CLIPBOARD))
			  (if
			      (featurep 'ns)
			      #21=(cdr yank-menu)
			      kill-ring))
			 . #20#)
			:help "Paste (yank) text most recently cut/copied")
		 (select-paste menu-item "Select and Paste" yank-menu :enable
			       (and #21# . #20#)
			       :help "Choose a string from the kill ring and paste it")
		 (clear menu-item "Clear" delete-region :enable #22# :help "Delete the text in region between mark and current position")
		 (mark-whole-buffer menu-item "Select All" mark-whole-buffer :help "Mark the whole buffer for a subsequent cut/copy")
		 (separator-search #2#)
		 (search menu-item #25="Search"
			 (keymap
			  (search-forward menu-item "String Forward..." nonincremental-search-forward :help "Search forward for a string")
			  (search-backward menu-item "String Backwards..." nonincremental-search-backward :help "Search backwards for a string")
			  (re-search-forward menu-item "Regexp Forward..." nonincremental-re-search-forward :help "Search forward for a regular expression")
			  (re-search-backward menu-item "Regexp Backwards..." nonincremental-re-search-backward :help "Search backwards for a regular expression")
			  (separator-repeat-search #2#)
			  (repeat-search-fwd menu-item "Repeat Forward" nonincremental-repeat-search-forward :enable #23=(or
															  (and
															   (eq menu-bar-last-search-type 'string)
															   search-ring)
															  (and
															   (eq menu-bar-last-search-type 'regexp)
															   regexp-search-ring))
					     :help "Repeat last search forward")
			  (repeat-search-back menu-item "Repeat Backwards" nonincremental-repeat-search-backward :enable #23# :help "Repeat last search backwards")
			  (separator-tag-search #2#)
			  (tags-srch menu-item "Search Tagged Files..." tags-search :help "Search for a regexp in all tagged files")
			  (tags-continue menu-item "Continue Tags Search" tags-loop-continue :help "Continue last tags search operation")
			  (separator-tag-isearch #2#)
			  (i-search menu-item #24="Incremental Search"
				    (keymap
				     (isearch-forward menu-item "Forward String..." isearch-forward :help "Search forward for a string as you type it")
				     (isearch-backward menu-item "Backward String..." isearch-backward :help "Search backwards for a string as you type it")
				     (isearch-forward-regexp menu-item "Forward Regexp..." isearch-forward-regexp :help "Search forward for a regular expression as you type it")
				     (isearch-backward-regexp menu-item "Backward Regexp..." isearch-backward-regexp :help "Search backwards for a regular expression as you type it")
				     #24#))
			  #25#))
		 (replace menu-item #26="Replace"
			  (keymap
			   (query-replace menu-item "Replace String..." query-replace :enable #19# :help "Replace string interactively, ask about each occurrence")
			   (query-replace-regexp menu-item "Replace Regexp..." query-replace-regexp :enable #19# :help "Replace regular expression interactively, ask about each occurrence")
			   (separator-replace-tags #2#)
			   (tags-repl menu-item "Replace in Tagged Files..." tags-query-replace :help "Interactively replace a regexp in all tagged files")
			   (tags-repl-continue menu-item "Continue Replace" tags-loop-continue :help "Continue last tags replace operation")
			   #26#))
		 (goto menu-item #29="Go To"
		       (keymap
			(go-to-line menu-item "Goto Line..." goto-line :help "Read a line number and go to that line")
			(go-to-pos menu-item "Goto Buffer Position..." goto-char :help "Read a number N and go to buffer position N")
			(beg-of-buf menu-item "Goto Beginning of Buffer" beginning-of-buffer)
			(end-of-buf menu-item "Goto End of Buffer" end-of-buffer)
			(separator-tags #2#)
			(find-tag menu-item "Find Tag..." find-tag :help "Find definition of function or variable")
			(find-tag-otherw menu-item "Find Tag in Other Window..." find-tag-other-window :help "Find function/variable definition in another window")
			(next-tag menu-item "Find Next Tag" menu-bar-next-tag :enable #28=(and
											   (boundp 'tags-location-ring)
											   (not
											    (ring-empty-p . #27=(tags-location-ring))))
				  :help "Find next function/variable matching last tag name")
			(next-tag-otherw menu-item "Next Tag in Other Window" menu-bar-next-tag-other-window :enable #28# :help "Find next function/variable matching last tag name in another window")
			(apropos-tags menu-item "Tags Apropos..." tags-apropos :help "Find function/variables whose names match regexp")
			(separator-tag-file #2#)
			(set-tags-name menu-item "Set Tags File Name..." visit-tags-table :help "Tell Tags commands which tag table file to use")
			#29#))
		 (bookmark menu-item "Bookmarks" menu-bar-bookmark-map)
		 (separator-bookmark #2#)
		 (fill menu-item "Fill" fill-region :enable #22# :help "Fill text in region to fit between left and right margin")
		 (spell menu-item "Spell" ispell-menu-map)
		 (props menu-item "Text Properties" facemenu-menu)
		 #30#)
	   (options #108="Options" keymap
		    (transient-mark-mode menu-item "Highlight Active Region" transient-mark-mode :enable
					 (not . #40=(cua-mode))
					 :help "Make text in active region stand out in color (Transient Mark mode)" :button
					 (:toggle and
						  (default-boundp . #31=('transient-mark-mode))
						  (default-value . #31#)))
		    (highlight-paren-mode menu-item "Highlight Matching Parentheses" show-paren-mode :help "Highlight matching/mismatched parentheses at cursor (Show Paren mode)" :button
					  (:toggle and
						   (default-boundp . #32=('show-paren-mode))
						   (default-value . #32#)))
		    (highlight-separator #2#)
		    (line-wrapping menu-item "Line Wrapping in This Buffer"
				   (keymap
				    (window-wrap menu-item "Wrap at Window Edge"
						 #[nil "\203 \300\303!\210\304\n\205 \305\306!\207"
						       [visual-line-mode word-wrap truncate-lines 0 nil toggle-truncate-lines -1]
						       2 nil nil]
						 :help "Wrap long lines at window edge" :button
						 (:radio and #36=(null truncate-lines)
							 #33=(not . #34=((truncated-partial-width-window-p)))
							 (not . #37=(word-wrap)))
						 . #35=(:visible #15# :enable #33#))
				    (truncate menu-item "Truncate Long Lines"
					      #[nil "\203 \300\302!\210\303\304\305!\207"
						    [visual-line-mode word-wrap 0 nil toggle-truncate-lines 1]
						    2 nil nil]
					      :help "Truncate long lines at window edge" :button
					      (:radio or truncate-lines . #34#)
					      . #35#)
				    (word-wrap menu-item "Word Wrap (Visual Line mode)"
					       #[nil "\204 \300\301!\210\302\303!\207"
						     [visual-line-mode 1 message "Visual-Line mode enabled"]
						     2 nil nil]
					       :help "Wrap long lines at word boundaries" :button
					       (:radio and #36# #33# . #37#)
					       :visible #15#)
				    "Line Wrapping"))
		    (case-fold-search menu-item "Ignore Case for Search" toggle-case-fold-search :help "Ignore letter-case in search commands" :button
				      (:toggle and
					       (default-boundp . #38=('case-fold-search))
					       (default-value . #38#)))
		    (cua-emulation-mode menu-item "Shift movement mark region (CUA)" cua-mode :visible
					(and #42=(boundp 'cua-enable-cua-keys)
					     (not . #39=(cua-enable-cua-keys)))
					:help "Use shifted movement keys to set and extend the region" . #43=(:button
													      (:toggle and
														       (default-boundp . #41=('cua-mode))
														       (default-value . #41#))))
		    (cua-mode menu-item "Use CUA Keys (Cut/Paste with C-x/C-c/C-v)" cua-mode :visible
			      (or
			       (not #42#)
			       . #39#)
			      :help "Use C-z/C-x/C-c/C-v keys for undo/cut/copy/paste" . #43#)
		    (edit-options-separator #2#)
		    (uniquify menu-item "Use Directory Names in Buffer Names" toggle-uniquify-buffer-names :help "Uniquify buffer names by adding parent directory names" :button
			      (:toggle and
				       (default-boundp . #44=('uniquify-buffer-name-style))
				       (default-value . #44#)))
		    (save-place menu-item "Save Place in Files between Sessions" toggle-save-place-globally :help "Visit files of previous session when restarting Emacs" :button
				(:toggle and
					 (default-boundp . #45=('save-place))
					 (default-value . #45#)))
		    (cursor-separator #2#)
		    (blink-cursor-mode menu-item "Blink Cursor" blink-cursor-mode :help "Whether the cursor blinks (Blink Cursor mode)" :button
				       (:toggle and
						(default-boundp . #46=('blink-cursor-mode))
						(default-value . #46#)))
		    (debugger-separator #2#)
		    (debug-on-error menu-item "Enter Debugger on Error" toggle-debug-on-error :help "Enter Lisp debugger when an error is signaled" :button
				    (:toggle and
					     (default-boundp . #47=('debug-on-error))
					     (default-value . #47#)))
		    (debug-on-quit menu-item "Enter Debugger on Quit/C-g" toggle-debug-on-quit :help "Enter Lisp debugger when C-g is pressed" :button
				   (:toggle and
					    (default-boundp . #48=('debug-on-quit))
					    (default-value . #48#)))
		    (mule-separator #2#)
		    (mule menu-item "Multilingual Environment"
			  (keymap
			   (set-language-environment menu-item #50="Set Language Environment"
						     (keymap
						      (Default menu-item #49# setup-specified-language-environment)
						      #50#
						      (Chinese #51# . setup-chinese-environment-map)
						      (Cyrillic #52# . setup-cyrillic-environment-map)
						      (Indian #53# . setup-indian-environment-map)
						      (Sinhala #54# . setup-specified-language-environment)
						      (English #55# . setup-specified-language-environment)
						      (ASCII #56# . setup-specified-language-environment)
						      (Ethiopic #57# . setup-specified-language-environment)
						      (European #58# . setup-european-environment-map)
						      (Turkish #59# . setup-specified-language-environment)
						      (Greek #60# . setup-specified-language-environment)
						      (Hebrew #61# . setup-specified-language-environment)
						      (Windows-1255 #62# . setup-specified-language-environment)
						      (Japanese #63# . setup-specified-language-environment)
						      (Korean #64# . setup-specified-language-environment)
						      (Lao #65# . setup-specified-language-environment)
						      (TaiViet #66# . setup-specified-language-environment)
						      (Thai #67# . setup-specified-language-environment)
						      (Tibetan #68# . setup-specified-language-environment)
						      (Vietnamese #69# . setup-specified-language-environment)
						      (IPA #70# . setup-specified-language-environment)
						      (Arabic #71# . setup-specified-language-environment)
						      (Persian #72# . setup-specified-language-environment)
						      (UTF-8 #73# . setup-specified-language-environment)
						      (Khmer #74# . setup-specified-language-environment)
						      (Burmese #75# . setup-specified-language-environment)
						      (Cham "Cham" . setup-specified-language-environment)))
			   (separator-mule #2#)
			   (toggle-input-method menu-item "Toggle Input Method" toggle-input-method)
			   (set-input-method menu-item "Select Input Method..." set-input-method)
			   (separator-input-method #2#)
			   (set-various-coding-system menu-item "Set Coding Systems"
						      (keymap
						       (universal-coding-system-argument menu-item "For Next Command" universal-coding-system-argument :help "Coding system to be used by next command")
						       (separator-1 #2#)
						       (set-buffer-file-coding-system menu-item "For Saving This Buffer" set-buffer-file-coding-system :help "How to encode this buffer when saved")
						       (revert-buffer-with-coding-system menu-item "For Reverting This File Now" revert-buffer-with-coding-system :enable buffer-file-name :help "Revisit this file immediately using specified coding system")
						       (set-file-name-coding-system menu-item "For File Name" set-file-name-coding-system :help "How to decode/encode file names")
						       (separator-2 #2#)
						       (set-keyboard-coding-system menu-item "For Keyboard" set-keyboard-coding-system :help "How to decode keyboard input")
						       (set-terminal-coding-system menu-item "For Terminal" set-terminal-coding-system :enable
										   (null
										    (memq initial-window-system
											  '(x w32 . #76=(ns))))
										   :help "How to encode terminal output")
						       (separator-3 #2#)
						       (set-selection-coding-system menu-item "For X Selections/Clipboard" set-selection-coding-system :visible #77=(display-selections-p)
										    :help "How to en/decode data to/from selection/clipboard")
						       (set-next-selection-coding-system menu-item "For Next X Selection" set-next-selection-coding-system :visible #77# :help "How to en/decode next selection/clipboard operation")
						       (set-buffer-process-coding-system menu-item "For I/O with Subprocess" set-buffer-process-coding-system :visible
											 (fboundp 'start-process)
											 :enable
											 (get-buffer-process . #78#)
											 :help "How to en/decode I/O from/to subprocess connected to this buffer")
						       "Set Coding System")
						      :enable . #79#)
			   (view-hello-file menu-item "Show Multilingual Sample Text" view-hello-file :enable
					    (file-readable-p
					     (expand-file-name "HELLO" data-directory))
					    :help "Demonstrate various character sets")
			   (separator-coding-system #2#)
			   (describe-language-environment menu-item #4# #80# :help "Show multilingual settings for a specific language")
			   (describe-input-method menu-item "Describe Input Method" describe-input-method)
			   (describe-coding-system menu-item #81# describe-coding-system)
			   (list-character-sets menu-item "List Character Sets" list-character-sets :help "Show table of available character sets")
			   (mule-diag menu-item "Show All Multilingual Settings" mule-diag . #82#)
			   "Mule (Multilingual Environment)"))
		    (showhide-separator #2#)
		    (showhide menu-item #107="Show/Hide"
			      (keymap
			       (showhide-tool-bar menu-item "Tool-bar" toggle-tool-bar-mode-from-frame :help "Turn tool-bar on/off" :visible #84=(display-graphic-p)
						  :button
						  (:toggle menu-bar-positive-p
							   (frame-parameter #83=(menu-bar-frame-for-menubar)
									    'tool-bar-lines)))
			       (menu-bar-mode menu-item "Menu-bar" toggle-menu-bar-mode-from-frame :help "Turn menu-bar on/off" :button
					      (:toggle menu-bar-positive-p
						       (frame-parameter #83# 'menu-bar-lines)))
			       (showhide-tooltip-mode menu-item "Tooltips" tooltip-mode :help "Turn tooltips on/off" :visible
						      (and #84#
							   (fboundp 'x-show-tip))
						      :button
						      (:toggle . tooltip-mode))
			       (showhide-scroll-bar menu-item #87="Scroll-bar"
						    (keymap
						     (none menu-item #88="None" menu-bar-no-scroll-bar :help "Turn off scroll-bar" :visible #84# :button
							   (:radio eq #86=(cdr
									   (assq 'vertical-scroll-bars
										 (frame-parameters)))
								   . #85#))
						     (left menu-item #89="On the Left" menu-bar-left-scroll-bar :help "Scroll-bar on the left side" :visible #84# :button
							   (:radio eq #86# . #94=('left)))
						     (right menu-item #90="On the Right" menu-bar-right-scroll-bar :help "Scroll-bar on the right side" :visible #84# :button
							    (:radio eq #86# . #95=('right)))
						     #87#)
						    . #92=(:visible #84#))
			       (showhide-fringe menu-item #99="Fringe"
						(keymap
						 (none menu-item #88# menu-bar-showhide-fringe-menu-customize-disable :help "Turn off fringe" :visible #84# :button
						       (:radio eq fringe-mode . #91=(0)))
						 (left menu-item #89# menu-bar-showhide-fringe-menu-customize-left :help "Fringe only on the left side" :visible #84# :button
						       (:radio equal fringe-mode
							       '(nil . 0)))
						 (right menu-item #90# menu-bar-showhide-fringe-menu-customize-right :help "Fringe only on the right side" :visible #84# :button
							(:radio equal fringe-mode '#91#))
						 (default menu-item #49# menu-bar-showhide-fringe-menu-customize-reset :help "Default width fringe on both left and right side" :visible #84# :button
						   (:radio eq fringe-mode . #85#))
						 (customize menu-item "Customize Fringe" menu-bar-showhide-fringe-menu-customize :help "Detailed customization of fringe" . #92#)
						 (indicate-empty-lines menu-item "Empty Line Indicators" toggle-indicate-empty-lines :help "Indicate trailing empty lines in fringe, globally" :button
								       (:toggle and
										(default-boundp . #93=('indicate-empty-lines))
										(default-value . #93#)))
						 (showhide-fringe-ind menu-item "Buffer Boundaries"
								      (keymap
								       (none menu-item "No Indicators" menu-bar-showhide-fringe-ind-none :help "Hide all buffer boundary indicators and arrows" :visible #84# :button
									     (:radio eq indicate-buffer-boundaries . #85#))
								       (left menu-item "In Left Fringe" menu-bar-showhide-fringe-ind-left :help "Show buffer boundaries and arrows in left fringe" :visible #84# :button
									     (:radio eq indicate-buffer-boundaries . #94#))
								       (right menu-item "In Right Fringe" menu-bar-showhide-fringe-ind-right :help "Show buffer boundaries and arrows in right fringe" :visible #84# :button
									      (:radio eq indicate-buffer-boundaries . #95#))
								       (box menu-item "Opposite, No Arrows" menu-bar-showhide-fringe-ind-box :help "Show top/bottom indicators in opposite fringes, no arrows" :visible #84# :button
									    (:radio equal indicate-buffer-boundaries '#97=(#96=(top . left)
															       (bottom . right))))
								       (mixed menu-item "Opposite, Arrows Right" menu-bar-showhide-fringe-ind-mixed :help "Show top/bottom indicators in opposite fringes, arrows in right" :visible #84# :button
									      (:radio equal indicate-buffer-boundaries
										      '((t . right)
											#96#)))
								       (customize menu-item "Other (Customize)" menu-bar-showhide-fringe-ind-customize :help "Additional choices available through Custom buffer" :visible #84# :button
										  (:radio not
											  (member indicate-buffer-boundaries
												  '(nil left right #97# . #98=(((t . right)
																#96#))))))
								       "Buffer boundaries")
								      :visible #84# :help "Indicate buffer boundaries in fringe")
						 #99#)
						. #92#)
			       (showhide-speedbar menu-item "Speedbar" speedbar-frame-mode :help "Display a Speedbar quick-navigation frame" :button
						  (:toggle and
							   (boundp . #100=('speedbar-frame))
							   (frame-live-p . #101=((symbol-value . #100#)))
							   (frame-visible-p . #101#)))
			       (datetime-separator #2#)
			       (showhide-date-time menu-item "Time, Load and Mail" display-time-mode :help "Display time, system load averages and mail status in mode line" :button
						   (:toggle and
							    (default-boundp . #102=('display-time-mode))
							    (default-value . #102#)))
			       (showhide-battery menu-item "Battery Status" display-battery-mode :help "Display battery status information in mode line" :button
						 (:toggle and
							  (default-boundp . #103=('display-battery-mode))
							  (default-value . #103#)))
			       (linecolumn-separator #2#)
			       (size-indication-mode menu-item "Size Indication" size-indication-mode :help "Show the size of the buffer in the mode line" :button
						     (:toggle and
							      (default-boundp . #104=('size-indication-mode))
							      (default-value . #104#)))
			       (line-number-mode menu-item "Line Numbers" line-number-mode :help "Show the current line number in the mode line" :button
						 (:toggle and
							  (default-boundp . #105=('line-number-mode))
							  (default-value . #105#)))
			       (column-number-mode menu-item "Column Numbers" column-number-mode :help "Show the current column number in the mode line" :button
						   (:toggle and
							    (default-boundp . #106=('column-number-mode))
							    (default-value . #106#)))
			       #107#))
		    (menu-set-font menu-item "Set Default Font..." menu-set-font :visible
				   (display-multi-font-p)
				   :help "Select a default font")
		    (custom-separator #2#)
		    (save menu-item "Save Options" menu-bar-options-save :help "Save options set from the menu above")
		    (package menu-item "Manage Emacs Packages" package-list-packages :help "Install or uninstall additional Emacs packages")
		    (customize menu-item "Customize Emacs"
			       (keymap
				(customize-themes menu-item "Custom Themes" customize-themes :help "Choose a pre-defined customization theme")
				(customize menu-item "Top-level Customization Group" customize :help "The master group called `Emacs'")
				(customize-browse menu-item "Browse Customization Groups" customize-browse :help "Browse all customization groups")
				(separator-3 #2#)
				(customize-saved menu-item "Saved Options" customize-saved :help "Customize previously saved options")
				(customize-changed-options menu-item "New Options..." customize-changed-options :help "Options added or changed in recent Emacs versions")
				(separator-2 #2#)
				(customize-option menu-item "Specific Option..." customize-option :help "Customize value of specific option")
				(customize-face menu-item "Specific Face..." customize-face :help "Customize attributes of specific face")
				(customize-group menu-item "Specific Group..." customize-group :help "Customize settings of specific group")
				(separator-1 #2#)
				(customize-apropos menu-item "All Settings Matching..." customize-apropos :help "Browse customizable settings matching a regexp or word list")
				(customize-apropos-options menu-item "Options Matching..." customize-apropos-options :help "Browse options matching a regexp or word list")
				(customize-apropos-faces menu-item "Faces Matching..." customize-apropos-faces :help "Browse faces matching a regexp or word list")
				"Customize"))
		    #108#)
	   (buffer #109="Buffers" keymap #109#
		   [("temp.el  *"
		     (nil)
		     lambda nil #110=(interactive)
		     (funcall menu-bar-select-buffer-function #<buffer temp.el>))
		    ("debug-pkb.el  "
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer debug-pkb.el>))
		    ("pkb.el  "
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer pkb.el>))
		    ("fix_in_emacs.org  "
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer fix_in_emacs.org>))
		    ("*scratch*  "
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer *scratch*>))
		    ("*Messages*  *%"
		     (nil)
		     lambda nil #110#
		     (funcall menu-bar-select-buffer-function #<buffer *Messages*>))]
		   (frames-separator . #111=(#2#))
		   (frames menu-item "Frames"
			   (keymap "Select Frame"
				   [("temp.el - emacs@geoffreys-prize.pc.sdu.dk"
				     (nil)
				     lambda nil #110#
				     (menu-bar-select-frame #<frame temp.el - emacs@geoffreys-prize.pc.sdu.dk 0x103e61798>))
				    ("pkb.el - emacs@geoffreys-prize.pc.sdu.dk"
				     (nil)
				     lambda nil #110#
				     (menu-bar-select-frame #<frame pkb.el - emacs@geoffreys-prize.pc.sdu.dk 0x102b8d250>))
				    ("pkb.el - emacs@geoffreys-prize.pc.sdu.dk"
				     (nil)
				     lambda nil #110#
				     (menu-bar-select-frame #<frame pkb.el - emacs@geoffreys-prize.pc.sdu.dk 0x10382f048>))]))
		   (command-separator . #111#)
		   (next-buffer menu-item "Next Buffer" next-buffer :help "Switch to the \"next\" buffer in a cyclic order")
		   (previous-buffer menu-item "Previous Buffer" previous-buffer :help "Switch to the \"previous\" buffer in a cyclic order")
		   (select-named-buffer menu-item "Select Named Buffer..." switch-to-buffer :help "Prompt for a buffer name, and select that buffer in the current window")
		   (list-all-buffers menu-item "List All Buffers" list-buffers :help "Pop up a window listing all Emacs buffers"))
	   (tools #115="Tools" keymap
		  (grep menu-item "Search Files (Grep)..." grep :help "Search files for strings or regexps (with Grep)")
		  (compile menu-item "Compile..." compile :help "Invoke compiler or Make, view compilation errors")
		  (shell menu-item "Shell Command..." shell-command :help "Invoke a shell command and catch its output")
		  (shell-on-region menu-item "Shell Command on Region..." shell-command-on-region :enable mark-active :help "Pass marked region to a shell command")
		  (gdb menu-item "Debugger (GDB)..." gdb :help "Debug a program from within Emacs with GDB")
		  (ede menu-item "Project Support (EDE)" global-ede-mode :help "Toggle the Emacs Development Environment (Global EDE mode)" :button
		       (:toggle bound-and-true-p global-ede-mode))
		  (semantic menu-item "Source Code Parsers (Semantic)" semantic-mode :help "Toggle automatic parsing in source code buffers (Semantic mode)" :button
			    (:toggle bound-and-true-p semantic-mode))
		  (separator-prog #2#)
		  (spell menu-item "Spell Checking" ispell-menu-map)
		  (separator-spell #2#)
		  (compare menu-item "Compare (Ediff)" menu-bar-ediff-menu)
		  (ediff-merge menu-item "Merge" menu-bar-ediff-merge-menu)
		  (epatch menu-item "Apply Patch" menu-bar-epatch-menu)
		  (separator-compare #2#)
		  (vc menu-item "Version Control" vc-menu-map :filter vc-menu-map-filter)
		  (separator-vc #2#)
		  (gnus menu-item "Read Net News" gnus :help "Read network news groups")
		  (rmail menu-item "Read Mail" menu-bar-read-mail :visible
			 (and read-mail-command
			      (not
			       (eq read-mail-command . #112=('ignore))))
			 :help "Read your mail")
		  (compose-mail menu-item "Compose New Mail" compose-mail :visible
				(and mail-user-agent
				     (not
				      (eq mail-user-agent . #112#)))
				:help "Start writing a new mail message")
		  (directory-search menu-item "Directory Search" eudc-tools-menu)
		  (browse-web menu-item "Browse the Web..." browse-web)
		  (separator-net #2#)
		  (calendar menu-item "Calendar" calendar :help "Invoke the Emacs built-in calendar")
		  (calc menu-item "Programmable Calculator" calc :help "Invoke the Emacs built-in full scientific calculator")
		  (simple-calculator menu-item "Simple Calculator" calculator :help "Invoke the Emacs built-in quick calculator")
		  (separator-encryption-decryption #2#)
		  (encryption-decryption menu-item #113="Encryption/Decryption"
					 (keymap
					  (decrypt-file menu-item "Decrypt File..." epa-decrypt-file :help "Decrypt a file")
					  (encrypt-file menu-item "Encrypt File..." epa-encrypt-file :help "Encrypt a file")
					  (verify-file menu-item "Verify File..." epa-verify-file :help "Verify digital signature of a file")
					  (sign-file menu-item "Sign File..." epa-sign-file :help "Create digital signature of a file")
					  (separator-file #2#)
					  (decrypt-region menu-item "Decrypt Region" epa-decrypt-region :help "Decrypt the current region")
					  (encrypt-region menu-item "Encrypt Region" epa-encrypt-region :help "Encrypt the current region")
					  (verify-region menu-item "Verify Region" epa-verify-region :help "Verify digital signature of the current region")
					  (sign-region menu-item "Sign Region" epa-sign-region :help "Create digital signature of the current region")
					  (separator-keys #2#)
					  (list-keys menu-item "List Keys" epa-list-keys :help "Browse your public keyring")
					  (import-keys menu-item "Import Keys from File..." epa-import-keys :help "Import public keys from a file")
					  (import-keys-region menu-item "Import Keys from Region" epa-import-keys-region :help "Import public keys from the current region")
					  (export-keys menu-item "Export Keys" epa-export-keys :help "Export public keys to a file")
					  (insert-keys menu-item "Insert Keys" epa-insert-keys :help "Insert public keys after the current point")
					  #113#))
		  (separator-games #2#)
		  (games menu-item #114="Games"
			 (keymap
			  (5x5 menu-item "5x5" 5x5 :help "Fill in all the squares on a 5x5 board")
			  (adventure menu-item "Adventure" dunnet :help "Dunnet, a text Adventure game for Emacs")
			  (black-box menu-item "Blackbox" blackbox :help "Find balls in a black box by shooting rays")
			  (bubbles menu-item "Bubbles" bubbles :help "Remove all bubbles using the fewest moves")
			  (gomoku menu-item "Gomoku" gomoku :help "Mark 5 contiguous squares (like tic-tac-toe)")
			  (hanoi menu-item "Towers of Hanoi" hanoi :help "Watch Towers-of-Hanoi puzzle solved by Emacs")
			  (land menu-item "Landmark" landmark :help "Watch a neural-network robot learn landmarks")
			  (life menu-item "Life" life :help "Watch how John Conway's cellular automaton evolves")
			  (mult menu-item "Multiplication Puzzle" mpuz :help "Exercise brain with multiplication")
			  (pong menu-item "Pong" pong :help "Bounce the ball to your opponent")
			  (snake menu-item "Snake" snake :help "Move snake around avoiding collisions")
			  (solitaire menu-item "Solitaire" solitaire :help "Get rid of all the stones")
			  (tetris menu-item "Tetris" tetris :help "Falling blocks game")
			  (zone menu-item "Zone Out" zone :help "Play tricks with Emacs display when Emacs is idle")
			  #114#))
		  #115#)
	   (mouse-1 . tmm-menubar-mouse))
 (f4 . kmacro-end-or-call-macro)
 (f3 . kmacro-start-macro-or-insert-counter)
 (C-M-end . end-of-defun)
 (C-M-home . beginning-of-defun)
 (C-M-down . down-list)
 (C-M-up . backward-up-list)
 (C-M-right . forward-sexp)
 (C-M-left . backward-sexp)
 (S-delete . kill-region)
 (C-backspace . backward-kill-word)
 (C-delete . kill-word)
 (C-left . left-word)
 (C-right . right-word)
 (M-left . left-word)
 (M-right . right-word)
 (mouse-movement . ignore)
 (deletechar . delete-forward-char)
 (deleteline . kill-line)
 (open . find-file)
 (redo . repeat-complex-command)
 (undo . undo)
 (S-insert . yank)
 (C-insert . kill-ring-save)
 (insert . overwrite-mode)
 (M-end . end-of-buffer-other-window)
 (C-end . end-of-buffer)
 (end . end-of-buffer)
 (M-prior . scroll-other-window-down)
 (M-next . scroll-other-window)
 (C-next . scroll-left)
 (C-prior . scroll-right)
 (C-down . forward-paragraph)
 (C-up . backward-paragraph)
 (next . scroll-up-command)
 (prior . scroll-down-command)
 (down . next-line)
 (right . right-char)
 (up . previous-line)
 (left . left-char)
 (M-home . beginning-of-buffer-other-window)
 (C-home . beginning-of-buffer)
 (home . beginning-of-buffer)
 (C-S-backspace . kill-whole-line)
 (menu . execute-extended-command)
 (67108896 . set-mark-command)
 (67108909 . negative-argument)
 (67108921 . digit-argument)
 (67108920 . digit-argument)
 (67108919 . digit-argument)
 (67108918 . digit-argument)
 (67108917 . digit-argument)
 (67108916 . digit-argument)
 (67108915 . digit-argument)
 (67108914 . digit-argument)
 (67108913 . digit-argument)
 (67108912 . digit-argument)
 (XF86Back . previous-buffer)
 (XF86Forward . next-buffer)
 (67108911 . undo)
 (make-frame-visible . ignore-event)
 (iconify-frame . ignore-event)
 (delete-frame . handle-delete-frame)
 (select-window . handle-select-window)
 (switch-frame . handle-switch-frame))

(pptcb (current-local-map))
(keymap
 (24 keymap
     (1 keymap
	(12 . edebug-where)
	(3 . edebug-go-mode)
	(14 . edebug-next-mode)
	(19 . edebug-step-mode)))
 (3 keymap
    (100 . duplicate-line))
 (menu-bar keymap
	   (emacs-lisp #4="Emacs-Lisp" keymap
		       (indent-line menu-item "Indent Line" lisp-indent-line)
		       (indent-region menu-item "Indent Region" indent-region :help "Indent each nonblank line in the region" . #1=(:enable mark-active))
		       (comment-region menu-item "Comment Out Region" comment-region :help "Comment or uncomment each line in the region" . #1#)
		       (separator-format #2="--")
		       (eval-sexp menu-item "Evaluate Last S-expression" eval-last-sexp :help "Evaluate sexp before point; print value in echo area")
		       (eval-region menu-item "Evaluate Region" eval-region :help "Execute the region as Lisp code" . #1#)
		       (eval-buffer menu-item "Evaluate Buffer" eval-buffer :help "Execute the current buffer as Lisp code")
		       (ielm menu-item "Interactive Expression Evaluation" ielm :help "Interactively evaluate Emacs Lisp expressions")
		       (separator-eval #2#)
		       (byte-compile menu-item "Byte-compile This File" emacs-lisp-byte-compile :help "Byte compile the file containing the current buffer")
		       (emacs-byte-compile-and-load menu-item "Byte-compile and Load" emacs-lisp-byte-compile-and-load :help "Byte-compile the current file (if it has changed), then load compiled code")
		       (byte-recompile menu-item "Byte-recompile Directory..." byte-recompile-directory :help "Recompile every `.el' file in DIRECTORY that needs recompilation")
		       (disas menu-item "Disassemble Byte Compiled Object..." disassemble :help "Print disassembled code for OBJECT in a buffer")
		       (separator-byte #2#)
		       (edebug-defun menu-item "Instrument Function for Debugging" edebug-defun :help "Evaluate the top level form point is in, stepping through with Edebug" :keys "C-u C-M-x")
		       (lint "Linting" keymap
			     (lint-d menu-item "Lint Defun" elint-defun :help "Lint the function at point")
			     (lint-b menu-item "Lint Buffer" elint-current-buffer :help "Lint the current buffer")
			     (lint-f menu-item "Lint File..." elint-file :help "Lint a file")
			     (lint-di menu-item "Lint Directory..." elint-directory :help "Lint a directory"))
		       (profiling "Profiling" keymap
				  (prof-natprof-start menu-item "Start Native Profiler..." profiler-start :help "Start recording profiling information")
				  (prof-natprof-report menu-item "Show Profiler Report" profiler-report :help "Show the current profiler report" . #3=(:enable
																		       (and
																			(featurep 'profiler)
																			(profiler-running-p))))
				  (prof-natprof-stop menu-item "Stop Native Profiler" profiler-stop :help "Stop recording profiling information" . #3#)
				  (sep-natprof #2#)
				  (prof-func menu-item "Instrument Function..." elp-instrument-function :help "Instrument a function for profiling")
				  (prof-pack menu-item "Instrument Package..." elp-instrument-package :help "Instrument for profiling all function that start with a prefix")
				  (prof-res menu-item "Show Profiling Results" elp-results :help "Display current profiling results")
				  (prof-resfunc menu-item "Reset Counters for Function..." elp-reset-function :help "Reset the profiling information for a function")
				  (prof-resall menu-item "Reset Counters for All Functions" elp-reset-all :help "Reset the profiling information for all functions being profiled")
				  (sep-rem #2#)
				  (prof-restfunc menu-item "Remove Instrumentation for Function..." elp-restore-function :help "Restore an instrumented function to its original definition")
				  (prof-restall menu-item "Remove Instrumentation for All Functions" elp-restore-all :help "Restore the original definitions of all functions being profiled"))
		       (tracing "Tracing" keymap
				(tr-f menu-item "Trace Function..." trace-function :help "Trace the function given as an argument")
				(tr-q menu-item "Trace Function Quietly..." trace-function-background :help "Trace the function with trace output going quietly to a buffer")
				(tr-sep #2#)
				(tr-uf menu-item "Untrace Function..." untrace-function :help "Untrace function, and possibly activate all remaining advice")
				(tr-a menu-item "Untrace All" untrace-all :help "Untrace all currently traced functions"))
		       (re-builder menu-item "Construct Regexp" re-builder :help "Construct a regexp interactively")
		       (checkdoc menu-item "Check Documentation Strings" checkdoc :help "Check documentation strings for style requirements")
		       (eldoc menu-item "Auto-Display Documentation Strings" eldoc-mode :button
			      (:toggle bound-and-true-p eldoc-mode)
			      :help "Display the documentation string for the item under cursor")
		       #4#))
 (27 keymap
     (17 . indent-pp-sexp)
     (24 . eval-defun)
     (9 . completion-at-point))
 (keymap (24 . dothis))
 #4# keymap
 (127 . backward-delete-char-untabify)
 (27 keymap
     (17 . indent-sexp))
 keymap
 (27 keymap
     (17 . prog-indent-sexp)))

(pptcb
 (make-composed-keymap '((keymap (1 . dothis-1)) (keymap (2 . dothis-2))) '(keymap (3 . dothis-3))))
(keymap
 (keymap
  (1 . dothis-1))
 (keymap
  (2 . dothis-2))
 keymap
 (3 . dothis-3))




(cl-loop
 for i to 2
 do (print "ab")
 and if (= i 1) do (print "gg"))

(pptcb (current-local-map))
(keymap
 (24 keymap
     (1 keymap
	(12 . edebug-where)
	(3 . edebug-go-mode)
	(14 . edebug-next-mode)
	(19 . edebug-step-mode)))
 (3 keymap
    (100 . duplicate-line))
 (27 keymap
     (17 . indent-pp-sexp)
     (24 . eval-defun)
     (9 . completion-at-point))
 #4# keymap
 (127 . backward-delete-char-untabify)
 (27 keymap
     (17 . indent-sexp))
 keymap
 (27 keymap
     (17 . prog-indent-sexp)))


(intern-soft "abc")

(let ((abc 1)))

(if nil
    (let ((abc 1))
      #1=abc)
  (let ((abc 2))
    #1#))

(pptcb (pkb-list-keymap-symbols (current-local-map)))
(((keymap
   (127 . backward-delete-char-untabify)
   (27 keymap
       (17 . indent-sexp))
   keymap
   (27 keymap
       (17 . prog-indent-sexp)))
  ((lisp-mode-shared-map . value))
  ([keymap-parent 6 nil])
  []))

(pptcb (current-local-map))

(pptcb (pkb-list-keymap-symbols (current-local-map)))
(((keymap
   (127 . backward-delete-char-untabify)
   (27 keymap
       (17 . indent-sexp))
   keymap
   (27 keymap
       (17 . prog-indent-sexp)))
  ((lisp-mode-shared-map . value))
  ([keymap-parent 6 nil])
  []))


(pptcb (pkb-list-keymap-symbols (current-global-map)))







(pkb-map-keymap-items
 (lambda (item item-type item-loc)
   (ptcb (list item item-type item-loc))
   (insert "\n"))
 (current-local-map))

(cl-case 'a
  ((a b) (print "a") ))


(pptcb (current-local-map))


(keymap
 (24 keymap
     (1 keymap
	(12 . edebug-where)
	(3 . edebug-go-mode)
	(14 . edebug-next-mode)
	(19 . edebug-step-mode)))
 (3 keymap
    (100 . duplicate-line))
 (menu-bar keymap
	   (emacs-lisp #4="Emacs-Lisp" keymap
		       (indent-line menu-item "Indent Line" lisp-indent-line)
		       (indent-region menu-item "Indent Region" indent-region :help "Indent each nonblank line in the region" . #1=(:enable mark-active))
		       (comment-region menu-item "Comment Out Region" comment-region :help "Comment or uncomment each line in the region" . #1#)
		       (separator-format #2="--")
		       (eval-sexp menu-item "Evaluate Last S-expression" eval-last-sexp :help "Evaluate sexp before point; print value in echo area")
		       (eval-region menu-item "Evaluate Region" eval-region :help "Execute the region as Lisp code" . #1#)
		       (eval-buffer menu-item "Evaluate Buffer" eval-buffer :help "Execute the current buffer as Lisp code")
		       (ielm menu-item "Interactive Expression Evaluation" ielm :help "Interactively evaluate Emacs Lisp expressions")
		       (separator-eval #2#)
		       (byte-compile menu-item "Byte-compile This File" emacs-lisp-byte-compile :help "Byte compile the file containing the current buffer")
		       (emacs-byte-compile-and-load menu-item "Byte-compile and Load" emacs-lisp-byte-compile-and-load :help "Byte-compile the current file (if it has changed), then load compiled code")
		       (byte-recompile menu-item "Byte-recompile Directory..." byte-recompile-directory :help "Recompile every `.el' file in DIRECTORY that needs recompilation")
		       (disas menu-item "Disassemble Byte Compiled Object..." disassemble :help "Print disassembled code for OBJECT in a buffer")
		       (separator-byte #2#)
		       (edebug-defun menu-item "Instrument Function for Debugging" edebug-defun :help "Evaluate the top level form point is in, stepping through with Edebug" :keys "C-u C-M-x")
		       (lint "Linting" keymap
			     (lint-d menu-item "Lint Defun" elint-defun :help "Lint the function at point")
			     (lint-b menu-item "Lint Buffer" elint-current-buffer :help "Lint the current buffer")
			     (lint-f menu-item "Lint File..." elint-file :help "Lint a file")
			     (lint-di menu-item "Lint Directory..." elint-directory :help "Lint a directory"))
		       (profiling "Profiling" keymap
				  (prof-natprof-start menu-item "Start Native Profiler..." profiler-start :help "Start recording profiling information")
				  (prof-natprof-report menu-item "Show Profiler Report" profiler-report :help "Show the current profiler report" . #3=(:enable
																		       (and
																			(featurep 'profiler)
																			(profiler-running-p))))
				  (prof-natprof-stop menu-item "Stop Native Profiler" profiler-stop :help "Stop recording profiling information" . #3#)
				  (sep-natprof #2#)
				  (prof-func menu-item "Instrument Function..." elp-instrument-function :help "Instrument a function for profiling")
				  (prof-pack menu-item "Instrument Package..." elp-instrument-package :help "Instrument for profiling all function that start with a prefix")
				  (prof-res menu-item "Show Profiling Results" elp-results :help "Display current profiling results")
				  (prof-resfunc menu-item "Reset Counters for Function..." elp-reset-function :help "Reset the profiling information for a function")
				  (prof-resall menu-item "Reset Counters for All Functions" elp-reset-all :help "Reset the profiling information for all functions being profiled")
				  (sep-rem #2#)
				  (prof-restfunc menu-item "Remove Instrumentation for Function..." elp-restore-function :help "Restore an instrumented function to its original definition")
				  (prof-restall menu-item "Remove Instrumentation for All Functions" elp-restore-all :help "Restore the original definitions of all functions being profiled"))
		       (tracing "Tracing" keymap
				(tr-f menu-item "Trace Function..." trace-function :help "Trace the function given as an argument")
				(tr-q menu-item "Trace Function Quietly..." trace-function-background :help "Trace the function with trace output going quietly to a buffer")
				(tr-sep #2#)
				(tr-uf menu-item "Untrace Function..." untrace-function :help "Untrace function, and possibly activate all remaining advice")
				(tr-a menu-item "Untrace All" untrace-all :help "Untrace all currently traced functions"))
		       (re-builder menu-item "Construct Regexp" re-builder :help "Construct a regexp interactively")
		       (checkdoc menu-item "Check Documentation Strings" checkdoc :help "Check documentation strings for style requirements")
		       (eldoc menu-item "Auto-Display Documentation Strings" eldoc-mode :button
			      (:toggle bound-and-true-p eldoc-mode)
			      :help "Display the documentation string for the item under cursor")
		       #4#))
 (27 keymap
     (17 . indent-pp-sexp)
     (24 . eval-defun)
     (9 . completion-at-point))
 #4# keymap
 (127 . backward-delete-char-untabify)
 (27 keymap
     (17 . indent-sexp))
 keymap
 (27 keymap
     (17 . prog-indent-sexp)))

(pkb-symbol-locations-for-value (nthcdr 6 (current-local-map)))




(intern-soft "ffff")
(unintern "ffff") 

(defun test (ffff)
  ())


(setq test 'b)

(cl-case 'test
  (test (print "test"))
  (b (print "b")))
