(defmacro macro-defn (newelt to-list)
;; (cl-defmacro macro-defn (newelt to-list)
  "Add NEWELT to the end of TO-LIST."
  ;; if to-list is null, make a new list
  (if (null (eval to-list))
      `(setf ,to-list (list ,newelt))
    `(nconc ,to-list (list ,newelt))))

(cl-defun somefcn ()
  "Print key bindings of KEYMAP to STREAM, returning the list created.
If EVENTS-AS-ESCAPE-SEQUENCES-FLAG (true, by default), print the events in
KEYMAP using `character-code-to-escape-syntax'."
  (macro-defn cons reaped-list))


(progn
  (defmacro macro-defn (newelt to-list)
    ;; (cl-defmacro macro-defn (newelt to-list)
    "Add NEWELT to the end of TO-LIST."
    ;; if to-list is null, make a new list
    (when (null (eval to-list))
	`(setf ,to-list (list ,newelt))))

  (cl-defun somefcn ()
    "Print key bindings of KEYMAP to STREAM, returning the list created.
If EVENTS-AS-ESCAPE-SEQUENCES-FLAG (true, by default), print the events in
KEYMAP using `character-code-to-escape-syntax'."
    (macro-defn cons reaped-list))
  )

;; mark 1
(defmacro macro-defn (newelt to-list)
    (when (null (eval to-list))
	`(setf ,to-list (list ,newelt))))

(cl-defun somefcn ()
    "Print key bindings of KEYMAP to STREAM, returning the list created.
If EVENTS-AS-ESCAPE-SEQUENCES-FLAG (true, by default), print the events in
KEYMAP using `character-code-to-escape-syntax'."
    (let (a) (macro-defn cons a)))

(cl-loop for name in '("macro-defn" "somefcn" "reaped-list")
	 collect (unintern name))

;; mark 2
(defmacro macro-defn (to-list) ""
  (when (null (eval to-list))
    t))

(cl-defun somefcn () ""
    (let (a) (macro-defn a)))

(cl-loop for name in '("macro-defn" "somefcn" "reaped-list")
	 collect (unintern name))

;; mark 3
(defmacro macro-defn (to-list) ""
  (null (eval to-list)))

(cl-defun somefcn () ""
    (let (a) (macro-defn a)))

(cl-loop for name in '("macro-defn" "somefcn")
	 collect (unintern name))

;; mark 4
(cl-loop for name in '("macro-defn" "somefcn")
	 collect (unintern name))
(defmacro macro-defn (to-list) ""
	  (eval to-list))
(cl-defun somefcn () ""
	  (let (a) (macro-defn a)))

;; mark 5
(cl-loop for name in '("macro-defn" "somefcn")
	 collect (unintern name))
(defmacro mymacro (a) ""
	  (eval a))
(defun somefcn () ""
	  (let (b) (mymacro b)))


;; mark 6 (ok)
(cl-loop for name in '("macro-defn" "somefcn" "a" "b" "name" "to-list")
	 collect (unintern name))
(defmacro macro-defn (a) ""
	  (eval a))
(defun somefcn () ""
       (let (a) (macro-defn a)))

;; mark 7
(cl-loop for name in '("macro-defn" "somefcn" "a" "b" "name" "to-list")
	 collect (unintern name))
(progn
  (defmacro macro-defn (a) ""
	    (eval a t))
  (defun somefcn () "ABC"
	 (let (b) (macro-defn b))))

;; mark 8
(cl-loop for name in '("mymacro" "myfcn" "a" "b") collect (unintern name))
(defmacro mymacro (a) "" (eval a))
(defun somefcn (b) "" (mymacro b))

(somefcn ''a)

(ptcb (symbol-function 'mymacro))
(macro lambda (a) "" (eval a))




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

(defun )
