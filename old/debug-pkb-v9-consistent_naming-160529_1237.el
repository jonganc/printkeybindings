;; ************************************************
;; pkb

(require 'cl)

(defun pkb-join-symbs-to-symb (&rest args)
  "Join arguments, which can be an integer, symbol, or strings, into a single symbol."
  (car (read-from-string
	(cl-loop 
	 for arg in args
	 concat (pcase arg
		  ((or (pred symbolp) (pred integerp))
		   (prin1-to-string arg))
		  ((pred stringp)
		   arg)
		  (_
		   (error "join-symbs-to-symb: argument should a symbol or string: %S"
			  arg)))))))

(cl-defun pkb-make-test-keymap (&optional symbol-prefix)
  "Generate a keymap for testing. 
The keymap will use several symbols for storing values, each prefixed by
SYMBOL-PREFIX. If SYMBOL-PREFIX, a symbol or a string, is not given, it will be
generated. If t is passed, no variables will be saved. Note that in this case,
no prefix commands are defined using `define-prefix-command'.

If SYMBOL-PREFIX was t, the return value is just KEYMAP, the output
keymap. Otherwise, the return value is (KEYMAP . OUTPUT-SYMBOL-PREFIX-STRING), where
OUTPUT-SYMBOL-PREFIX-STRING is the symbol prefix used, as a string.

The variables defined will be:
SYMBOL-PREFIX-km-base-full - the keymap
SYMBOL-PREFIX-km-par-full - a full keymap parent
SYMBOL-PREFIX-km-par-spare - a sparse keymap parent
SYMBOL-PREFIX-ctl-q-prefix - a prefix defined for ctl-q

KM1 is a full keymap. Its parent is the sparse keymap KM2, whose parent is the full keymap KM3.
"
  (let* ((outp-symb-pref-str
	  (pcase symbol-prefix
	    (`t nil)
	    ((pred null) (concat (prin1-to-string (cl-gensym)) "-"))
	    ((pred symbolp) (prin1-to-string symbol-prefix))
	    ((pred stringp) symbol-prefix)
	    (_
	     (error "SYMBOL-PREFIX should be nil, t, a symbol, or a string: %S"
		    symbol-prefix)
	     (cl-return-from pkb-make-test-keymap))))
	 ;;  KMS is a list of three keymaps
	 ;;  we refer to its elements as KM1, KM2, KM3, i.e. km2 is (nth 1 kms)
	 ;;  KM1 is a full keymap, whose parent keymap is the sparse keymap KM2,
	 ;;  whose parent keymap is the full keymap KM3.
	 (kms (list (make-keymap) (make-sparse-keymap) (make-keymap)))
	 ;;  convenience symbols
	 (km1 (nth 0 kms)) (km2 (nth 1 kms)) (km3 (nth 2 kms))
	 ;;  a list holding the symbols which are defined later as prefix keys
	 ;;  via `define-prefix-command'
	 p-pref-symbs)
    ;; let us define some helper functions
    (cl-flet*
	;; let's define JSTS as an alias for `pkb-join-symbs-to-symb'
	((jsts (&rest args) (apply 'pkb-join-symbs-to-symb args))
	 ;; DEFINE-ALL takes KEY-SEQ, a key sequence, and SYMB-SUFF, a string
	 ;; which is appended to the binding (to mark it as coming from that
	 ;; that key)
	 (define-all (key-seq symb-suff)
	   (cl-loop
	    for i to 2
	    do (define-key (nth i kms) key-seq (jsts "km" (1+ i) "-" symb-suff))))
	 ;; DEFINE-SHADOW-12-DFN-3 takes a key-sequence KEY-SEQ and a definition
	 ;; DEFN. It explicitly sets KEY-SEQ to nil in KM1, KM2, and sets it to
	 ;; DEFN in KM3.
	 (define-shadow-12-dfn-3 (key-seq defn)
	   (cl-loop for i to 1
		    do (define-key (nth i kms) key-seq nil))
	   (define-key km3 key-seq defn)))
      
      (set-keymap-parent km1 km2)
      (set-keymap-parent km2 km3)

      ;; set prefix commands using `define-prefix-command'
      (when outp-symb-pref-str
	;;  first we define the symbols for the commands
	(cl-loop
	 with p-pref-symb
	 for i to 2
	 for p-pref-symb = (jsts outp-symb-pref-str "p-pref-km" (1+ i))
	 do
	 ;;  add symbs to P-PREF-SYMBS
	 (pkb-push-end p-pref-symb p-pref-symbs)
	 (define-prefix-command p-pref-symb)
	 (define-key (nth i kms)  [?p] p-pref-symb)
	 (define-key p-pref-symb [?a] (jsts "p-pref" (1+ i) "-a"))))
      
      ;;  define a character key
      (define-all [?a] "a")
      ;;  define a meta+character key
      (define-all [?\M-a] "M-a")
      ;;  define a non-character-integer key
      (define-all [?\S-a] "S-a")
      ;;  define a meta+non-character-integer key
      (define-all [?\M-\S-a] "M-S-a")
      ;;  define a non-integer key
      (define-all [kp-1] "next")
      ;;  define a meta+non-integer key
      (define-all [M-kp-1] "M-next")
      ;;  define a remapping
      (define-all [remap forward-char] "remap-forward-char")

      ;; define keys that are only in parent keymap and clear in base keymaps
      (define-key km3 [?b] 'km3-only-b)
      (define-key km3 [?\M-b] 'km3-only-M-b)
      (define-key km3 [?\S-b] 'km3-only-b)
      (define-key km3 [?\M-\S-b] 'km3-only-M-S-b)
      (define-key km3 [kp-2] 'km3-only-kp-2)
      (define-key km3 [M-kp-2] 'km3-only-M-kp-2)
      (define-key km3 [remap backward-char] 'km3-only-backward-char)
      
      ;;  add some shadowed keys (keys that are defined a parent keymap and set
      ;;  explicitly to nil in base keymaps)
      (define-shadow-12-dfn-3 [?c] 'km3-shadowed-c)
      (define-shadow-12-dfn-3 [?\M-c] 'km3-shadowed-M-c)
      (define-shadow-12-dfn-3 [kp-3] 'km3-shadowed-kp-3)
      (define-shadow-12-dfn-3 [remap next-line] 'km3-shadowed-remap-next-line)
      
      ;;  define a deeper sequence
      (define-all [?1 ?1 ?1] "deeper-1-1-1")
      
      ;; define some menu items
      ;; overlapping menus
      (easy-menu-define nil kms "An overlapping menu"
	'("Overlap-Menu"
	  ["Overlap cmd a string" overlap-cmd-a]
	  ["Overlap cmd b string" overlap-cmd-b]))

      ;; non-overlapping menus
      (cl-loop for i to 2
	       for i-str = (prin1-to-string (1+ i))
	       for cmd-a = (jsts "nonoverlap-" i-str "-cmd-a")
	       for cmd-b = (jsts "nonoverlap-" i-str "-cmd-b")
	       do
	       (eval
		`(easy-menu-define nil (nth i kms)
		   ,(concat "An nonoverlapping menu - " i-str)
		   '(,(concat "Nonoverlap-Menu-" i-str)
		     [,(concat (prin1-to-string cmd-a) " string") ,cmd-a]
		     [,(concat (prin1-to-string cmd-b) " string") ,cmd-b]))))

      ;;  define some tool-bar items
      (cl-loop for i to 2
	       for km = (nth i kms)
	       for i-str = (prin1-to-string (1+ i))
	       for tool-bar-map = (make-sparse-keymap)
	       do
	       (define-key km [tool-bar] tool-bar-map)

	       ;;  overlapping items
	       ;;  note that `tool-bar-add-item' automatically adds to `tool-bar-map'
	       (tool-bar-add-item "overlap-icon-a" 'tool-bar-overlap-defn-a
				  'tool-bar-overlap-key-a)
	       (tool-bar-add-item "overlap-icon-b" 'tool-bar-overlap-defn-b
				  'tool-bar-overlap-key-b)

	       ;;  non-overlapping items
	       ;;  note that `tool-bar-add-item' automatically adds to `tool-bar-map'
	       (tool-bar-add-item (concat "overlap-" i-str "-icon-a")
				  (jsts 'tool-bar-overlap- i-str '-defn-a)
				  (jsts 'tool-bar-overlap- i-str '-key-a))
	       (tool-bar-add-item (concat "overlap-" i-str "-icon-b")
				  (jsts 'tool-bar-overlap- i-str '-defn-b)
				  (jsts 'tool-bar-overlap- i-str '-key-b)))
      

      ;; save to external variables if desired
      
      km1)))
(byte-compile 'pkb-make-test-keymap)

;;; pkb-concat=
(ert-deftest pkb-test-pkb-concat= ()
  "Tests `pkb-concat='"
  (should
   (equal
    (let ((txt "abc"))
      (pkb-concat= txt "123")
      txt)
    "abc123")))

;;; pkb-dolist-cons
(ert-deftest pkb-test-dolist-cons ()
  "Tests `pkb-dolist-cons'"
  (should
   (equal
    (let ((tst-lst (copy-tree '(1 2 3))))
      (pkb-dolist-cons (elem-cons tst-lst)
	(setcar elem-cons (1+ (car elem-cons))))
      tst-lst)
    '(2 3 4))))

;;; pkb-include-base-key-list
(ert-deftest pkb-test-include-base-key-list ()
  "Test `pkb-include-base-key-list'"
  (should
   (equal
    pkb-include-base-key-list
    '(return tab escape backspace delete insert home end prior next right down left up f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 lwindow rwindow apps kp-down kp-add kp-begin kp-decimal kp-delete kp-divide kp-down kp-end kp-enter kp-equal kp-home kp-insert kp-left kp-multiply kp-next kp-prior kp-right kp-separator kp-space kp-subtract kp-tab kp-up kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9 wheel-down wheel-up mouse-1 mouse-2 mouse-3 mouse-4))))

;;; pkb-include-keyboard-modifier
(ert-deftest pkb-test-include-keyboard-modifier ()
  "Test `pkb-include-keyboard-modifier'"
  (should
   (equal
    pkb-include-keyboard-modifier
    '(shift control meta alt))))

;;; pkb-include-mouse-modifier
(ert-deftest pkb-test-include-mouse-modifier ()
  "Test `pkb-include-mouse-modifier'"
  (should
   (equal
    pkb-include-mouse-modifier
    '(click down double drag triple))))

;;; pkb-char-event-p
(ert-deftest pkb-test-char-event-p ()
  "Test `pkb-char-event-p'"
  (should (equal (pkb-char-event-p ?\C-a) t))
  (should (equal (pkb-char-event-p ?\e) t))
  (should (equal (pkb-char-event-p ?a) t))
  (should (equal (pkb-char-event-p 'prior) nil))
  (should (equal (pkb-char-event-p 'right) nil)))



;;; pkb-include-mk-p
(ert-deftest pkb-test-include-mk-p ()
  "Test `pkb-include-mk-p'"
  (should (equal (pkb-include-mk-p '?\C-a) t))
  (should (equal (pkb-include-mk-p '?\C-\M-g) t))
  (should (equal (pkb-include-mk-p '?\e) t))
  (should (equal (pkb-include-mk-p '?a) t))
  (should (equal (pkb-include-mk-p 'prior) t))
  (should (equal (pkb-include-mk-p 'right) t))
  (should (equal (pkb-include-mk-p '[?4 ?6]) t))
  (should (equal (pkb-include-mk-p '[f19 ?t]) nil))
  (should (equal (pkb-include-mk-p '[?\C-x cut]) nil))
  (should (equal (pkb-include-mk-p '[C-up-mouse-2]) nil)))

;;; pkb-compare-key-sequences, pkb-compare-modified-keys,
;;;  pkb-compare-base-key
(ert-deftest pkb-test-compare-keys ()
  """Test `pkb-compare-key-sequences', `pkb-compare-modified-keys',
  `pkb-compare-base-key'"""
  (should
   (equal
    (sort (copy-tree '( [?a ?b ?c] [?a ?b] [?a ?\C-b] [?a left] [?a ?a]))
	  'pkb-compare-key-sequences)
    '([?a ?a] [?a ?b] [?a ?b ?c] [?a left] [?a ?\C-b]))))

;;; PKB-ACCESSIBLE-KEYMAPS
(ert-deftest pkb-test-accessible-keymaps ()
  "Test `pkb-accessible-keymaps'"
  (should
   (equal
    (pkb-accessible-keymaps
     `#1=(keymap (?a . #2=(keymap (?a . binding1) (?b . binding2)))
		 (?b . #3=(keymap (?a . binding3) (?b . #4=(keymap (?a . binding4)))))
		 (?c . #5=(keymap (?a . binding5)
				  (?\C-w . #6=(keymap (?a . binding5)))) )))
    `(([?c ?\C-w] nil . #6#)
      ([?b ?b] nil . #4#)
      ([?c] nil . #5#)
      ([?b] nil . #3#)
      ([?a] nil . #2#)
      ([] nil . #1#))))
  (should
   (equal
    (pkb-accessible-keymaps
     `(keymap (1 . (keymap (1 . binding) (2 . binding2)))
	      (2 . #1=(keymap (1 . binding3) (2 . (keymap (1 . binding4)))))
	      (3 . #2=(keymap (1 . binding5)
			      (2 . (keymap (1 . binding5) (2 . #3=(keymap (1 . bind)))))
			      (3 . #4=(keymap (1 . binding5)
					      (2 . #5=(keymap (ccba . bind))))))))
     '([2] [3]) '([2 2] ([3 2])))
    `(([2] nil . #1#)
      ([3 3 2] nil . #5#)
      ([3 2 2] nil . #3#)
      ([3 3] nil . #4#)
      ([3] nil . #2#))))

  (should (equal
	   (pkb-accessible-keymaps (current-global-map) '(([?\M-g])))
	   `(([?\M-g] nil . ,(key-binding [?\M-g])))))
  
  ;;  the purpose of this test is just to make sure that applying
  ;;  `pkb-accessible-keymaps' to `current-global-map' does not return an error
  ;;  maybe should be replaced with/always have additional tests
  (should
   (progn
     (pkb-accessible-keymaps (current-global-map))
     t)))

;;; pkb-list-keys

(ert-deftest pkb-test-list-keys ()
  ;;  the purpose of this test is just to make sure that applying
  ;;  `pkb-list-keys' to `current-global-map' does not return an error
  ;;  can and probably should have additional tests
  "Test `pkb-list-keys'"
  (should
   (progn
     (pkb-list-keys (current-global-map))
     t)))

(progn
  (pkb-list-keys (current-global-map))
  t)


(let ((km (make-keymap))
      (events-binds-for-ct
       `((?a . bind-ab)
	 (?b . bind-ab)
	 (?c . bind-c)
	 (?\C-a . bind-C-a)
	 (?\v . bind-vv)))
      ;;  EVENTS-BINDS-TO-ESC will have bindings that will end up in ESC. The
      ;;  bindings are listed as they will be in the ESC prefix key,
      ;;  i.e. without the meta
      (events-binds-to-esc
       `((?a . bind-M-a)
	 (?\C-a . bind-C-M-ab)
	 (?\C-b . bind-C-M-ab)))
      (events-binds-other
       `((?\C-\e . bind-C-ee)
	 (M-escape . bind-M-escape)))
      (ct (make-char-table 'keymap))
      keys-list
      pkb-list-keys-outp)

  (cl-loop for (event . bind) in (append events-binds-for-ct events-binds-other)
  	   do (define-key km (vector event) bind))
  (cl-loop for (event . bind) in events-binds-to-esc
  	   do (define-key km (vector 27 event) bind))

  (cl-loop for (event . bind) in events-binds-other
	   do (push (list event nil bind) keys-list))
  (cl-loop for (event . bind) in events-binds-to-esc
	   do (push (list (event-convert-list (list 'meta event)) nil bind) keys-list))

  (cl-loop for (event . bind) in events-binds-other
	   collect (list event nil bind))
  
  (cl-loop for (event . bind) in events-binds-for-ct
  	   do (set-char-table-range ct event (list nil bind)))
  (set-char-table-range ct 27 `(nil (keymap ,events-binds-to-esc)))
  (setf pkb-list-keys-outp (pkb-list-keys km))
  (in-pr (cl-set-exclusive-or (car pkb-list-keys-outp) keys-list :test 'equal))
  ;; (equal (cdr pkb-list-keys-outp) ct)
  ;; (insert "\n")
  ;; (let ((print-circle nil))
  ;;   (in-pr (car pkb-list-keys-outp))
  ;;   (insert "\n")    
  ;;   (set-char-table-range (cdr pkb-list-keys-outp) 27 nil)
  ;;   (insert "\n")    
  ;;   (in-pr (cdr pkb-list-keys-outp))
  ;;   (insert "\n")
  ;;   (in-pr ct)
  ;;   )
  ;; (in-pr km)
  ;; (insert "\n\n")
  ;; (in-pr (pkb-list-keys km))
  (insert "\n")
  (in-pr-keymap (list 'keymap (cdr pkb-list-keys-outp)))
  (insert "\n\n")
  (in-pr-keymap (list 'keymap ct))
  
  (in-pr km)
  (insert "\n\n")
  (in-pr (pkb-list-keys km))
  )
nil
(("?\\C-a": (nil bind-C-a))
 ("?\\v": (nil bind-vv))
 ("?\\e": (nil (keymap (2 . bind-C-M-ab) (1 . bind-C-M-ab) (97 . bind-M-a))))
 ("(97 . 98)": (nil bind-ab))
 ("?c": (nil bind-c)))

(("?\\C-a": (nil bind-C-a))
 ("?\\v": (nil bind-vv))
 ("?\\e": (nil (keymap ((97 . bind-M-a) (1 . bind-C-M-ab) (2 . bind-C-M-ab)))))
 ("?a": (nil bind-ab))
 ("?b": (nil bind-ab))
 ("?c": (nil bind-c)))(keymap #^[nil nil keymap #1=
#^^[3 0 nil bind-C-a nil nil nil nil nil nil nil nil nil bind-vv nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil (keymap (2 . bind-C-M-ab) (1 . bind-C-M-ab) (97 . bind-M-a)) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil bind-ab bind-ab bind-c nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] #^^[1 0 #^^[2 0 #1# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] (M-escape . bind-M-escape) (67108891 . bind-C-ee))

(((134217825 nil bind-M-a) (134217729 nil bind-C-M-ab) (134217730 nil bind-C-M-ab) (67108891 nil bind-C-ee) (M-escape nil bind-M-escape)) . #^[nil nil keymap #2=
#^^[3 0 nil (nil bind-C-a) nil nil nil nil nil nil nil nil nil (nil bind-vv) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil (nil (keymap (2 . bind-C-M-ab) (1 . bind-C-M-ab) (97 . bind-M-a))) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil #1=(nil bind-ab) #1# (nil bind-c) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] #^^[1 0 #^^[2 0 #2# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil])




(let ((km (make-keymap))
      (events-binds-for-ct
       `((?a . bind-ab)
	 (?b . bind-ab)
	 (?c . bind-c)
	 (?\C-a . bind-C-a)
	 (?\v . bind-vv)
	 ;; (?\x334 . bind-xx334)
	 ))
      (events-binds-other
       `((?\M-a . bind-M-a)
	 (?\C-\M-a . bind-C-M-ab)
	 (?\C-\M-b . bind-C-M-ab)
	 (?\C-\e . bind-C-ee)
	 (M-escape . bind-M-escape)))
      (ct (make-char-table 'keymap))
      (pkb-list-keys-outp))
  (cl-loop for (event . bind) in (append events-binds-for-ct events-binds-other)
  	   do (define-key km (vector event) bind))
  (cl-loop for (event . bind) in events-binds-for-ct
  	   do (set-char-table-range ct event (list nil bind)))
  (set-char-table-range ct 27 `(nil 'keymap ))
  (setf pkb-list-keys-outp (pkb-list-keys km))
  (in-pr (cl-set-exclusive-or (car pkb-list-keys-outp)
  		       (cl-loop for (event . bind) in events-binds-other
  				collect (list event nil bind)) :test 'equal))
  ;; (equal (cdr pkb-list-keys-outp) ct)
  ;; (insert "\n")
  ;; (let ((print-circle nil))
  ;;   (in-pr (car pkb-list-keys-outp))
  ;;   (insert "\n")    
  ;;   (set-char-table-range (cdr pkb-list-keys-outp) 27 nil)
  ;;   (insert "\n")    
  ;;   (in-pr (cdr pkb-list-keys-outp))
  ;;   (insert "\n")
  ;;   (in-pr ct)
  ;;   )
  ;; (in-pr km)
  ;; (insert "\n\n")
  ;; (in-pr (pkb-list-keys km))
  (insert "\n")
  (in-pr-keymap (list 'keymap (cdr pkb-list-keys-outp)))
  (insert "\n\n")
  (in-pr-keymap (list 'keymap ct))
  )
nil
(("?\\C-a": (nil bind-C-a))
 ("?\\v": (nil bind-vv))
 ("?\\e": (nil (keymap (2 . bind-C-M-ab) (1 . bind-C-M-ab) (97 . bind-M-a))))
 ("(97 . 98)": (nil bind-ab))
 ("?c": (nil bind-c)))

(("?\\C-a": (nil bind-C-a))
 ("?\\v": (nil bind-vv))
 ("?a": (nil bind-ab))
 ("?b": (nil bind-ab))
 ("?c": (nil bind-c)))



(in-pr
 (let ((km (make-keymap)))
   (define-key km [?\M-\e] 'bind01)
   (define-key km [?a] 'bind02)
   (define-key km [?\C-a] 'bind03)
   (define-key km [?\C-q] 'bind04)
   (define-key km [?f] 'bind05)
   (define-key km [?g] 'bind06)
   (define-key km [?\M--] 'bind07)
   (define-key km [?\M-+] 'bind08)
   (define-key km [?e] 'bind12)
   (define-key km [(?1 . ?9)] 'bind09)
   (define-key km [?\[] 'bind10)
   (define-key km [prior] 'bind11)
   (define-key km [C-prior] 'bind13)
   (pkb-list-keys km nil t)
   ))

(((134217755 nil bind01) (134217773 nil bind07) (134217771 nil bind08) (prior nil bind11) (C-prior nil bind13)) . #^[nil nil keymap #2=
#^^[3 0 nil (nil bind03) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil (nil bind04) nil nil nil nil nil nil nil nil nil (nil (keymap (43 . bind08) (45 . bind07) (27 . bind01))) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil #1=(nil bind09) #1# #1# #1# #1# #1# #1# #1# #1# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil (nil bind10) nil nil nil nil nil (nil bind02) nil nil nil (nil bind12) (nil bind05) (nil bind06) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] #^^[1 0 #^^[2 0 #2# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil])

(let ((km (make-keymap)))
  (define-key km [(?1 . ?9)] 'bind9)
  (let* ((list-keys (pkb-list-keys km nil t))
	 (categorized-keys
	  (pkb-categorize-key-list list-keys 11 13)))
    (in-pr categorized-keys)
    ;; (in-pr-map 'map-char-table (nth 1 list-keys))
    )
)

;; pkb-translate-events

(let* ((55-mods-type-w-binds `((nil bind09)))
       (bks-w-binds `((55 nil . ,55-mods-type-w-binds))))
  bks-w-binds)


;;  To test



(let* (;; setup
       (1-bind-w-opt `(bind-1 . bind-opt-1))

       ;; arguments
       (bks-w-binds (copy-tree `((?1 bk-opt-1 (nil . ,1-bind-w-opt)))))
       (binds-char-table (make-char-table 'keymap))
       (translate-events-list '((?1 (next ?\C-L) t)))

       ;; expected results
       (bks-w-binds-expctd (copy-tree bks-w-binds))
       (binds-char-table-expctd (copy-sequence binds-char-table)))

  (pkb-translate-events 'bks-w-binds binds-char-table translate-events-list t nil)

  ;;  set bks-w-binds-expctd
  (setq bks-w-binds-expctd (assq-delete-all ?1 bks-w-binds-expctd))
  (add-to-list 'bks-w-binds-expctd `(next nil (nil . ,1-bind-w-opt)) t)
  ;;  set binds-char-table-expctd
  (set-char-table-range binds-char-table-expctd ?\C-L `(nil . ,1-bind-w-opt))
  (and (equal binds-char-table binds-char-table-expctd)
       (equal bks-w-binds bks-w-binds-expctd)))



(let* (;; setup
       (55-mods-type-w-binds `(((control) bind09 . opt)))

       ;; stuff to translate
       (bks-w-binds-orig `((55 options . ,55-mods-type-w-binds)))
       (bks-w-binds (copy-tree bks-w-binds-orig))
       (binds-char-table (make-char-table 'keymap))
       (translate-events-list '((67108919 (next 12) t)))

       ;; manually produced results
       bks-w-binds-manual
       binds-char-table-manual)
  ;; (set-char-table-range binds-char-table 27 'defn-of-esc)
  (pkb-translate-events 'bks-w-binds binds-char-table translate-events-list
			t nil)

  ;;  manually set bks-w-binds-manual
  (setq bks-w-binds-manual (copy-tree bks-w-binds-orig))
  (setq bks-w-binds-manual (assq-delete-all 55 bks-w-binds-manual))
  (add-to-list 'bks-w-binds-manual `(next nil . ,55-mods-type-w-binds) t)
  ;;  manually set binds-char-table-manual
  (setq binds-char-table-manual (make-char-table 'keymap))
  (set-char-table-range binds-char-table-manual 12 55-mods-type-w-binds)
  ;; (in-pr (char-table-range binds-char-table 12))
  (in-pr binds-char-table))


(let* (;; setup
       (55-mods-type-w-binds `(((control) bind09 . opt)))

       ;; stuff to translate
       (bks-w-binds-orig `((55 nil . ,55-mods-type-w-binds)))
       (bks-w-binds (copy-tree bks-w-binds-orig))
       (binds-char-table (make-char-table 'keymap))
       (translate-events-list '((67108919 (next 12) t))))
  ;; (set-char-table-range binds-char-table 27 'defn-of-esc)
  (pkb-translate-events 'bks-w-binds binds-char-table translate-events-list
			t nil)
  (in-pr binds-char-table))#^

[nil nil keymap #1=
#^^[3 0 nil nil nil nil nil nil nil nil nil nil nil nil (bind09 . opt) nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] #^^[1 0 #^^[2 0 #1# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]





((next nil (nil bind09 . opt)))






  ;; (setq )
  ;; (equal bks-w-binds `((next nil . ,55-mods-type-w-binds)))
  ;; (equal)

  

  ;; (equal bks-w-binds bks-w-binds-manual)
  ;; (in-pr binds-char-table)
  ;; (setq binds-char-table-manual (make-char-table 'keymap))
  ;; (set-char-table-range binds-char-table-manual 12 )
  ;; (setq )
  ;; (equal bks-w-binds `((next nil . ,55-mods-type-w-binds)))
  ;; (equal)
  ;; (in-pr bks-w-binds)
  ;; (insert "\n")
  ;; (in-pr-map 'map-char-table binds-char-table)
  ;; )

((next nil (nil bind09)))
((12: (bind09))
)




()


((next nil (nil bind09)))
((12: (bind09)))

((12: (bind09)))


(let ((bks-w-binds
       '((55 nil (nil bind09) ((meta) bind07))
	 (56 nil (nil bind09))
	 (57 nil (nil bind09) ((meta) bind08))
	 (91 nil (nil bind10)
	     ((control meta) bind01)
	     ((control) .
	      #1=(keymap (57 . bind08) (55 . bind07) (27 . bind01))))
	 (97 nil ((control) bind03) (nil bind02))
	 (102 nil (nil bind05))
	 (103 nil (nil bind06))
	 (113 nil ((control) bind04))
	 ((49 . 54) nil (nil bind09))))
      (binds-char-table (make-char-table 'keymap))
      (translate-events-list '((55 (next 12) t) (57 (C-prior)))))
  ;; (set-char-table-range binds-char-table 27 'defn-of-esc)
  (pkb-translate-events 'bks-w-binds binds-char-table  translate-events-list
		     t nil)
  (in-pr bks-w-binds)
  (insert "\n")
  (in-pr-map 'map-char-table binds-char-table)
  )
((prior nil ((control) . #1=(bind09))) (next nil (nil bind09)) (escape nil ((meta) bind01) (nil keymap (57 . bind08) (55 . bind07) (27 . bind01))) (55 nil ((meta) bind07)) (56 nil (nil bind09)) (57 nil (nil . #1#) ((meta) bind08)) (91 nil (nil bind10)) (97 nil ((control) bind03) (nil bind02)) (102 nil (nil bind05)) (103 nil (nil bind06)) (113 nil ((control) bind04)) ((49 . 54) nil (nil bind09)))
((12: (bind09))
)








;; PKB-TRANSLATE-SIMP
(let
    ((mks
      '((?\C-d nil bind1)
	(?e nil bind2)
	(55 nil bind3)
	(up nil bind4)
	(27 nil esc-def)
	(91 nil lbra-def)
	(57 nil 57-def)
	(?\M-\e nil other-def)))
     (translate-events-list '((55 (next 12) t) (57 (C-prior)))))
  (in-pr (pkb-translate-simp mks 50 translate-events-list t))
)


;;; pkb-categorize-key-list

(ert-deftest pkb-test-categorize-key-list ()
  "Test `pkb-categorize-key-list'"
  )

(let ((translate-events-list '((f5 (next 12) t) (57 (C-prior)))))
  (in-pr (pkb-categorize-key-list (pkb-list-keys (current-local-map)) 7 11
				  translate-events-list)))
(simple (escape nil . #1=((keymap (keymap (17 . indent-pp-sexp) (24 . eval-defun) (9 . completion-at-point)) keymap (keymap (17 . indent-sexp)) keymap (17 . prog-indent-sexp)))) (127 nil backward-delete-char-untabify) (27 nil . #1#) (3 nil (keymap (100 . duplicate-line))) (134217737 nil completion-at-point) (134217752 nil eval-defun) (134217745 nil indent-pp-sexp))


(let ((translate-events-list '((f5 (weird 444) t) (?\C-m (C-H-prior)))))
  (in-pr (pkb-categorize-key-list (pkb-list-keys (current-global-map) nil t) 7 11 translate-events-list)))

(in-pr-map 'map-char-table
	   (cddr (pkb-categorize-key-list
		   (pkb-list-keys (current-global-map) nil t) 7 11)))

(let ((km (make-keymap)))
  (define-key km [?\M-\e] 'bind01)
  (define-key km [?a] 'bind02)
  (define-key km [?\C-a] 'bind03)
  (define-key km [?\C-q] 'bind04)
  (define-key km [?f] 'bind05)
  (define-key km [?g] 'bind06)
  (define-key km [?\M--] 'bind07)
  (define-key km [?\M-+] 'bind08)
  (define-key km [?e] 'bind12)
  (define-key km [(?1 . ?9)] 'bind09)
  (define-key km [?\[] 'bind10)
  (define-key km [prior] 'bind11)
  (define-key km [C-prior] 'bind13)
  (let ((list-keys (pkb-list-keys km nil t)))
    (in-pr (pkb-categorize-key-list list-keys 6 13)))
)

(let ((km (make-keymap)))
  (define-key km [(?1 . ?9)] 'bind9)
  (let* ((list-keys (pkb-list-keys km nil t))
	 (categorized-keys
	  (pkb-categorize-key-list list-keys 11 13)))
    (in-pr categorized-keys)
    ;; (in-pr-map 'map-char-table (nth 1 list-keys))
    )
)


(let ((proc-list-keys
       (pkb-categorize-key-list (pkb-list-keys (current-global-map) nil t)
				7 13)))
  (in-pr proc-list-keys))

;; PKB-SPLIT-FULL-TO-GROUPS

(let* ((list-keys (pkb-list-keys (current-global-map) nil t))
       (categorized-keys
	(pkb-categorize-key-list list-keys 6 13 pkb-translate-events-list))
       (split-for-full
	(pkb-split-full-to-groups (cdr categorized-keys) pkb-key-groups))
       )
  (in-pr split-for-full)
)

;; PKB-BKS-W-BINDS-TO-BLOCKS-PRELIM

(in-pr
 (pkb-bks-w-binds-to-blocks-prelim
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
     ((49 . 54) nil (nil bind09)))
  5
  )
)
(((55 nil (nil bind09) ((meta) bind07))
  (next nil (nil bind09))
  (57 nil (nil bind09) ((meta) bind08))
  (91 nil (nil bind10) ((control meta) bind01) ((control) #1=(keymap (57 . bind08) (55 . bind07) (27 . bind01))))
  (97 nil ((control) bind03) (nil bind02)))
 ((102 nil (nil bind05))
  (mouse-1 nil ((control click) bind12) ((down) bind13))
  (mouse-2 nil ((control click) bind06) ((down) bind11))
  (113 nil ((control) bind04))
  (escape nil (nil #1#) ((meta) bind01)))
 (((49 . 54) nil (nil bind09))))

;; `pkb-process-blocks'

(in-pr
 (pkb-process-blocks
  (pkb-bks-w-binds-to-blocks-prelim
   (pkb-html-int-pr-bks
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
      ((49 . 54) nil (nil bind09)))
    nil nil nil nil nil)
   5
   ))
)

;; `pkb-add-options'

(defun tst-me ()
  (let ((list-to-test
	 '((nil . "abc")
	   (nil . (outp-str . "cde"))
	   ("qrt" . (outp-str . "tft"))
	   (((outp-str . "ghj")) . "trt")
	   (((outp-str . "ghj")) . (esc-map-fl . t))
	   (((esc-map-fl .  t)) . (outp-str . "ghj"))
	   (((esc-map-fl . t)) . (anchor-name . "tjf"))
	   (((esc-map-fl . t) (other-one . "a")) . (anchor-name . "tjf"))
	   (((esc-map-fl . t) (other-one . "b")) . (other-one . "abde")))))
    (insert "(")
    (pkb-dolist-cons (elem-cons list-to-test)
      (insert
       (concat 
	(unless (eq elem-cons list-to-test)
	  " ")
	"("
	(prin1-to-string (caar elem-cons))
	", "
	(prin1-to-string (cdar elem-cons))
	" -> "))
      (insert
       (concat
	(prin1-to-string
	 (pkb-update-options (caar elem-cons) (cdar elem-cons)))
	")"
	(when (cdr elem-cons)
	  "\n")))))
  (insert ")")
)

(tst-me)

;; PKB-KEY-SEQUENCE-LOCATION

(defun tst-me ()
  (let ((list-to-test
	 '([?\M-\e]
	   [?q ?a ?\M-e]
	   [?q ?a M-next]
	   [?t C-esc])))
    (insert "(")
    (pkb-dolist-cons (elem-cons list-to-test)
      (insert
       (concat 
	(unless (eq elem-cons list-to-test)
	  " ")
	"("
	(prin1-to-string (car elem-cons))
	" -> "
	(prin1-to-string (pkb-key-sequence-location (car elem-cons)))
	")"
	(when (cdr elem-cons)
	  "\n")))))
  (insert ")")
)

(tst-me)
