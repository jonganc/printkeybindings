;; ************************************************
;; pkb

(require 'cl)

;;; pkb-concat=
(ert-deftest pkb-test-pkb-concat= ()
  "Tests `pkb-concat='"
  (should
   (equal
    (let ((txt "abc"))
      (pkb-concat= 'txt "123")
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

;;; pkb-char-table-event-p
(ert-deftest pkb-test-char-table-eventp ()
  "Test `pkb-char-table-event-p'"
  (should (equal (pkb-char-table-event-p ?\C-a) t))
  (should (equal (pkb-char-table-event-p ?\e) t))
  (should (equal (pkb-char-table-event-p ?a) t))
  (should (equal (pkb-char-table-event-p 'prior) nil))
  (should (equal (pkb-char-table-event-p 'right) nil)))



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
  (let* ((a-map '(keymap (?a . binding1) (?b . binding2)))
	 (b-b-map '(keymap (?a . binding4)))
	 (b-map `(keymap (?a . binding3) (?b . ,b-b-map)))
	 (c-ctrl-w-map '(keymap (?a . binding5)))
	 (c-map `(keymap (?a . binding5) (?\C-w . ,c-ctrl-w-map)))
	 (root-map `(keymap (?a . ,a-map) (?b . ,b-map) (?c . ,c-map )))
	 )
    (should
     (equal
      (pkb-accessible-keymaps root-map)
      `(([?c ?\C-w] nil . ,c-ctrl-w-map)
	([?b ?b] nil . ,b-b-map)
	([?c] nil . ,c-map)
	([?b] nil . ,b-map)
	([?a] nil . ,a-map)
	([] nil . ,root-map)))))
  
  (let* ((1-map `(keymap (1 . binding) (2 . binding2)))
	 (2-2-map `(keymap (1 . binding4)))
	 (2-map `(keymap (1 . binding3) (2 . ,2-2-map)))
	 (3-2-2-map `(keymap (1 . bind)))
	 (3-2-map `(keymap (1 . binding5) (2 . ,3-2-2-map)))
	 (3-3-2-map `(keymap (ccba . bind)))
	 (3-3-map `(keymap (1 . binding5) (2 . ,3-3-2-map)))
	 (3-map `(keymap (1 . binding5) (2 . ,3-2-map) (3 . ,3-3-map)))
	 (root-map `(keymap (1 . ,1-map) (2 . ,2-map) (3 . ,3-map))))
    (should
     (equal (pkb-accessible-keymaps root-map '([2] [3]) '([2 2] ([3 2])))
	    `(([2] nil . ,2-map)
	      ([3 3 2] nil . ,3-3-2-map)
	      ([3 2 2] nil . ,3-2-2-map)
	      ([3 3] nil . ,3-3-map)
	      ([3] nil . ,3-map)))))

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
