;; 16/04/14: not used (apparently)
(defun pkb-add-to-end-of-list (list-symb object last-cons-symb) ;; OK
  "Add OBJECT to end of LIST-SYMB quickly by using the last cons cell
LAST-CONS-SYMB in LIST-SYMB. To work properly, last-cons-symb must a
variable that keeps its state after the macro is called.

The return value is the last cons cell of the list.

This command can also be used to add to a list that is not a
symbol. For this to work, one must manually start the list and set
LAST-CONS-SYMB. Then, call the command using using 't' for LIST-SYMB."
  (if (and (boundp list-symb) (symbol-value list-symb))
      (progn (setcdr (symbol-value last-cons-symb) (list object))
	     (set last-cons-symb (cdr (symbol-value last-cons-symb))))
    (set list-symb (list object))
    (set last-cons-symb (symbol-value list-symb))))

;;; pkb-add-to-end-of-list
(ert-deftest pkb-test-add-to-end-of-list ()
  "Tests `pkb-add-to-end-of-list'"
  (should
   (equal
    (let (tst-lst last-cons)
      (pkb-add-to-end-of-list 'tst-lst 'a 'last-cons)
      (pkb-add-to-end-of-list 'tst-lst '(g 56) 'last-cons)
      tst-lst)
    '(a (g 56)))))

;; 16/04/14: Seems unused
(defun pkb-esc-map-p (key-sequence) ;; OK
  "Return t if the last element of the vector KEY-SEQUENCE is an ESC"
  (and (not (equal key-sequence []))
       (equal 27 (aref key-sequence (- (length key-sequence) 1))))
)

;;; pkb-esc-map-p
(ert-deftest pkb-test-esc-map-p ()
  "Test `pkb-esc-map-p'"
  (should (equal (pkb-esc-map-p `[?a ?b]) nil))
  (should (equal (pkb-esc-map-p `[?b ?\C-r ?\e]) t)))


;;; 16/04/20 seems unused. pkb--fix-m-esc-in-key-sequences
(defun pkb--fix-m-esc-in-key-sequences (vector-in) ;; OK
  "An internal function used `pkb-accessible-keymaps'."
  ;; Replace M-ESC (actually '134217755') in VECTOR-IN with '27 27'.  This
  ;; issue comes up since `accessible-keymaps' converts consecutive escs to
  ;; 134217755 (i.e. M-ESC).
  (if
   ;; if '134217755' is an element of VECTOR-IN...
      (catch 'quit (dotimes (i (length vector-in))
		     (if (equal (aref vector-in i) 134217755)
			 (throw 'quit t)))
	     nil)
   ;; then replace it
      (let (list-eqv)
      (dotimes (i (length vector-in))
	(if (equal (aref vector-in i) 134217755)
	    (setq list-eqv (append list-eqv '(27 27)))
	  (setq list-eqv (append list-eqv (list (aref vector-in i))))))
	(vconcat list-eqv))
    vector-in))

(ert-deftest pkb-test--fix-m-esc-in-key-sequences ()
  "Test `'pkb--fix-m-esc-in-key-sequences'"
  (should (equal (pkb--fix-m-esc-in-key-sequences [?a ?b]) [?a ?b]))
  (should (equal (pkb--fix-m-esc-in-key-sequences [?b ?\C-r ?\e]) [?b ?\C-r ?\e]))
  (should (equal (pkb--fix-m-esc-in-key-sequences [?b ?\e ?\e ?h ?\e ?\e]) [?b ?\e ?\e ?h ?\e ?\e]))
  (should (equal (pkb--fix-m-esc-in-key-sequences [?b ?\M-\e ?h ?\M-\e]) [?b ?\e ?\e ?h ?\e ?\e])))


(defun pkb--find-event-defn (key-list event) ;; OK
  "Search for the definition of EVENT in KEY-LIST (i.e. a cons cell
\(MKS . BINDS-CHAR-TABLE) ), returning (MK-OPTIONS BIND . BIND-OPTIONS) or
nil (if no binding for EVENT).

EVENT should be a single event, not a char-range."
  (when (consp event)
    (error "event %s should be not be a char-range" event))
  (if (pkb-char-event-p event)
      (char-table-range (cdr key-list) event)
    (cdr (assoc event (car key-list))))
)
