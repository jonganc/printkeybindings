;;; pkb --- tools to print keymap bindings in a pretty fashion
;;; v.0.3

;; Copyright (C) 2011 Jonathan Ganc
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(require 'cl-lib)

(defmacro pkb-push-end (newelt place)
  "Add NEWELT to the end of list at PLACE."
  ;; if to-list is null, make a new list
  
  `(if (null ,place)
       (setf ,place (list ,newelt))
    (nconc ,place (list ,newelt))))

(defmacro pkb-concat= (variable &rest txt-to-add) ;; OK
  "Add the text TXT-TO-ADD to the string stored in VARIABLE"
  `(setf ,variable (concat ,variable ,@txt-to-add)))

(defcustom pkb-include-base-key-list ;; OK
  (let (cur-list mouse-pref-only mouse-event-w-pref mouse-event-w-pref-num)
    (setq cur-list 
      (append cur-list
	      '(return tab escape backspace delete insert home end prior next
		       right down left up)))
    ;; function keys
    (dolist (num (number-sequence 1 12))
      (add-to-list 'cur-list
    		   (intern (concat "f" (number-to-string num))) t))
    (setq cur-list
      (append cur-list
	'(
	  lwindow rwindow apps kp-down kp-add kp-begin kp-decimal
	  kp-delete kp-divide kp-down kp-end kp-enter kp-equal kp-home
	  kp-insert kp-left kp-multiply kp-next kp-prior kp-right
	  kp-separator kp-space kp-subtract kp-tab kp-up)))
    ;; number keys
    (dolist (num (number-sequence 1 9) cur-list)
      (add-to-list 'cur-list
    		   (intern (concat "kp-" (number-to-string num))) t))
    ;; mouse events
    (setq cur-list (append
		    cur-list
		    (list
		     (when (boundp 'mouse-wheel-up-event) mouse-wheel-up-event)
		     (when (boundp 'mouse-wheel-down-event) mouse-wheel-down-event))
			   '(mouse-1 mouse-2 mouse-3 mouse-4)))
    cur-list
   )
  "A list of the non-ascii base-keys that should be included (ascii base-keys will automatically be included).

The order of this list is used by `pkb-compare-base-key' to sort base-keys"
)

(defcustom pkb-include-keyboard-modifier ;; OK
  '(shift control meta alt)
  "A list of the keyboard modifiers that should be included.

The order of this list is used by `pkb-compare-base-key' to sort base-keys"
)

(defcustom pkb-include-mouse-modifier ;; OK
  '(click down double drag triple)
  "A list of the mouse modifiers that should be included.

The order of this list is used by `pkb-compare-base-key' to sort base-keys"
)

(defun pkb-char-event-p (event) ;; OK
  "Return t if EVENT is a character event, either a single character or a cons
cell of two characters (which is sometimes returned by map-keymap acting on a
char-table."
  (if (consp event)
	(and (characterp (car event)) (characterp (cdr event)))
      (characterp event)))

(defun pkb-include-mk-p (modified-key) ;; OK
  "Return t if MODIFIED-KEY (e.g. C-g) should be included in the output. If a
vector is passed, return t if every element of vector should be included in
the output."
  (if (not (vectorp modified-key))
    ;; if modified-key is not a vector
    (cond
     ;; if [t] (i.e. a default value)
     ((eq modified-key t))
     ;; keep if MODIFIED-KEY is a range with cdr less 4194303, the maximum
     ;;  allowed value for a character in a range
     ((and (consp modified-key))
      (<= (car modified-key) 4194303))
     ((let* ((mods-type (event-modifiers modified-key))
	     (basic-type (event-basic-type modified-key))
	     (flag t)
	     (pkb-include-modifier
	      (append pkb-include-keyboard-modifier
		      pkb-include-mouse-modifier)))
      (and
       (or
	;; is it a printable ascii character?
	(and (integerp basic-type)
	     (>= basic-type 32)
	     (<= basic-type 127)) 
	;; or is it another physical key
	(memq basic-type pkb-include-base-key-list))
       ;; are the modifiers OK?
       ;; note that the following construction is true if modifiers is nil
       (dolist (elem mods-type flag)
	 (setq flag (and (memq elem 
	   (append pkb-include-keyboard-modifier pkb-include-mouse-modifier))
			 flag)))))))
    ;; else modified key is a vector
    (let ((flag t))
      (dotimes (i (length modified-key) flag)
	(setq flag (and flag (pkb-include-mk-p (aref modified-key i)))))))
)

(defun pkb-compare-key-sequences (ks1 ks2) ;; OK
  "Return non-nil if the key-sequence (given as a vector) KS1 should precede KS2 in a sort.

The key-sequences are compared item by item, using
`pkb-compare-modified-keys'. If KS1 and KS2 start with the same events
except that KS1 is shorter than KS2, then KS1 precedes KS2."
  (let ((idx 0))
  (catch 'quit
    (while t
      (if (<= (length ks1) idx)
	  (throw 'quit t)
	(if (<= (length ks2) idx)
	    (throw 'quit nil)
	  ;; (elt ks1 idx), (elt ks2 idx) both defined
	  (if (not (equal (elt ks1 idx) (elt ks2 idx)))
	      (throw 'quit
		  (pkb-compare-modified-keys (elt ks1 idx) (elt ks2 idx))))
	  ))
      (setq idx (1+ idx)))))
)

(defun pkb-compare-modified-keys (mk1 mk2) ;; OK
  "Return non-nil if the modified key MK1 should precede MK2 in a sort. Char-ranges sort as the first character in the range.

The sorting of mks is dependent on the order of items in the list (reverse (append pkb-include-keyboard-modifier pkb-include-mouse-modifier)). MK1 and MK2 are sequentially checked to see if they have each modifier in the list. If MK2 has a modifier and MK1 does not then MK1 procedes MK2. If MK1 and MK2 have the same modifiers, `pkb-compare-base-key' is used to compare their base-key.

Intended to be used by `pkb-compare-key-sequences'. "
  (when (consp mk1) (integerp (car mk1)) (integerp (cdr mk1))
	(setq mk1 (car mk1)))
  (when (consp mk2) (integerp (car mk2)) (integerp (cdr mk2))
	(setq mk2 (car mk2)))
  (let (mk1-mod mk2-mod)
    ;; the order in which modified keys should be listed 
    ;; catch will be used to exit and return an appropriate value
    (catch 'quit 
      ;; first check if one of the mks should be first because of the
      ;;  modifier
      (setq mk1-mod (event-modifiers mk1))
      (setq mk2-mod (event-modifiers mk2))
      (dolist (modif (reverse
	   (append pkb-include-keyboard-modifier pkb-include-mouse-modifier)))
	(if (memq modif mk1-mod)
	    (if (not (memq modif mk2-mod))
		;; MK2 < MK1 if MODIF in MK1 and not in MK2
		(throw 'quit nil)) 
	  (if (memq modif mk2-mod)
	      ;; MK1 < MK2 if MODIF in MK2 and not in MK1
	      (throw 'quit t))
	  ))
      (pkb-compare-base-keys
       (event-basic-type mk1) (event-basic-type mk2))))
)

(defun pkb-compare-modifier-lists (mods1 mods2)
  "Return non-nil if the list of modifiers MODS1 should precede MODS2 in a sort, as in `pkb-compare-modified-keys'."
  (catch 'quit 
    (dolist (modif (reverse
	(append pkb-include-keyboard-modifier pkb-include-mouse-modifier)))
      (if (memq modif mods1)
	  (if (not (memq modif mods2))
	      (throw 'quit nil)) ;; MK2 < MK1 if MODIF in MK1 and not in MK2
	(if (memq modif mods2)
	    (throw 'quit t)) ;; MK1 < MK2 if MODIF in MK2 and not in MK1
	))
    nil)
)

(defun pkb-compare-base-keys (bk1 bk2) ;; OK
  "Return non-nil if the base-key BK1 should precede BK2 in a sort.

Integer events precede other events and smaller integers preceded larger.  ESC is sorted after any other integer-events. Otherwise, they are ordered according to `pkb-include-base-key-list'.

Intended to be used by `pkb-compare-modified-keys'"
  (catch 'quit
    (if (integerp bk1)
	(if (integerp bk2)
	    ;; ESC = ?\e should be after other integer-events
	    (cond ((equal bk1 ?\e) (throw 'quit nil))
		  ((equal bk2 ?\e) (throw 'quit t))
		  (t (throw 'quit (< bk1 bk2))))
	  (throw 'quit t))
      (if (integerp bk2)
	  (throw 'quit nil)))
    ;; neither keys are integers
    (let (bk1-pos)
      (setq bk1-pos (memq bk1 pkb-include-base-key-list))
      ;; is bk1 in list before bk2
      (if bk1-pos
	  (if (memq bk2 bk1-pos)
	      (throw 'quit t) ;; bk1 before bk2
	    ;; bk1 in list, bk2 not after bk1. Is bk2 in list at all?
	    (if (memq bk2 pkb-include-base-key-list)
		(throw 'quit nil)
	      (throw 'quit t)))
	    nil ;; if bk1 is not in list, then it shouldn't precede bk2
	    )
      ))
)

(defun pkb-accessible-keymaps (keymapi &optional include-list exclude-list)
  "Find all keymaps accessible via prefix-keys from KEYMAPI, except as
restricted by INCLUDE-LIST and/or EXCLUDE-LIST. The output will be in the
form (PK PK-OPTIONS . KEYMAP).

In contrast to the command `accessible-keymaps', 1) prefix-keys that are not
input events according `pkb-include-mk-p' will be excluded; 2) the output
will be unsorted; 3) if a pk is stored in the ESC key of the parent keymap,
the `esc-map-fl' key in PK-OPTIONS will be t.

If INCLUDE-LIST is not nil, only the prefix-keys it specifies will be
included. The basic syntax for INCLUDE-LIST is a list of the form (PK-1
... PK-n), where each PK-i is a prefix-key. In this case, this function will
return only keymaps accessible via prefix-keys from some PK-i \(inclusive). This
behavior can be adjusted by replacing PK-i with (list PK-i), which will include
PK-i but not keymaps accessible from PK-i (note that a prefix-key accessible
from an element (list PK-i) may still be included if it's accessible from a
different element of INCLUDE-LIST). E.g. INCLUDE-LIST given by ([?\C-x] [?\C-h])
means: \"Include all prefix-keys accessible from either 'C-x' and 'C-h'\", while
\( ([?C-x]) [?\C-h] ) means: \"Include 'C-x' and all prefix-keys accessible from
'C-h'\".
Any prefix-keys beginning with an element of EXCLUDE-LIST will not be
included. Exact-only matches can be included in EXCLUDE-LIST in the same way as in INCLUDE-LIST.
With respect to priority between INCLUDE-LIST and EXCLUDE-LIST, longer
prefix-key vectors take preference over shorter ones and then EXCLUDE-LIST
takes preference over INCLUDE-LIST, except that any exact-match elements in
INCLUDE-LIST will always be added."
  (let (access-kms-vrbl pks-included inc-exl-hierarchy inc-pk-exact)
   ;; ACCESS-KMS-VRBL will hold the return value.
   ;; PKS-INCLUDED will be a list of prefix-keys that have been
   ;;  considered for addition.
   ;; INC-EXC-HIERARCHY (or "INClude-EXclude-hierarchy") will be used to
   ;;   determine which prefix-keys to include and exclude. An efficient way
   ;;   of doing this is to establish a tree-like hierarchy among the PK's in
   ;;   INCLUDE-LIST. This is much easier to make than explain!! Higher
   ;;   branches will contain pks that start with the next lowest
   ;;   branch. E.g. if INCLUDE-LIST = ([?a] [?a ?c ?d] [?a ?r ?t] [?a ?c ?d
   ;;   ?p] [?\C-c] [?\C-c ?r]), there will be two trees:
   ;;              ([?a])		|	([?\C-c])
   ;;  ([?a ?c ?d])     ([?a ?r ?t])	|     ([?\C-c ?r])
   ;;  ([?a ?c ?d ?p])
   ;;  Then, each element EXC-PK in EXCLUDE-LIST is associated with the
   ;;   longest tree-element that begins with the same events as EXC-PK but
   ;;   is shorter than EXC-PK (if there is no such element, EXC-PK is
   ;;   irrelevant). For example, if INCLUDE-LIST is as above and
   ;;   EXCLUDE-LIST = ([?a ?c] [?\C-c ?q] [?\C-c ?r ?t] [?e]), the trees will look
   ;;   like:
   ;;         ([?a] [?a ?c])		|	([?\C-c] [?\C-c ?q])
   ;;  ([?a ?c ?d])    ([?a ?r ?t])	|     ([?\C-c ?r] [?\C-c ?r ?t]).
   ;;  ([?a ?c ?d ?p])
   ;;   This corresponds to INC-EXC-HIERARCHY =
   ;;       '( ( ([?a] [?a ?c])
   ;;            ( ([?a ?c ?d])
   ;;              ( ([?a ?c ?d ?p]) ) )
   ;;            ( ([?a ?r ?t]) ) )
   ;;          ( ([?\C-c] [?\C-c ?q])
   ;;            ([?\C-c ?r] [?\C-c ?r ?t]) ) )    .
   ;;   In other words, each branch of INC-EXC-HIERARCHY is a list with the
   ;;   structure:
   ;;   ( (INC-PK EXC-PK-1 ...) SUBBRANCH-1 ...),
   ;;   So that INC-EXC-HIERARCHY = (ROOT-1-BRANCH ...)
   ;;  Ultimately, we can simply use accessible-keymaps on each of the roots
   ;;   (e.g. [?a] and [?\C-c] for the above case) to find prospective keymaps,
   ;;   find the highest (i.e. longest) branch that matches a given keymap, and
   ;;   test if the associated elements from EXCLUDE-LIST exclude that
   ;;   keymap. If not, that keymap is OK to be include. E.g., considering a pk
   ;;   [?\C-c ?r ?e], we would look at the branch [?\C-c ?r] and accept the pk
   ;;   because it doesn't start with [?\C-c ?r ?t]; consider a pk [?a ?c ?l],
   ;;   we would look at the branch [?a] and reject the pk because it begins
   ;;   with [?a ?c].
   ;; INC-PK-EXACT will list pks that are included only exactly (i.e. are of
   ;;  the form (list PK-i) rather simply PK-i)

  (when (null include-list) (setq include-list (list [])))

  ;; populate INC-PK-EXACT and remove those elements from INCLUDE-LIST
  (dolist (inc-pk include-list)
    (when (listp inc-pk)
      (add-to-list 'inc-pk-exact (car inc-pk) nil 'ignore)
      (setq include-list (delq inc-pk include-list))))
  
  ;; now, decide which pks to include

  ;; generate INC-EXC-HIERARCHY
  (let (cur-pks-len cur-pks-list)
   ;; CUR-PKS-LEN will be used by the dolist to determine when the pks'
   ;;  length changes (in the sorted version of INCLUDE-LIST) and thus which
   ;;  pks need to be compared to see if one is the parent of another
   ;; CUR-PKS-LIST will be a list of the pks of length CUR-PKS-LEN
    ;; set initial conditions
    (setq include-list
	  (sort include-list
		(lambda (pk1 pk2) (> (length pk1) (length pk2)))))
    ;; Remember: a branch with one INC-PK (i.e. a pk to include) is of the
    ;;  form: ( (INC-PK) ), and we want CUR-PKS-LIST to be a list of such
    ;;  branches
    (when include-list
      (setq cur-pks-list (list (list (list (car include-list))))))
    (setq cur-pks-len (length (car include-list)))
    (dolist (inc-pk (cdr include-list))
      (when (/= (length inc-pk) cur-pks-len)
       ;; when length of INC-PK is different from CUR-PKS-LEN, update
       ;;  INC-EXL-HIERARCHY
	(setq inc-exl-hierarchy (append cur-pks-list inc-exl-hierarchy))
	(setq cur-pks-list nil)
	(setq cur-pks-len (length inc-pk)))
	;; are any of the longer pks subbranches of INC-PK?
      (let ((cur-branch (list (list inc-pk))))
	(dolist (branch inc-exl-hierarchy)
	  (when (equal (substring (caar branch) 0 cur-pks-len) inc-pk)
	    ;; when pk of branch is a subbranch of INC-PK?
	    (add-to-list 'cur-branch branch t 'ignore)
	    (setq inc-exl-hierarchy (delq branch inc-exl-hierarchy))))
	(add-to-list 'cur-pks-list cur-branch nil 'ignore)))
    ;; when INCLUDE-LIST has been exhausted, add remaining CUR-PKS-LIST to
    ;;  INC-EXL-HIERARCHY
    (setq inc-exl-hierarchy (append cur-pks-list inc-exl-hierarchy)))
  ;; Now, all (not-exactly-matching) elements of INCLUDE-LIST are included in
  ;;  INC-EXL-HIERARCHY.

  ;; now add elements of EXCLUDE-LIST to INC-EXL-HIERARCHY.
  (dolist (exc-pk-entry exclude-list)
    (let* (cur-branch
	   (exc-pk (if (listp exc-pk-entry) (car exc-pk-entry) exc-pk-entry))
	   (exc-pk-len (length exc-pk)))
    ;; first we will determine if EXC-PK falls within any INC-EXL-HIERARCHY
    ;;  roots Note that if EXC-PK does not fall within any root, then
    ;;  CUR-BRANCH will still be nil and EXC-PK will be ignored
    (catch 'quit
      (dolist (root-branch inc-exl-hierarchy)
	(when (and (<= (length (caar root-branch)) exc-pk-len)
		   (equal (caar root-branch)
			  (substring exc-pk 0 (length (caar root-branch)))))
	 ;; if EXC-PK falls within ROOT-BRANCH
	  (setq cur-branch root-branch)
	  (throw 'quit nil))))
    ;; only need to add EXC-PK if it falls with a root
    (while cur-branch
      (catch 'quit
	;; does EXC-PK lies in a subbranch of CUR-BRANCH?
	(dolist (subbranch (cdr cur-branch))
	  (let ((sub-pk (caar subbranch)))
	  (when (and (<= (length sub-pk) exc-pk-len)
		     (equal sub-pk (substring exc-pk 0 (length sub-pk))))
	    ;; when EXC-PK lies in SUBBRANCH
	    (setq cur-branch subbranch)
	    (throw 'quit nil))))
       ;; this point will only be reached in EXC-PK doesn't lie in any
       ;;  subbranch of CUR-BRANCH
	(setcdr (last (car cur-branch)) (list exc-pk-entry))
	;; quit while loop
	(setq cur-branch nil)))))
  ;; INC-EXL-HIERARCHY is now made

  ;; now make ACCESS-KMS-VRBL
  (dolist (hier-elt inc-exl-hierarchy)
    (dolist (pk-w-keymap (accessible-keymaps keymapi (caar hier-elt)))
      (setq pk-w-keymap (cons nil pk-w-keymap))
      (setcar pk-w-keymap (cadr pk-w-keymap))
      (setcar (cdr pk-w-keymap) nil)
      ;; PK-W-KEYMAP is now of form (PK PK-OPTIONS . KEYMAP)
      (let ((cur-pk (car pk-w-keymap)))
      ;; Convert repeated ESCs
      (when
	  ;; is CUR-PK allowed, i.e. is CUR-PK not in PKS-INCLUDED, allowed
	  ;;  by `pkb-include-mk-p', and not excluded by EXCLUDE-LIST?
	  (and
	   (not (member cur-pk pks-included))
	   (pkb-include-mk-p cur-pk)
	   ;; is CUR-PK not excluded by EXCLUDE-LIST?
	   (let ((cur-pk-len (length cur-pk)) (cur-branch hier-elt)
		 high-branch)
	   ;; first, find the highest branch including CUR-PK
	   (while cur-branch
	     ;; does CUR-PK lies in a subbranch of CUR-BRANCH?
	     (catch 'quit
	       (dolist (subbranch (cdr cur-branch))
		 (let ((sub-pk (caar subbranch)))
		 (when (and (<= (length sub-pk) cur-pk-len)
			    (equal sub-pk
				   (substring cur-pk 0 (length sub-pk))))
		  ;; when CUR-PK lies in SUBBRANCH
		   (setq cur-branch subbranch)
		   (throw 'quit nil))))
	       ;; when CUR-PK doesn't lie in any subbranches of CUR-BRANCH
	       (setq high-branch cur-branch)
	       (setq cur-branch nil)))
	   ;; HIGH-BRANCH is now the highest branch that includes CUR-PK
	   
	   ;; is CUR-PK excluded by any EXCLUDE-LIST elements in HIGH-BRANCH?
	   (catch 'quit
	     (dolist (exc-pk-entry (cdar high-branch))
	       ;; is CUR-PK excluded by EXC-PK-ENTRY?
	       (when
		   (if (listp exc-pk-entry)
		    ;; if EXC-PK-ENTRY is an exact-only entry, does it match
		    ;;  CUR-PK exactly?
		       (equal cur-pk (car exc-pk-entry))
		    ;; else, EXC-PK-ENTRY does not correspond to an
		    ;;  exact-only entry
		     (and (<= (length exc-pk-entry) cur-pk-len)
			  (equal exc-pk-entry
				 (substring cur-pk 0 (length exc-pk-entry))
				 )))
		;; when CUR-PK excluded by EXC-PK-ENTRY, return nil to 'and'
		 (throw 'quit nil)))
	    ;; when CUR-PK is not excluded, return t to 'and'
	     t)))
       ;; when CUR-PK is allowed
	(add-to-list 'access-kms-vrbl pk-w-keymap nil 'ignore))
      ;; regardless, we don't need to check CUR-PK again
      (add-to-list 'pks-included cur-pk nil 'ignore))))

  ;; now add any of the exact member of INCLUDE-LIST
  (dolist (pk-to-add inc-pk-exact)
    (unless (member pk-to-add pks-included)
      (let ((defn (lookup-key keymapi pk-to-add)))
	(when (keymapp defn)
	    (add-to-list 'access-kms-vrbl
			 (append (list pk-to-add) (cons nil defn))
			 nil 'ignore)))))
  ;; set `esc-map-fl'
  (dolist (pk-w-keymap access-kms-vrbl)
    ;; (let* ((pk-as-stored
    ;; 	    (pkb--fix-m-esc-in-key-sequences (car pk-w-keymap)))
    ;; 	   (pk-options
    ;; 	    (list (cons 'pk-as-stored pk-as-stored))))
    ;;   (when (equal (substring pk-as-stored -1) ?\e)
    ;; 	(add-to-list 'pk-options (list (cons 'esc-map-fl t))))	   
    ;;   (setcar (cdr pk-w-keymap) pk-options))
    (when (and (>= (length (car pk-w-keymap)) 1)
	       (member (substring (car pk-w-keymap) -1) '((?\e) (?\M-\e))))
      (setcar (cdr pk-w-keymap)
	      (list
	       (list (cons 'esc-map-fl t))))))
  ;; ACCESS-KMS-VRBL is now finished
  access-kms-vrbl)
)

;; For reference, this is a description of the print format of a char-table (note: a char-table will only be printed corrected if the variable `print-circle' is set to t; it's better to make char-tables using the char-table-specific functions, like make-char-table, anyway). 
;; #^[default-value parent subtype defn-1-(0 . -1) defn-2-(0 . 65535)
;;    defn-3-(65536 . 131071) ... defn-65-(4128768 . 4194303)]

(defun pkb-list-keys (input-keymap &optional esc-mapf ignore-sic-if-unmod)
  "List the modified-keys (mks) in INPUT-KEYMAP.
If ESC-MAPF, ignore events if they can be stored in a char-table (intended for
ignoring events in a non-root keymap whose last prefix key is ESC).

Returns (MKS . BINDS-CHAR-TABLE), where MKS, BINDS-CHAR-TABLE are described
in the comments at the top of the .el file. The bindings in BINDS-CHAR-TABLE
are not also in MKS.

If IGNORE-SIC-IF-UNMOD is non-nil, bindings of self-insert-command
will be ignored for unmodified keys.

Note that this function will properly treat keys with a Meta modifier
\(e.g. so that [27 ?b] -> M-b). "
  (let (mks events-w-defn-for-ct binds-char-table)
   ;; EVENTS-W-DEFN-FOR-CT is a list with elements (int-event . defn) to be
   ;;  used to construct BINDS-CHAR-TABLE.
  (map-keymap
   (lambda (event defn)
     ;; add events as long as they belong
     (when (and (pkb-include-mk-p event)
		;; ESC = 27 will be dealt with separately
		(not (equal event 27))
		;; if ESC-MAPF, integers or char-range events that don't
		;;  include meta modifiers need to be ignored
		(or (not esc-mapf)
		    (symbolp event)
		    (and (integerp event)
			 (memq 'meta (event-modifiers event))))
		;; if IGNORE-SIC-IF-UNMOD, only include if EVENT is modified
		;;  or DEFN is not `self-insert-command'
		(or (not ignore-sic-if-unmod)
		    (event-modifiers event)
		    (not (eq defn 'self-insert-command))))
       (if (pkb-char-event-p event)
	;; if EVENT can fit on a char-table
	   ;; Note that, when adding to EVENTS-W-DEFN-FOR-CT, the first
	   ;;  events mapped will be the the last ones added to
	   ;;  BINDS-CHAR-TABLE, preserving the correct priority for event
	   ;;  definitions, so that we can just use DEFN rather than having
	   ;;  to use `lookup-key'.
	   (push (cons event defn) events-w-defn-for-ct)
        ;; else EVENT cannot fit on a char-table and should be added
	 (pkb--add-event event))))
   input-keymap)
  
  (setq binds-char-table (make-char-table 'keymap))
  (dolist (event-w-defn events-w-defn-for-ct)
    (set-char-table-range binds-char-table
			  (car event-w-defn) (cdr event-w-defn)))
  ;; put the final values in BINDS-CHAR-TABLE in the proper form
  (map-char-table
   (lambda (event defn)
     (set-char-table-range binds-char-table event (list nil defn)))
   binds-char-table)

  ;; deal with ESC key. 
  (let ((esc-defn (lookup-key input-keymap [27])))
  (when esc-defn
  ;; add ESC key itself if needed
    (when (pkb-include-mk-p 27)
      (set-char-table-range binds-char-table 27 (list nil esc-defn)))
    (when (keymapp esc-defn)
      (map-keymap 
       (lambda (event defn)
	 (if (not (consp event))
	  ;; if event is not a char-range
	     (when (and (integerp event)
			(not (memq 'meta (event-modifiers event))))
	       ;; when event is an non-meta key integer...
	       (let ((meta-event
		      (event-convert-list (list 'meta event))))
	       (when (pkb-include-mk-p meta-event)
		;; when event supposed to be included, add it
		 (pkb--add-event meta-event))))
	  ;; else event is a char-range
	   ;; do a quick sanity check to make sure there won't be
	   ;;  too many events added
	   (when (> (- (cdr event) (car event)) 128)
	       (error (concat "In pkb-list-keys - trying to add a"
			 " very large char-range in the ESC map: %s")
		      event))
	   (dolist (int-event
		    (number-sequence (car event) (cdr event)))
	     (let ((meta-event
		    (event-convert-list (list 'meta int-event))))
	       (when (pkb-include-mk-p meta-event)
		;; and event supposed to be included
		 (pkb--add-event meta-event))))))
       esc-defn))))
  (cons mks binds-char-table))
)

(defun pkb--add-event (event &optional key-defn) ;; OK
  "An internal function used by `pkb-list-keys'

Add non-char-range EVENT to MKS if it has not yet been added.
If KEY-DEFN not provided, will look it up in INPUT-KEYMAP.

Note: This function uses global variables MKS, INPUT-KEYMAP."
  ;; add EVENT unless it has already been added
  (unless (assoc event mks)
    (unless key-defn
      (setq key-defn (lookup-key input-keymap (vector event))))
    (add-to-list 'mks (list event nil key-defn)))
)

(defun pkb-categorize-key-list
  (key-list max-mks-for-simple max-bks-for-compact
   &optional translate-events-list) ;; OK
  "(Destructively) convert KEY-LIST into a format appropriate for output.

The format for KEY-LIST is the same as the output from
`pkb-list-keys', i.e. (MKS . BINDS-CHAR-TABLE).

If MAX-BKS-FOR-COMPACT < 0, this function will never return format (3)
\(i.e. it will behave as if MAX-BKS-FOR-COMPACT = infinity).

The output will be a list in one the following formats, covering the
different ways of printing differently sized keymap:
1) ('simple . MKS), if the number of mks < MAX-MKS-FOR-SIMPLE. MKS now
  includes all events in BINDS-CHAR-TABLE;
2) ('compact . BKS-W-BINDS), if the number of bks <
  MAX-BKS-FOR-COMPACT. BKS-W-BINDS has all events, including those
  previously in BINDS-CHAR-TABLE.
3) ('full BKS-W-BINDS . BINDS-CHAR-TABLE), otherwise. An event from
  BINDS-CHAR-TABLE whose basic-type was originally in MKS will now be
  only in BKS-W-BINDS (events whose basic-type was not originally in
  MKS will still only be in BINDS-CHAR-TABLE).
4) nil, if KEY-LIST has no binds.

TRANSLATE-EVENTS-LIST is passed to `pkb-translate-events', `pkb-translate-simp'"

  (let* ((mks (car key-list)) (binds-char-table (cdr key-list))
	 (mks-tally (length mks)) tent-mks)
  ;; TENT-MKS (for "TENTative") is a list that will be used when it is
  ;;  unclear if the entries being processed will need to be added to MKS
    
  (catch 'outer-quit
  ;; are there few enough characters to have a simple keymap?
  (catch 'quit
    (when (> mks-tally max-mks-for-simple)
      (throw 'quit nil))
    (map-char-table
     (lambda (event defn)
       (setq mks-tally (1+ mks-tally))
       (when (> mks-tally max-mks-for-simple)
	;; if there are too many events for a simple keymap
	   (throw 'quit nil))
       (add-to-list 'tent-mks (append (list (copy-tree event)) defn)))
     binds-char-table)
    (if tent-mks
	(setcdr (last tent-mks) mks)
      (setq tent-mks mks))
    (setq tent-mks
	  (pkb-translate-simp tent-mks max-mks-for-simple
			   translate-events-list))
    ;; remember: (listp nil) = t
    (unless (listp tent-mks)
      (throw 'quit nil))
   ;; at this point, there are few enough events for a simple keymap...
    (if (equal (length tent-mks) 0)
	(throw 'outer-quit nil)
      (throw 'outer-quit (cons 'simple tent-mks))))

   ;; at this point, we've established there are too many events for a
   ;;  simple keymap
  (let (bks-w-binds (bks-tally 0) tent-bks-w-binds)
  ;; first, we convert the keys in MKS to BKS-W-BINDS
  (dolist (mk-w-bind mks)
    (let* ((event (car mk-w-bind))
	   (bind-w-opt (nthcdr 2 mk-w-bind))
	   (mods (event-modifiers event))
	   (basic-type (event-basic-type event))
	   (one-bk-w-binds (assoc basic-type bks-w-binds)))
    (if one-bk-w-binds
     ;; if BASIC-TYPE already in BKS-W-BINDS
	(setcdr (last one-bk-w-binds) (list (cons mods bind-w-opt)))
     ;; else BASIC-TYPE not yet in BKS-W-BINDS and we have to add it
      (setq one-bk-w-binds (list basic-type nil (cons mods bind-w-opt)))
      ;; we first add events from EVENTS-CHAR-TABLE with same BASIC-TYPE
      (when (integerp basic-type)
	(setq mods-to-check
	      (cond
	       ;; if bk, C-bk, S-bk fit in a char-table
	       ((and (>= basic-type 97) (<= basic-type 122))
		'(nil control shift))
	       ;; if bk, C-bk fit in a char-table
	       ((or (= basic-type 64)
		    (and (>= basic-type 91) (<= basic-type 95)))
		'(nil control))
	       ;; if only bk fits in a char-table
	       ((and (>= basic-type 0) (<= basic-type (max-char)))
		'(nil))
	       (t nil)))
	(dolist (one-mod mods-to-check)
	  (let ((mod-event
		 (if (null mods-to-check)
		     basic-type
		   (event-convert-list (list one-mod basic-type)))))
	    (when (char-table-range binds-char-table mod-event)
	      ;; when MOD-EVENT has an entry in BINDS-CHAR-TABLE
	      (setcdr (last one-bk-w-binds)
		      (list
		       (append
			(list (when one-mod (list one-mod)))
			;; we don't want MK-OPTIONS
			(cdr
			 (char-table-range binds-char-table mod-event)))))
	      (set-char-table-range binds-char-table mod-event nil)))))
      (add-to-list 'bks-w-binds one-bk-w-binds)
      (setq bks-tally (1+ bks-tally)))))

  ;; translate events
  (setq bks-tally (+ bks-tally
		     (pkb-translate-events 'bks-w-binds binds-char-table
					translate-events-list)))

  ;; process BINDS-CHAR-TABLE while seeing if there few enough keys for a
  ;;  compact keymap
  (catch 'quit
    (when (and (> bks-tally max-bks-for-compact) (>= max-bks-for-compact 0))
      (throw 'quit nil))
    (map-char-table
     (lambda (event defn)
       ;; we don't need MK-OPTIONS
       (setq defn (cdr defn))
       (let (asscn)
       (cond
	((consp event)
	 (add-to-list 'tent-bks-w-binds
		      (list (copy-tree event) nil
			    (cons nil defn)))
	 (setq bks-tally (1+ bks-tally)))
	;; if EVENT is in the range 64 - 122 (but not 96), there is a
	;;  smaller char-table event with the same basic type, so that the
	;;  basic-type might already have been defined
	((and (>= event 64) (<= event 122) (/= event 96)
	      (setq asscn (assoc (event-basic-type event) tent-bks-w-binds)))
	 (setcdr (last asscn)
		 (list (cons (event-modifiers event) defn))))
	(t
	 (add-to-list 'tent-bks-w-binds
		      (list (event-basic-type event) nil
			    (cons (event-modifiers event) defn)))
	 (setq bks-tally (1+ bks-tally)))))
       (when (and (> bks-tally max-bks-for-compact)
		  (>= max-bks-for-compact 0))
	 (throw 'quit nil)))
     binds-char-table)
    ;; at this point, we can have a compact keymap
    (if bks-w-binds
	(progn
	  (setcdr (last tent-bks-w-binds) bks-w-binds)
	  (setq bks-w-binds tent-bks-w-binds))
      (setq bks-w-binds tent-bks-w-binds))
    (throw 'outer-quit (cons 'compact bks-w-binds)))
  ;; then there are too many keys for a compact keymap
  (append (list 'full) (cons bks-w-binds binds-char-table)))))
)

(defun pkb-translate-events
  (bks-w-binds-symb binds-char-table
   &optional translate-events-list do-not-keep-esc-in-lbr do-not-translate-lbr-to-esc) ;; OK
  "Translate events (i.e. move or copy bindings) in
BKS-W-BINDS-SYMB (passed as a symbol) and BINDS-CHAR-TABLE, both
of which may be altered. The returned value is the net change in
the length of BKS-W-BINDS-SYMB.

Unless DO-NOT-TRANSLATE-LBR-TO-ESC (\"lbr\"=\"line break\") is
non-nil, this function will copy events of C-[ = 27 to escape,
since that is how they are interpreted. If DO-NOT-KEEP-ESC-IN-LBR
is non-nil, such events will be moved instead of copied (the
latter option is ignored if the former option is non-nil).

TRANSLATE-EVENTS-LIST allows certain events to be copied to other
events \(e.g. delete, M-delete is usually translated to C-d,
M-DEL, respectively, so it makes sense to copy the bindings). It
is a list of elements (EVENT COPY-LIST DELETE-ORIG-FL). EVENT is
the event to translate and COPY-LIST is a list of the events to
copy the binding to. If DELETE-ORIG-FL is non-nil then the
original event is deleted."
  (let ((change-in-bks-length 0))
  (unless do-not-translate-lbr-to-esc
    ;; set up bindings to be added to '[' if desired
    (let ((escape-bk-w-binds (list 'escape nil))
	  (lbrack-bk-w-binds (assoc ?\[ (symbol-value bks-w-binds-symb))))
      (when lbrack-bk-w-binds
	(dolist (mods-type-w-bind (nthcdr 2 lbrack-bk-w-binds))
	  (when (memq 'control (car mods-type-w-bind))
	    (add-to-list
	     'escape-bk-w-binds
	     (cons
	      ;; I use `event-convert-list' then `event-modifiers' so that
	      ;;  modifiers are always listed in the order returned by
	      ;; `event-modifiers'.
	      (event-modifiers
	       (event-convert-list
		(append (delq 'control (car mods-type-w-bind))
			(list 'escape))))
	      (cdr mods-type-w-bind))
	     t)
	    (when do-not-keep-esc-in-lbr
	      ;; note that `delq' should be able to delete
	      ;;  MODS-TYPE-W-BIND by side effect
	      (delq mods-type-w-bind lbrack-bk-w-binds))))
	(when (<= (length lbrack-bk-w-binds) 2)
	  ;; when all binds in LBRACK-BK-W-BINDS have been deleted
	  (set bks-w-binds-symb (delq lbrack-bk-w-binds
				      (symbol-value bks-w-binds-symb)))
	  (setq change-in-bks-length (1- change-in-bks-length))))
      ;; now we have to deal with 27
      (when (char-table-range binds-char-table 27)
	(setcdr (last escape-bk-w-binds)
		(list
		 (cons nil (cdr (char-table-range binds-char-table 27)))))
	(when do-not-keep-esc-in-lbr
	  (set-char-table-range binds-char-table 27 nil)))
      (when (> (length escape-bk-w-binds) 2)
	;; when binds have been added to ESCAPE-BK-W-BINDS
	(add-to-list bks-w-binds-symb escape-bk-w-binds)
	(setq change-in-bks-length (1+ change-in-bks-length)))))
  (dolist (translate-event translate-events-list)
    (let* ((event (car translate-event))
	   (mods (event-modifiers event))
	   (basic-type (event-basic-type event))
	   defn asscn mods-type-w-bind)
      ;; if EVENT fits in a char-table, first check BINDS-CHAR-TABLE
      (when (and (pkb-char-event-p event)
		 (char-table-range binds-char-table event))
	(setq defn (cdr (char-table-range binds-char-table event)))
	(when (nth 2 translate-event)
	  (set-char-table-range binds-char-table event nil)))
      ;; EVENT not in BINDS-CHAR-TABLE
      (unless defn
	(setq asscn (assoc basic-type (symbol-value bks-w-binds-symb)))
	(setq mods-type-w-bind (assoc mods (nthcdr 2 asscn)))
	(when mods-type-w-bind
	 ;; EVENT found in BKS-W-BINDS-SYMB
	  (setq defn (cdr mods-type-w-bind))
	  (when (nth 2 translate-event)
	   ;; when we are supposed to delete EVENT (because
	   ;;  DELETE-ORIG-FL is non-nil and MODS-TYPE-W-BIND is the
	   ;;  last one in ASSCN)
	    (if (<= (length asscn) 3)
	     ;; if EVENT is the last mods-type in ASSCN
		(progn
		  (set bks-w-binds-symb
		     (delq asscn (symbol-value bks-w-binds-symb)))
		  (setq change-in-bks-length (1- change-in-bks-length)))
	     ;; else EVENT not the last mods-type in ASSCN
	      (delq mods-type-w-bind asscn)))))
      (when defn
       ;; proceed to copy EVENT if defined
	(dolist (target-event (nth 1 translate-event))
	  (setq mods (event-modifiers target-event))
	  (setq basic-type (event-basic-type target-event))
	  (setq asscn (assoc basic-type (symbol-value bks-w-binds-symb)))
	  (if asscn
	   ;; if BASIC-TYPE has an entry in BKS-W-BINDS-SYMB
	      (if (assoc mods (nthcdr 2 asscn))
		;; what do we do if TARGET-EVENT already defined?
		  (error
		   "Attempt to translate to event %s, which has a definition"
		   target-event)
		;; TARGET-EVENT not yet defined
		(setcdr (last asscn) (list (cons mods defn))))
	   ;; else BASIC-TYPE does not have an entry in BKS-W-BINDS-SYMB
	    (if (pkb-char-event-p target-event)
	     ;; if TARGET-EVENT fits in a char-table
		(if (char-table-range binds-char-table target-event)
		 ;; quit if TARGET-EVENT already defined
		    (error (concat "Attempt to translate to event %s, "
				   "which already has a definition")
			   target-event)
		  (set-char-table-range binds-char-table target-event (cons nil defn)))
	     ;; else TARGET-EVENT does not fit in a char-table
	      (add-to-list bks-w-binds-symb
			   (list basic-type nil (cons mods defn)))
	      (setq change-in-bks-length (1+ change-in-bks-length))))))))
  change-in-bks-length)
)

(defun pkb-translate-simp
  (mks max-mks-for-simple
   &optional translate-events-list do-not-keep-esc-in-lbr
   do-not-translate-lbr-to-esc) ;; OK
  "Translate events (i.e. move or copy bindings) in MKS, as long as the
resulting number of mks is less than MAX-MKS-FOR-SIMPLE. In the latter case, an updated mks list is returned (and MKS may be altered); otherwise, 0 is returned.

The optional arguments are as in `pkb-translate-events'"
  (let (tent-mks tent-dels (mks-tally (length mks)))
   ;; TENT-MKS/TENT-DELS are MKS that will be added to/deleted from MKS if
   ;;  the resultant number of mks is less than MAX-MKS-FOR-SIMPLE.
    (catch 'quit
      (dolist (mk-w-bind mks)
	(let* ((mods-type (event-modifiers (car mk-w-bind)))
	       (basic-type (event-basic-type (car mk-w-bind))))
	  (unless do-not-translate-lbr-to-esc
	    ;; if current mk can be expressed as an escape key, move/copy it
	    (when (and (equal basic-type ?\[) (memq 'control mods-type))
	      (if do-not-keep-esc-in-lbr
		  (add-to-list 'tent-dels mk-w-bind)
		(setq mks-tally (1+ mks-tally)))
	      (add-to-list
	       'tent-mks
	       (append (list (event-convert-list
			      (append (delq 'control mods-type) '(escape)))
			     nil)
		       (nthcdr 2 mk-w-bind)))))
	  (let ((asscn (assoc (car mk-w-bind) translate-events-list)))
	    (when asscn
	      (when (nth 2 asscn)
		  (add-to-list 'tent-dels mk-w-bind)
		  (setq mks-tally (1- mks-tally)))
	      (dolist (target-mk (nth 1 asscn))
		(setq mks-tally (1+ mks-tally))
		(add-to-list 'tent-mks
			     (append (list target-mk nil)
				     (nthcdr 2 mk-w-bind))))))
	  (when (> mks-tally max-mks-for-simple)
	    ;; when there are too many events for a simple keymap
	    (throw 'quit 0))))
      ;; at this point, number of MKS is OK
      (dolist (mk-to-del tent-dels)
	(setq mks (delq mk-to-del mks)))
      (if tent-mks
	  (setcdr (last tent-mks) mks)
	(setq tent-mks mks))
      tent-mks))
)

(defun pkb-split-full-to-groups
  (key-list key-groups &optional percent-for-full-gr) 
  "Split a full prefix-key, as given by KEY-LIST, into groups given by
KEY-GROUPS. Note that KEY-LIST will be modified.
Unless overridden by a group-specific setting, a group is given as a full-gr
\(i.e. printed with all base-keys, even those without bindings) if at least
PERCENT-FOR-FULL-GR percent of base-keys in the group have
bindings. PERCENT-FOR-FULL-GR defaults to 70%.

The format for KEY-LIST is the same as the cdr of the output from
`pkb-categorize-key-list' for a full keymap, i.e. a cons of the form
\(BKS-W-BINDS . BINDS-CHAR-TABLE). Note that the elements of KEY-LIST may be
modified by this command.

KEY-GROUPS is a list of the key-groups for a full keymap. A key-group is a
list of the form
  ( (group-name &optional NUM-FOR-FULL-GR OPTIONS) 
    BKS-IN-ONE-BLOCK-1 ... BKS-IN-ONE-BLOCK-k),
where BKS-IN-ONE-BLOCK-i is a list of the form (BK-1 ... BK-n), i.e. a list
of the bks expected for ONE-BLOCK-i. NUM-FOR-FULL-GR gives the number of
base-keys that need to be defined for the group to be printed as a full-gr;
NUM-FOR-FULL-GR can be omitted, whereby PERCENT-FOR-FULL-GR is used
instead. OPTIONS is intended for additional group-specific options to be
specified.

The output will be 
\(BKS-W-BINDS-USED REMAINING-BKS-W-BINDS . GROUPS).
BKS-W-BINDS-USED is a list of all the BKS-W-BINDS in the groups, each one
contained exactly once (so that they can converted for
output). REMAINING-BKS-W-BINDS will hold all bindings not listed in
GROUPS. Note that the BKS-W-BINDS in REMAINING-BKS-W-BINDS are not also in
BKS-W-BINDS-USED.
 
GROUPS is a list of ONE-GROUP's, each of which is in one of two formats:
 1) ('full-gr (GROUP-NAME OPTIONS) . BLOCKS-PRELIM).
 2) ('compact-gr (GROUP-NAME OPTIONS) . BKS-W-BINDS)."

  (let* (groups
	 (bks-w-binds (car key-list))
	 (binds-char-table (cdr key-list))
	 new-one-bk-w-binds-from-chars
	 (remaining-from-bks-w-binds (copy-sequence bks-w-binds))
	 bks-w-no-binds)
  ;; GROUPS is intended as part of the output
  ;; NEW-ONE-BK-W-BINDS-FROM-CHARS will be a list of the new cons cells
  ;;  created from characters from BINDS-CHAR-TABLE that have been
  ;;  included. 
  ;; REMAINING-FROM-BKS-W-BINDS will hold the entries from BKS-W-BINDS that
  ;;  aren't used in any groups.
  ;; BKS-W-NO-BINDS will be a list of the unmatched bks (i.e. elements of
  ;;  the form '(BK) ) that have been included in groups. 
  ;; Note: for the current purposes of this function, I could just add
  ;;  all bks to BKS-W-BINDS instead of separating out some bks into
  ;;  NEW-ONE-BK-W-BINDS-FROM-CHARS and BKS-W-NO-BINDS to BKS-W-BINDS;
  ;;  however, it may be useful at a later point to have them
  ;;  separate.
  (unless percent-for-full-gr (setq percent-for-full-gr 0.7))
  (dolist (one-key-group key-groups)
    (let (blocks-prelim bks-w-binds-for-compact-gr 
	  (bks-total-tally 0) (bks-included-tally 0)
	  num-for-full-gr bks-w-no-binds-tent)
     ;; BLOCKS-PRELIM, BKS-W-BINDS-FOR-COMPACT-GR will be built up for the
     ;;  output (though only one of them will be used depending on
     ;;  BKS-INCLUDED-TALLY vs. NUM-FOR-FULL-GR)
     ;; BKS-TOTAL-TALLY will tally the total number of bks in ONE-KEY-GROUP
     ;; BKS-INCLUDED-TALLY will tally the number of bks with bindings in
     ;;  ONE-KEY-GROUP
     ;; NUM-FOR-FULL-GR has an affect as described in this function's
     ;;  description
     ;; BKS-W-NO-BINDS-TENT will hold the unmatched bks in the block, to be
     ;;  added to BKS-W-NO-BINDS if the group is a `full-gr'
    (dolist (bks-in-one-block (cdr one-key-group))
      (let (one-block-prelim)
      (dolist (bk bks-in-one-block)
	;; search for BK in BKS-W-BINDS and BINDS-CHAR-TABLE
	(let (one-bk-w-binds)
	(cond
	 ((setq one-bk-w-binds (assoc bk bks-w-binds))
	  (setq remaining-from-bks-w-binds
		(delq one-bk-w-binds remaining-from-bks-w-binds)))
	 ((setq one-bk-w-binds (or (assoc bk new-one-bk-w-binds-from-chars)
				   (assoc bk bks-w-no-binds)
				   (assoc bk bks-w-no-binds-tent)))
	  nil)
	 (t
	  ;; generate ONE-BK-W-BINDS if BK doesn't already have an entry in
	  ;;  BKS-W-BINDS, NEW-ONE-BK-W-BINDS-FROM-CHARS, or BKS-W-NO-BINDS
	  (setq one-bk-w-binds (list bk nil))
	  (when (pkb-char-event-p bk)
	    (when (or (consp bk) (event-modifiers bk))
	      (error "Key group base keys must be a single base key"
		     " (i.e. not a char-range and not a modified-key)"))
	    ;; when BK is not in BKS-W-BINDS but BK (or C-BK, S-BK) could be
	    ;;  in BINDS-CHAR-TABLE
	    (when (char-table-range binds-char-table bk)
	      (add-to-list 'one-bk-w-binds
			   (append '(nil)
				   (cdr
				    (char-table-range binds-char-table bk)))
			   t 'ignore)
	      (set-char-table-range binds-char-table bk nil))
	    ;; C-BK is an integer if BK is 64, 91-95, 97-122.
	    ;; Thus, check for C-BK if BK is in that range.
	    (when (or (equal bk 64)
		      (and (>= bk 91) (<= bk 122) (/= bk 96)))
	      (let ((control-bk (event-convert-list (list 'control bk))))
	      (when (char-table-range binds-char-table control-bk)
	       ;; when CONTROL-BK (BK with a control modifier) has an entry
	       ;;  in BINDS-CHAR-TABLE
		(add-to-list 'one-bk-w-binds
		    (append '((control))
		  	    (cdr
			     (char-table-range binds-char-table control-bk)))
		  t 'ignore)
		(set-char-table-range binds-char-table control-bk nil)))
	      ;; S-BK is an integer if BK 97-122
	      ;; Thus, check for C-BK if BK is in that range.
	      (when (and (integerp bk) (>= bk 97) (<= bk 122))
		(let ((shift-bk (- bk 32)))
		(when (char-table-range binds-char-table shift-bk)
		 ;; when SHIFT-BK (BK with a shift modifier) has an entry
		 ;;  in BINDS-CHAR-TABLE
		  (add-to-list 'one-bk-w-binds
		      (append '((shift))
			      (cdr
			       (char-table-range binds-char-table shift-bk)))
		    t 'ignore)
		  (set-char-table-range binds-char-table shift-bk nil))))))
	  (if (nth 2 one-bk-w-binds)
	   ;; if BK has some definition
	      (add-to-list 'new-one-bk-w-binds-from-chars one-bk-w-binds)
	    (add-to-list 'bks-w-no-binds-tent one-bk-w-binds))))
	;; at this point, ONE-BK-W-BINDS is set appropriately
	(add-to-list 'one-block-prelim one-bk-w-binds t 'ignore)
	(when (nth 2 one-bk-w-binds)
	  ;; when BK has some bindings
	  (setq bks-included-tally (1+ bks-included-tally))
	  (add-to-list 'bks-w-binds-for-compact-gr one-bk-w-binds t 'ignore))
	(setq bks-total-tally (1+ bks-total-tally))))
      (add-to-list 'blocks-prelim one-block-prelim t 'ignore)))
    (setq num-for-full-gr
	  (cond ((nth 1 (car one-key-group)))
		(t (fceiling (* percent-for-full-gr bks-total-tally)))))
    (let* ((key-group-header (car one-key-group))
	   (new-group-header (cons (car key-group-header)
				   (nthcdr 2 key-group-header))))
      ;; NEW-GROUP-HEADER is the old group header with the second entry
      ;;  (NUM-FOR-FULL-GR) removed. We don't simply use `delq' on the second
      ;;  element of the old header because we don't want to change
      ;;  KEY-GROUPS.
      (cond
       ((>= bks-included-tally num-for-full-gr)
	(dolist (one-bk-w-no-binds bks-w-no-binds-tent)
	  (add-to-list 'bks-w-no-binds one-bk-w-no-binds nil 'eq))
	(add-to-list 'groups
		     (append (list 'full-gr new-group-header) blocks-prelim)
		     t 'ignore))
       ((> bks-included-tally 0)
	(add-to-list 'groups
		     (append (list 'compact-gr new-group-header)
			     bks-w-binds-for-compact-gr)
		     t 'ignore))))))

  ;; GROUPS completed at this point

  ;; remove entries that weren't matched into groups from bks-w-binds
  (dolist (unused-entry remaining-from-bks-w-binds)
    (setq bks-w-binds (delq unused-entry bks-w-binds)))

  ;; set BKS-W-BINDS to have all the ONE-BK-W-BINDS's, including those
  ;;  generated from BINDS-CHAR-TABLE and BKS-W-NO-BINDS
  (setq bks-w-binds
	(append bks-w-binds new-one-bk-w-binds-from-chars bks-w-no-binds))

  ;; fill REMAINING-FROM-BKS-W-BINDS with any remaining elements in
  ;;  BINDS-CHAR-TABLE
  (map-char-table
   (lambda (event defn)
     (if (consp event)
	 (add-to-list 'remaining-from-bks-w-binds
		      (list (copy-tree event) (car defn)
			    (cons nil (cdr defn)))
		      t 'ignore)
       (let* ((mods-type (event-modifiers event))
	      (basic-type (event-basic-type event))
	      (one-bk-w-binds (assoc basic-type remaining-from-bks-w-binds)))
       (if one-bk-w-binds
	   (add-to-list 'one-bk-w-binds (cons mods-type (cdr defn))
			t 'ignore)
	 (add-to-list 'remaining-from-bks-w-binds
		      (list basic-type nil (cons mods-type (cdr defn)))
		      t 'ignore)))))
   binds-char-table)
  
  (append (list bks-w-binds remaining-from-bks-w-binds) groups))
)

(defun pkb-sort-pks-list (pks-list &optional combine-simple-fl)
;; not corrected for format of MKS, BKS-W-BINDS, and BIND-W-OPT's
  "Destructively sort a PKS-LIST; also, if COMBINE-SIMPLE-FL, combine
consecutive simple sections.

A PKS-LIST is a list whose elements are given by the output of `pkb-key-list-to-output' and are one of three types (see definition of that function for more information):
1) (pk 'compact bks-w-binds)
2) (pk 'full bks-w-binds binds-char-table)
3) (pk 'simple mks).

If COMBINE-SIMPLE-FL, the sorted list that this command returns has the same
format except that elements of type 3 are replaced with:
3') (pks-simple-list 'simple-mixed mks) - a list of prefix-keys
PKS-SIMPLE-LIST and the list of modified-keys MKS for those prefix-keys."
  ;; sort list
  (setq pks-list
	(sort pks-list
	      (lambda (pk-for-output1 pk-for-output2)
		(pkb-compare-key-sequences
		 (car pk-for-output1) (car pk-for-output2)))))
  (when combine-simple-fl
    (let (consec-simp (prev-cons nil) (cur-cons pks-list) cons-to-rep-list)
     ;; PREV-CONS, CUR-CONS are used in iterating through pks-list
     ;;  while finding and marking consecutive simple entries.
     ;; CONSEC-SIMP will be a list of consecutive simple entries to be
     ;;  combined.
     ;; The first part of the algorithm will replace any series of
     ;;  consecutive simp entries in PKS-LIST with a single CONSEC-SIMP entry
     ;;  and then note the cons cells containing these CONSEC-SIMP entries in
     ;;  CONS-TO-REP-LIST. Later, we will go through CONS-TO-REP-LIST and
     ;;  replace the CONSEC-SIMP entries with the desired format.
      (when (eq (nth 1 (car pks-list)) 'simple)
       ;; when first element is simple, have to deal with separately
	;; first add to CONSEC-SIMP
	(setq consec-simp (list (car cur-cons)))
	(while (eq (nth 1 (car (setq cur-cons (cdr cur-cons)))) 'simple)
	  (add-to-list 'consec-simp (car cur-cons) t))
	(setq pks-list (cons consec-simp cur-cons))
	(setq cons-to-rep-list (list pks-list))
	(setq prev-cons cur-cons)
	(setq consec-simp nil))
      (while cur-cons
	(if (eq (nth 1 (car cur-cons)) 'simple)
	 ;; if the current cell is simple
	    (if consec-simp
	     ;; if the current cell is preceded by another simple cell
		(add-to-list 'consec-simp (car cur-cons) t)
	     ;; else the current cell is not preceded by another simple cell
	      (setq consec-simp (list (car cur-cons))))
	 ;; else the current cell is not simple
	  (when consec-simp
	   ;; when the previous cell is simple and the current one is not
	    (setcdr prev-cons (cons consec-simp cur-cons))
	    (add-to-list 'cons-to-rep-list (cdr prev-cons) t)
	    (setq consec-simp nil))
	  (setq prev-cons cur-cons))
	(setq cur-cons (cdr cur-cons)))
      (when consec-simp
       ;; when final cell was in a series of simple cells
	(setcdr prev-cons (cons consec-simp nil))
	(add-to-list 'cons-to-rep-list (cdr prev-cons) t))
      (dolist (cons-to-rep cons-to-rep-list)
	(let (simp-pks simp-kss)
	  (dolist (simp-entry (car cons-to-rep))
	    (add-to-list 'simp-pks (car simp-entry) t)
	    (dolist (one-mk (nth 2 simp-entry))
	      (add-to-list 'simp-kss
			   (cons
			    (vconcat (car simp-entry) (list (car one-mk)))
			    (cdr one-mk)) t)))
	  (setcar cons-to-rep (list simp-pks 'simple-mixed simp-kss))))))
  pks-list
)

(defun pkb-bks-w-binds-to-blocks-prelim (bks-w-binds max-bks-per-row) ;; OK
  "Split the bks in BKS-W-BINDS into rows (of ONE-BLOCK-PRELIM) with
MAX-BKS-PER-ROW bks per row. This converts BKS-W-BINDS (possibly for a
compact prefix-key) into BLOCKS-PRELIM (i.e. the input format for
`pkb-process-blocks').

BKS-W-BINDS is a list of ONE-BK-W-BINDS, as explained in
`pkb-categorize-key-list'; the format for BLOCKS-PRELIM is a list of
ONE-BLOCK-PRELIM's, each of which is a list of ONE-BK-W-BINDS."
  (let (blocks-prelim (count 0))
   ;; First, we break the bks into one-block-prelims
  (dolist (one-bk-w-binds bks-w-binds)
    (if (= 0 (mod count max-bks-per-row))
	(add-to-list 'blocks-prelim (list one-bk-w-binds) t 'ignore)
      (setcdr (last (car (last blocks-prelim))) (list one-bk-w-binds)))
    (setq count (1+ count)))
  blocks-prelim)
)

(defun pkb-process-blocks (blocks-prelim) ;; OK
  "Process BLOCKS-PRELIM into BLOCKS, ideal for printing tables. 

See comments in header of this .el for descriptions of BLOCKS-PRELIM
and BLOCKS."
  (let (blocks)
    (dolist (one-block-prelim blocks-prelim)
      (let (block-mods one-block)
      ;; first, we will extract all the modifiers in the block
      (dolist (one-bk-w-binds one-block-prelim)
	(dolist (mods-type-w-bind (nthcdr 2 one-bk-w-binds))
	;; MODS-TYPE-W-BIND is of the form
	;;    (MODS-TYPE-i BIND-i BIND-i-OPTIONS)
	  (unless (member (car mods-type-w-bind) block-mods)
	    (add-to-list 'block-mods (car mods-type-w-bind)))))
      ;; BLOCK-MODS should now contain all modifiers in ONE-BLOCK-PRELIM
      ;; time to create processed block (i.e. ONE-BLOCK)
      (setq block-mods (sort block-mods 'pkb-compare-modifier-lists))
      (setq one-block (list (list 'bks)))
      (dolist (mod block-mods)
	(add-to-list 'one-block (list mod) t 'ignore))
      ;; fill in ONE-BLOCK
      (dolist (one-bk-w-binds one-block-prelim)
	(let (;; (one-bk-w-binds-temp (copy-sequence one-bk-w-binds))
	      mods-type-w-bind)
	 ;; we don't want to destroy ONE-BK-W-BINDS while making the
	 ;;  ONE-BLOCK
	(dolist (one-row one-block)
	  (if (eq (car one-row) 'bks)
	      ;; first, add bk
	      (setcdr (last one-row) (list (cons (nth 0 one-bk-w-binds)
						 (nth 1 one-bk-w-binds))))
	    ;; find the mods-type-w-bind for the current ONE-BK-W-BINDS and
	    ;;  add it to the end of ONE-ROW
	    (setq mods-type-w-bind
		  (assoc (car one-row) (nthcdr 2 one-bk-w-binds)))
	    (setcdr (last one-row) (list (cdr mods-type-w-bind)))
	    ;; ;; now, delete MODS-TYPE-W-BIND so that the ONE-BK-W-BINDS-TEMP,
	    ;; ;;  which is an entry on the `bks' row, has only the elements
	    ;; ;;  (BK BK-OPTIONS)
	    ;; (when mods-type-w-bind
	    ;;  ;; we need this `when' because otherwise, when (null
	    ;;  ;;  MODS-TYPE-W-BIND), we might inappropriately delete an
	    ;;  ;;  element from ONE-BK-W-BINDS-TEMP
	    ;;   (delq mods-type-w-bind (cdr one-bk-w-binds-temp)))
	    ))))
      (add-to-list 'blocks one-block t 'ignore)))
    blocks)
)

(defun pkb-update-options (options-list string-or-option-key) ;; OK
  "Return an updated options list after adding STRING-OR-OPTION-KEY to OPTIONS-LIST.
If STRING-OR-OPTION-KEY is a string, it is treated as the output string of
the object (equivalent to passing (outp-str . OUTPUT-STRING)); if it is a
cons cell, the cons is added to the options list. This function
appropriately deals with whether or not any options are already set; setting
a string overwrites a previous string."
  (let (asscn)
    (when (and (consp string-or-option-key)
	       (eq (car string-or-option-key) 'outp-str))
      (setq string-or-option-key (cdr string-or-option-key)))
    (if (and (stringp string-or-option-key)
	     (or (stringp options-list) (null options-list)))
     ;; if OPTIONS-LIST should just be a string
	(setq options-list string-or-option-key)
     ;; else OPTIONS-LIST should be an alist
      (when (stringp options-list)
	(setq options-list (list (cons 'outp-str options-list))))
      (when (stringp string-or-option-key)
	(setq string-or-option-key (cons 'outp-str string-or-option-key)))
      (setq asscn (assq (car string-or-option-key) options-list))
      (if asscn
	  (setcdr asscn (cdr string-or-option-key))
	(add-to-list 'options-list string-or-option-key))
      options-list))
)

;; (defun pkb-key-sequence-location (key-sequence)
;;   "Specify where a definition KEY-SEQUENCE would be stored in a tree of
;; keymaps. Return (KEYMAP . EVENT), where KEYMAP is the keymap that would
;; contain the definition and EVENT is where in that keymap the definition
;; would be stored."
;;   (let* ((last-idx (- (length key-sequence) 1))
;; 	 (last-event (aref key-sequence last-idx))
;; 	 (last-event-mods (event-modifiers last-event))
;; 	 (but-last (substring key-sequence 0 -1))
;; 	 last-event-no-meta)
;;     (if (and (memq 'meta last-event-mods)
;; 	     (pkb-char-event-p
;; 	      (setq last-event-no-meta
;; 		    (event-convert-list
;; 		     (append (delq 'meta last-event-mods)
;; 			     (list (event-basic-type last-event)))))))
;;      ;; if KEY-SEQUENCE would be stored in the ESC key
;; 	(cons (vconcat but-last [27]) last-event-no-meta)
;;      ;; else KEY-SEQUENCE would not be stored in the ESC key
;;       (cons but-last last-event))))

(defun pkb-key-sequence-location (key-sequence)
  "Specify where the definition for KEY-SEQUENCE would be stored in a tree of
keymaps. Return (KEYMAP . EVENT), where KEYMAP is the keymap that would
contain the definition and EVENT is where in that keymap the definition
would be stored."
  (let* ((last-idx (- (length key-sequence) 1))
	 (last-event (aref key-sequence last-idx))
	 (last-event-mods (event-modifiers last-event))
	 (but-last (substring key-sequence 0 -1))
	 last-event-no-meta)
    (if (and (memq 'meta last-event-mods)
	     (pkb-char-event-p
	      (setq last-event-no-meta
		    (event-convert-list
		     (append (delq 'meta last-event-mods)
			     (list (event-basic-type last-event)))))))
     ;; if KEY-SEQUENCE would be stored in the ESC key
	(cons (vconcat but-last [27]) last-event-no-meta)
     ;; else KEY-SEQUENCE would not be stored in the ESC key
      (cons but-last last-event))))

(cl-defun pkb-character-code-to-escape-syntax (char &optional no-escaped-syntax-action
						interactive-flag)
  "Return the escaped character form of CHAR as a string. 
E.g. (character-code-to-escape-syntax ?\\C-\\M-r) gives \"?\\C-\\M-r\".

NO-ESCAPED-SYNTAX-ACTION controls how the function deals with CHARs that can not
be written using escape syntax. As an example, consider the character code given
by
    (+ ?@ (expt 2 26)) ; 
the 26th bit indicates the control modifier, so that this looks like
\"?\\C-\\@\", but actually \"?\\C-\\@\" gives 0. There is no way to enter this
code as \"?...\".  
NO-ESCAPED-SYNTAX-ACTION can be set to `unchanged' or nil, returns simply returns
CHAR as a string; `error', which returns an error; `pseudocode' which gives an
idea of what the character represents (e.g. the above example would be
\"?\\C-(\\@)\" ); or `return-nil', which simply returns nil."
  (interactive "ncharacter code: \ni\np")
  (when (not (integerp char))
    (error "CHAR should be an integer: %S" char)
    (cl-return-from character-code-to-escape-syntax))
  ;; note that the character codes wrap around after 2^28
  (setq char (mod char (expt 2 28)))
  (let (char-wo-mods modifiers outp basic-type no-escaped-syntax-action-message
		     pseudocode-flag)

    ;;  CHAR-WO-MODS is now CHAR with the modifiers bits subtracted off
    ;;  (this next line copied from `event-modifiers')
    (setf char-wo-mods (logand char
			       ;; note that (= ?\C-\@ 0) and alt (i.e. "\A") is
			       ;; the lowest modifier bit
			       (1- ?\A-\C-\@)))
    ;; 32 is the lowest character code which is not equivalent to a control character
    (setf modifiers (append modifiers (event-modifiers (+ (- char char-wo-mods) 32))))
    (setf outp
	  (pcase char-wo-mods
	   ;; first, let's test for special characters
	   (?\a "\\a")
	   (?\b "\\b")
	   (?\t "\\t")
	   (?\n "\\n")
	   (?\v "\\v")
	   (?\f "\\f")
	   (?\r "\\r")
	   (?\e "\\e")
	   (?\s "\\s")
	   (?\d "\\d")
	   ;;  non-ascii characters should be converted to hexadecimal
	   ((pred (lambda (char-wo-mods) (>= char-wo-mods 128)))
	    (format "\\x%x" char-wo-mods))
	   ;; deal with ascii characters
	   (_
	    ;;  we check for characters with no escape sequence. to do this,
	    ;;  note that characters between 63 and 95 plus the lowercase
	    ;;  versions of the letters (because ?\C-z, ?\C-Z are the same for
	    ;;  all letters), i.e. codes 63-95, 97-122, have
	    ;;  corresponding control characters (for characters 63-95, the
	    ;;  corresponding character is simply given by subtracting 64).  if
	    ;;  we have a character in that range and there is also a control
	    ;;  modifier in the modifer bits (the 2^26 bit), there is no way to
	    ;;  express the character as an escape sequence; in such a case, a
	    ;;  control modifier in the escape syntax would turn the code into a
	    ;;  control sequence in the range 0-31 rather than toggle the 2^26
	    ;;  bit [side note: actually ?\?=63 is there for a slightly
	    ;;  different reason, namely that emacs associates ?\C-\? = ?\d=127]
	    (when (or
		   (and (<=  63 char-wo-mods 122) (/= char-wo-mods 96)
			(member 'control modifiers))
		   ())
	      ;;  if we have no escaped syntax, let us set a common message
	      (setf no-escaped-syntax-action-message
		    "character-code-to-escape-syntax: No escape syntax possible for %d")
	      (cond

	       ((or (null no-escaped-syntax-action)
		    (eq no-escaped-syntax-action 'unchanged))
		(message no-escaped-syntax-action-message char)
		(cl-return-from character-code-to-escape-syntax
		  (number-to-string char)))
	       
	       ((eq no-escaped-syntax-action 'error)
		(error no-escaped-syntax-action-message char))
	       
	       ((eq no-escaped-syntax-action 'pseudocode)
		(message no-escaped-syntax-action-message char)
		;; the actual action will be taken later
		(setf pseudocode-flag t)
		)
	       
	       ((eq no-escaped-syntax-action 'return-nil)
		(message no-escaped-syntax-action-message char)
		(cl-return-from character-code-to-escape-syntax nil))
	       
	       (t (error "Invalid value for NO-ESCAPED-SYNTAX-ACTION: %S"
			 no-escaped-syntax-action))
	       )
	      )
	    (setf basic-type (event-basic-type char-wo-mods))
	    (concat	     
	     (when (member basic-type (append "()\\|;’‘\"#.," nil)) "\\")
	     ;;  for control characters, we can just use BASIC-TYPE. however. we
	     ;;  don't want to do this for printing characters because then we
	     ;;  lose capitalization (i.e. because "a", "A" both have basic-type
	     ;;  "a")
	     (string (if  (>= char-wo-mods 32)
			 char-wo-mods
		       ;;  we have a control characters, so add `control' to
		       ;;  modifiers
		       (push 'control modifiers)
		       basic-type))))))

    ;;  add pseucode signifier if appropriate
    (setq temp (list char-wo-mods modifiers))
    (when pseudocode-flag (setf outp (concat "(" outp ")")))
    
    ;; We want all the modifiers appended in a particular order, so we append
    ;; them all at once, rather than, e.g. appending them when first setting outp
    (cl-loop for (mod . str) in (reverse
				 '((control . "\\C") (meta . "\\M") (shift . "\\S")
				   (hyper . "\\H") (super . "\\s") (alt . "\\A")))
	     do (cl-loop repeat (cl-count mod modifiers)
			 do (setf outp (concat str "-" outp))))
    (setf outp (concat "?" outp))
    (unless (or pseudocode-flag (= (nth 0 (read-from-string outp)) char))
      (error (concat "Error in function. Character code %d "
		     "was escaped to \"%s\" but this appears incorrect")
	     char outp))
    (when interactive-flag (princ outp))
    outp))
(defalias 'pkb-char-escape-syntax 'pkb-character-code-to-escape-syntax)

(defun pkb-map-keymap-meta-chars-in-esc-prefix (function keymap)
  "Call FUNCTION once for each event \"M-ch\" in KEYMAP, where ch is an arbitrary character.
FUNCTION is called with two arguments: the event that is bound, and
the definition it is bound to.  The event may be a character range.

\(The events are those actually stored inside the ESC prefix of KEYMAP.)"
  ;;  note that if there are multiple definitions for ESC, e.g. because of
  ;;  inherited keymaps, all such definitions are returned and appended
  ;;  (e.g.:
  ;;   (let ((km-par (make-keymap))
  ;;   	  (km-child (make-keymap)))
  ;;     (set-keymap-parent km-child km-par)
  ;;     (define-key km-child [?\M-a] `defn-a-child)
  ;;     (define-key km-par [?\M-a] `defn-a-par)
  ;;     (define-key km-par [?\M-b] `defn-b-par)
  ;;     (equal (lookup-key km-child [?\e])
  ;;   	     '(keymap (keymap (97 . defn-a-child))
  ;;   		      keymap (98 . defn-b-par) (97 . defn-a-par))))
  ;;  )
  (let ((esc-defn (lookup-key keymap [?\e])) tsltd-key event-to-meta-event)
    ;; EVENT-TO-META-EVENT converts a character EVENT e to M-e
    ;;  remember that ?\C-@ = 0, so that ?\M-\C-@ is the meta bit alone.
    ;;  `logior' below could simply be `+' but using `logior' makes the intent
    ;;  more clear
    (fset 'event-to-meta-event (lambda (event) (logior ?\M-\C-@ event)))
    (when (keymapp esc-defn)
      (map-keymap
       (lambda (event defn)
	 (pcase event
	   ((pred characterp) (funcall function (event-to-meta-event event) defn))
	   (`(,(and event1 (pred characterp)) . ,(and event2 (pred characterp)))
	    (funcall function
		     (cons (event-to-meta-event event1) (event-to-meta-event event2))
		     defn))))
       esc-defn))))

(cl-defun pkb-print-event (event &optional (standard-output standard-output))
  "Output EVENT to STREAM.
Essentially, `pkb-character-code-to-escape-syntax' is used to output character codes and `prin1' is used for other things.

STANDARD-OUTPUT is used if STREAM not given.

\(fn EVENT STREAM)"
  (pcase event
    ((pred characterp) (princ (pkb-character-code-to-escape-syntax event)))
    (`(,(and event1 (pred characterp)) . ,(and event2 (pred characterp)))
     (princ "(")
     (princ (pkb-character-code-to-escape-syntax event1))
     (princ " . ")
     (princ (pkb-character-code-to-escape-syntax event2))
     (princ ")"))
    (event (prin1 event))))

(cl-defun pkb-print-keymap
    (keymap &optional (standard-output standard-output) indent prefix-key &key 
	    (events-as-escape-sequences-flag t) key-sequence-filter-fcn
	    print-defn-fcn sort-event-fcn (copy-esc-to-meta-flag t))
  "Print key bindings of KEYMAP to STREAM, returning the list created.

If EVENTS-AS-ESCAPE-SEQUENCES-FLAG (true, by default), print the events in
KEYMAP using `pkb-print-event', which in turn uses `pkb-character-code-to-escape-syntax'.

An event is printed only if, when passed as a vector key-sequence as the only
argument to KEY-SEQUENCE-FILTER-FCN, that function returns t.

By default, this function calls itself recursively when an event's definition is
a keymap and uses `prin1' otherwise. If PRINT-DEFN-FCN is non-nil, it is
called for each definition with four arguments: EVENT, DEFINITION,
INDENT, STREAM. EVENT and DEFINITION are defined as usual. INDENT is the number of
characters to indent by, calculated as number of characters on the line before
the current point in STREAM. Note that the return value of PRINT-DEFN-FCN is
collected in the \"defn\" slot when producing the return value of this function.

SORT-EVENT-FCN is a function used to sort events before printing them. It take the same syntax as PREDICATE in `sort'.

If COPY-ESC-TO-META-FLAG, characters defined in ESC are also shown as meta characters (i.e. so that M-a appears as [?\M-a], not just [?\e ?\a]).

\(fn keymap &optional stream &key (events-as-escape-sequences-flag t) key-sequence-filter-fcn print-defn-fcn sort-event-fcn (copy-esc-to-meta-flag t)"
  ;; INDENT and PREFIX-KEY are internal arguments.
  ;; Aside from the first line, ouput is indented by the string INDENT.
  ;; PREFIX-KEY is used to construct the key sequence for passing events to
  ;;  KEY-SEQUENCE-FILTER-FCN.

  (if (null key-sequence-filter-fcn)
      (setq key-sequence-filter-fcn (lambda (key-sequence) t))
    ;; if KEY-SEQUENCE-FILTER-FCN is not nil it must be a function
    (unless (functionp key-sequence-filter-fcn)
      (error (concat "KEY-SEQUENCE-FILTER-FCN must be a "
		     "function of one variable (a key sequence: %S")
	     key-sequence-filter-fcn)))

  (if (null print-defn-fcn)
      (setq print-defn-fcn
	    (lambda (event defn indent stream)
	      (if (keymapp defn)
		  (pkb-print-keymap
		   (defn stream indent (vconcat prefix-key (vector event)) 
		     events-as-escape-sequences-flag key-sequence-filter-fcn
		     nil sort-event-fcn copy-esc-to-meta-flag))
		(prin1 defn)
		defn)))
    ;; if PRINT-DEFN-FCN is not nil it must be a function
    (unless (functionp print-defn-fcn)
      (error (concat "PRINT-DEFN-FCN must be a "
		     "function of one variable (a key sequence: %S")
	     print-defn-fcn)))

  ;; error checking on KEYMAP
  (cond
   ((null keymap)
    (princ "nil") (cl-return-from pkb-print-keymap nil))
   ((not (keymapp keymap))
    (error "KEYMAP must be a keymap: %S" keymap)
    (cl-return-from pkb-print-keymap nil)))

  (let (keymap-entries returned-entries)
    (setq temp-entries nil)
    (map-keymap
     (lambda (event defn)
       (when (funcall key-sequence-filter-fcn (vconcat prefix-key (vector event)))
	 (push (cons event defn) keymap-entries)))
     keymap)

    ;; include meta events, if desired
    (when copy-esc-to-meta-flag
      (pkb-map-keymap-meta-chars-in-esc-prefix
       (lambda (event defn)
	 (when (funcall key-sequence-filter-fcn (vconcat prefix-key (vector event)))
	   (push (cons event defn) keymap-entries)))
       keymap))

    (setq keymap-entries (nreverse keymap-entries))
    
    (when sort-event-fcn
      (setq keymap-entries
	    (sort keymap-entries
		  (lambda (entry1 entry2)
		    (sort-event-fcn (car entry1) (car entry2))))))
    
    (when (null keymap-entries)
      (princ "nil")
      (cl-return-from pkb-print-keymap nil))

    ;; there are entries to print
    (ptcb keymap-entries)
    (princ "(")
    (cl-loop
     with line-start-string with returned-defn
     ;;  DEFN-TO-KEEP is the definition variable return 
     for (event defn) in keymap-entries
     for first = t then nil
     do
     (setq line-start-string
	   (concat
	    ;;  note that that the initial indent is increased by 1 because
	    ;;  of the opening parenthesis
	    (unless first (concat indent " "))
	    "("
	    (with-output-to-string
	      (if events-as-escape-sequences-flag (pkb-print-event event) (prin1 event)))
	    " . "))
     (princ (concat "\n" line-start-string))
     (setq returned-defn
	   (funcall print-defn-fcn event defn
		    (make-string (length line-start-string) ?\ ) standard-output))
     (princ ")")
     (pkb-push-end (cons event returned-defn) returned-entries)
     (pkb-push-end (cons event returned-defn) temp-entries))
    (princ ")")
    returned-entries))


;; (defun pkb-remap-esc-prefix-to-meta (keymap)
;;   "If ESC is a prefix-key in KEYMAP, (non-destuctively) remap all events in that prefix-key that fit in a char-table to an appropriate meta event in KEYMAP."
;;   (let ((keymap-copied (copy-keymap keymap))
;; 	(init-esc-defn (lookup-key keymap [?\e])) meta-key)
;;     (when (keymapp init-esc-defn)
;;       (define-key keymap ?\e (copy-keymap init-esc-defn)))
;;     (map-keymap
;;      (lambda (event defn)
;;        (when (pkb-char-event-p event)
;; 	 ;;  remember that ?\C-@ = 0, so that ?\M-\C-@ is the meta bit alone.
;; 	 ;;  `logior' below could simply be + but using `logior' makes the
;; 	 ;;  intent more clear
;; 	 (setf meta-key (logior ?\M-\C-@ event))
	 
;; 	 (define-key (event-convert-list))))
;;      init-esc-defn)))

(provide 'pkb)
