;;; clojure-mode.el -- Major mode for Clojure code

;; Copyright (C) 2007, 2008 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


(require 'comint)
(require 'inf-lisp)


(defgroup clojure-mode nil
  "Support for Clojure programming in Emacs"
  :prefix "clojure-"
  :group 'applications)


(defcustom clojure-inferior-lisp-program "clj"
  "*Program that runs Clojure."
  :type 'string
  :group 'clojure-mode)


(defcustom clojure-doc-command "(clojure/doc %s)\n"
  "Command to query inferior Clojure for a var's documentation"
  :type 'string
  :group 'clojure-mode)


(defcustom clojure-load-command "(load-file \"%s\")\n"
  "*Format-string for building a Clojure expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Clojure expression that will command the inferior Clojure
to load that file."
  :type 'string
  :group 'clojure-mode)


(defvar clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e\C-x" 'lisp-eval-defun)
    (define-key map "\C-x\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-l" 'clojure-load-file)
    (define-key map "\C-c\C-r" 'lisp-eval-region)
    (define-key map "\C-c\C-v" 'clojure-show-doc)
    (define-key map "\C-c\C-z" 'run-clojure)
    map)
  "Keymap for ordinary Clojure mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")


(easy-menu-define clojure-menu clojure-mode-map "Menu used in `clojure-mode'."
                  '("Clojure"
                    ["Eval defun"        lisp-eval-defun         t]
                    ["Eval defun and go" lisp-eval-defun-and-go  t]
                    ["Eval last sexp"    lisp-eval-last-sexp     t]
                    ["Eval region"       lisp-eval-region        t]
                    ["Eval region and go" lisp-eval-region-go    t]
                    ["Show Documentation" clojure-show-doc       t]
                    ["Load file..."      clojure-load-file       t]
                    ["Run Clojure"       run-clojure             t]))


(defvar clojure-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?~ "'   " table)
    (modify-syntax-entry ?, "    " table)
    table))


(defvar clojure-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `clojure-load-file' or `clojure-compile-file' command.")


;;;###autoload
(defun clojure-mode ()
  "Major mode for editing Clojure code - similar to Lisp mode..
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map clojure-mode-map)
  (setq major-mode 'clojure-mode)
  (setq mode-name "Clojure")
  (lisp-mode-variables)
  (set-syntax-table clojure-mode-syntax-table)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (make-local-variable 'lisp-indent-function)
  (setq lisp-indent-function 'clojure-indent-function)
  (setq font-lock-defaults
	'(clojure-font-lock-keywords
	  nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
	  (font-lock-mark-block-function . mark-defun)
	  (font-lock-syntactic-face-function
	   . lisp-font-lock-syntactic-face-function)))
  (run-mode-hooks 'clojure-mode-hook))


(defconst clojure-font-lock-keywords
  (eval-when-compile
    `( ;; Definitions.
      (,(concat "(\\(def"
		;; Function declarations.
		"\\(n\\|multi\\|macro\\|method\\|"
		;; Variable declarations.
                ""
		"\\)-?\\)\\>"
		;; Any whitespace
		"[ \t]*"
                ;; Possibly type
                "\\(?:#^\\sw+[ \t]*\\)?"
                "\\(\\sw+\\)?")
        (1 font-lock-keyword-face)
        (3 font-lock-function-name-face nil t))
      ;; Control structures
      (,(concat
         "(\\(?:clojure/\\)?" 
         (regexp-opt
          '(
            ;; clojure.core API, taken from http://richhickey.github.com/clojure/

            "*" "*1" "*2" "*3" "*agent*" "*clojure-version*" "*command-line-args*"
            "*compile-files*" "*compile-path*" "*e" "*err*" "*file*"
            "*flush-on-newline*" "*in*" "*ns*" "*out*" "*print-dup*" "*print-length*"
            "*print-level*" "*print-meta*" "*print-readably*" "*read-eval*"
            "*warn-on-reflection*"

            "+" "-" "->" "->>" ".." "/" "<" "<=" "=" "==" ">" ">="

            "accessor" "aclone" "add-classpath" "add-watch" "agent" "agent-error"
            "agent-errors" "aget" "alength" "alias" "all-ns" "alter" "alter-meta!"
            "alter-var-root" "amap" "ancestors" "and" "apply" "areduce" "array-map" "aset"
            "aset-boolean" "aset-byte" "aset-char" "aset-double" "aset-float" "aset-int"
            "aset-long" "aset-short" "assert" "assoc" "assoc!" "assoc-in" "associative?"
            "atom" "await" "await-for" "bases" "bean" "bigdec" "bigint" "binding"
            "bit-and" "bit-and-not" "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set"
            "bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean"
            "boolean-array" "booleans" "bound-fn" "bound-fn*" "bound?" "butlast" "byte"
            "byte-array" "bytes" "case" "cast" "char" "char-array" "char-escape-string"
            "char-name-string" "char?" "chars" "class" "class?" "clear-agent-errors"
            "clojure-version" "coll?" "comment" "commute" "comp" "comparator" "compare"
            "compare-and-set!" "compile" "complement" "concat" "cond" "condp" "conj"
            "conj!" "cons" "constantly" "construct-proxy" "contains?" "count" "counted?"
            "create-ns" "create-struct" "cycle" "dec" "decimal?" "declare" "definline"
            "defmacro" "defmethod" "defmulti" "defn" "defn-" "defonce" "defprotocol"
            "defrecord" "defstruct" "deftype" "delay" "delay?" "deliver" "denominator"
            "deref" "derive" "descendants" "disj" "disj!" "dissoc" "dissoc!" "distinct"
            "distinct?" "doall" "doc" "dorun" "doseq" "dosync" "dotimes" "doto" "double"
            "double-array" "doubles" "drop" "drop-last" "drop-while" "empty" "empty?"
            "ensure" "enumeration-seq" "error-handler" "error-mode" "eval" "even?"
            "every?" "extend" "extend-protocol" "extend-type" "extenders" "extends?"
            "false?" "ffirst" "file-seq" "filter" "find" "find-doc" "find-ns" "find-var"
            "first" "float" "float-array" "float?" "floats" "flush" "fn" "fn?" "fnext"
            "for" "force" "format" "future" "future-call" "future-cancel"
            "future-cancelled?" "future-done?" "future?" "gen-class" "gen-interface"
            "gensym" "get" "get-in" "get-method" "get-proxy-class" "get-thread-bindings"
            "get-validator" "hash" "hash-map" "hash-set" "identical?" "identity" "if-let"
            "if-not" "ifn?" "import" "in-ns" "inc" "init-proxy" "instance?" "int"
            "int-array" "integer?" "interleave" "intern" "interpose" "into" "into-array"
            "ints" "io!" "isa?" "iterate" "iterator-seq" "juxt" "key" "keys" "keyword"
            "keyword?" "last" "lazy-cat" "lazy-seq" "let" "letfn" "line-seq" "list"
            "list*" "list?" "load" "load-file" "load-reader" "load-string" "loaded-libs"
            "locking" "long" "long-array" "longs" "loop" "macroexpand" "macroexpand-1"
            "make-array" "make-hierarchy" "map" "map?" "mapcat" "max" "max-key" "memfn"
            "memoize" "merge" "merge-with" "meta" "methods" "min" "min-key" "mod" "name"
            "namespace" "neg?" "newline" "next" "nfirst" "nil?" "nnext" "not" "not-any?"
            "not-empty" "not-every?" "not=" "ns" "ns-aliases" "ns-imports" "ns-interns"
            "ns-map" "ns-name" "ns-publics" "ns-refers" "ns-resolve" "ns-unalias"
            "ns-unmap" "nth" "nthnext" "num" "number?" "numerator" "object-array" "odd?"
            "or" "parents" "partial" "partition" "pcalls" "peek" "persistent!" "pmap"
            "pop" "pop!" "pop-thread-bindings" "pos?" "pr" "pr-str" "prefer-method"
            "prefers" "print" "print-namespace-doc" "print-str" "printf" "println"
            "println-str" "prn" "prn-str" "promise" "proxy" "proxy-mappings" "proxy-super"
            "push-thread-bindings" "pvalues" "quot" "rand" "rand-int" "range" "ratio?"
            "rationalize" "re-find" "re-groups" "re-matcher" "re-matches" "re-pattern"
            "re-seq" "read" "read-line" "read-string" "reduce" "ref" "ref-history-count"
            "ref-max-history" "ref-min-history" "ref-set" "refer" "refer-clojure" "reify"
            "release-pending-sends" "rem" "remove" "remove-all-methods" "remove-method"
            "remove-ns" "remove-watch" "repeat" "repeatedly" "replace" "replicate"
            "require" "reset!" "reset-meta!" "resolve" "rest" "restart-agent"
            "resultset-seq" "reverse" "reversible?" "rseq" "rsubseq" "satisfies?" "second"
            "select-keys" "send" "send-off" "seq" "seq?" "seque" "sequence" "sequential?"
            "set" "set-error-handler!" "set-error-mode!" "set-validator!" "set?" "short"
            "short-array" "shorts" "shutdown-agents" "slurp" "some" "sort" "sort-by"
            "sorted-map" "sorted-map-by" "sorted-set" "sorted-set-by" "sorted?"
            "special-form-anchor" "special-symbol?" "split-at" "split-with" "str"
            "string?" "struct" "struct-map" "subs" "subseq" "subvec" "supers" "swap!"
            "symbol" "symbol?" "sync" "syntax-symbol-anchor" "take" "take-last" "take-nth"
            "take-while" "test" "the-ns" "thread-bound?" "time" "to-array" "to-array-2d"
            "trampoline" "transient" "tree-seq" "true?" "type" "unchecked-add"
            "unchecked-dec" "unchecked-divide" "unchecked-inc" "unchecked-multiply"
            "unchecked-negate" "unchecked-remainder" "unchecked-subtract" "underive"
            "update-in" "update-proxy" "use" "val" "vals" "var-get" "var-set" "var?"
            "vary-meta" "vec" "vector" "vector-of" "vector?" "when" "when-first"
            "when-let" "when-not" "while" "with-bindings" "with-bindings*" "with-in-str"
            "with-local-vars" "with-meta" "with-open" "with-out-str" "with-precision"
            "xml-seq" "zero?" "zipmap"

            ;; Forgotten?
            "do" "recur" "if" "."
            ) t)
         "\\>")
        .  1)
      ;; (fn name? args ...)
      (,(concat "(\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face nil t))
      ;; Constant values, Java method calls
      ("\\<[.:]\\sw+\\>" 0 font-lock-builtin-face)
      ;; Numeric literals.
      ("-?[0-9][0-9.xXa-fA-F]*" 0 font-lock-variable-name-face)
      ;; 
      ("@\\sw+" 0 font-lock-variable-name-face)
      ;; Capitalized words
      ("\\(\\b\\|/\\)\\(\\([a-z.]+\\.\\)?[A-Z][A-Za-z0-9_]*\\)" 2 font-lock-type-face)
      ;; Meta type annotation #^Type
      ("#^\\sw+" 0 font-lock-type-face)
      ))
  "Default expressions to highlight in Clojure mode.")


(defun clojure-load-file (file-name)
  "Load a Lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load Clojure file: " clojure-prev-l/c-dir/file
				  '(clojure-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq clojure-prev-l/c-dir/file (cons (file-name-directory file-name)
				     (file-name-nondirectory file-name)))
  (comint-send-string (inferior-lisp-proc)
		      (format clojure-load-command file-name))
  (switch-to-lisp t))


(defun clojure-show-doc (var)
  "Send a command to the inferior Clojure to give documentation for variable VAR.
See variable `clojure-doc-command'."
  (interactive (lisp-symprompt "Variable doc" (lisp-var-at-pt)))
  (comint-proc-query (inferior-lisp-proc)
		     (format clojure-doc-command var)))


;;;###autoload
(defun run-clojure (cmd)
  "Run Clojure as a subprocess of Emacs.
Like `inferior-lisp', but uses `clojure-inferior-lisp-program',
and modifies the local map .."
  (interactive (list (if current-prefix-arg
			 (read-string "Run clojure: " clojure-inferior-lisp-program)
		       clojure-inferior-lisp-program)))
  (inferior-lisp cmd)
  (local-set-key "\C-c\C-l" 'clojure-load-file))



(defun clojure-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function
which has a non-nil property `lisp-indent-function',
that specifies how to do the indentation.  The property value can be
* `defun', meaning indent `defun'-style;
* an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
* a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)              ;??
             (or (eq ?\[ (char-after (elt state 1))) ; inside vector literal
                 (not (looking-at "\\sw\\|\\s_"))))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
		(progn (goto-char calculate-lisp-indent-last-sexp)
		       (beginning-of-line)
		       (parse-partial-sexp (point)
					   calculate-lisp-indent-last-sexp 0 t)))
	    ;; Indent under the list or under the first sexp on the same
	    ;; line as calculate-lisp-indent-last-sexp.  Note that first
	    ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (get (intern-soft function) 'clojure-indent-function))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method indent-point state)))))))


(put 'loop 'clojure-indent-function 1)
(put 'for 'clojure-indent-function 1)   ; FIXME (for seqs expr) and (for seqs filter expr)
(put 'do 'clojure-indent-function 0)
(put 'let 'clojure-indent-function 1)
(put 'if 'clojure-indent-function 1)
(put 'if-let 'clojure-indent-function 2)
(put 'when 'clojure-indent-function 1)
(put 'when-let 'clojure-indent-function 2)
(put 'when-not 'clojure-indent-function 1)
(put 'locking 'clojure-indent-function 1)
(put 'defmulti 'clojure-indent-function 1)
(put 'binding 'clojure-indent-function 1)
(put 'sync 'clojure-indent-function 1)
(put 'doseq 'clojure-indent-function 2)
(put 'dotimes 'clojure-indent-function 2)
(put 'implement 'clojure-indent-function 1)
(put 'doto 'clojure-indent-function 1)
(put 'with-open 'clojure-indent-function 2)
(put 'with-local-vars 'clojure-indent-function 1)



(provide 'clojure-mode)

;;; clojure-mode.el ends here
