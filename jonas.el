;; Jonas' customizations
(setq kill-whole-line    t) ; Ctrl-K removes the newline too

(defun transpose-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2)
  )
(defun transpose-line-down ()
  (next-line)
  (interactive)
  (transpose-lines 1)
  (previous-line)
  )

(defun scroll-down-1 ()
  (interactive)
  (scroll-down 1))

(defun scroll-up-1 ()
  (interactive)
  (scroll-up 1))

(global-set-key (kbd "M-p") 'transpose-line-up)
(global-set-key (kbd "M-n") 'transpose-line-down)

(global-set-key [(meta up)] 'scroll-down-1) 
(global-set-key [(meta down)]   'scroll-up-1)

(define-key global-map [dead-tilde] "~")
(define-key global-map [S-dead-circumflex] "^")
(define-key global-map [S-dead-grave] "`")

(define-key global-map [f1]   'next-error)
(global-set-key [(shift f1)]  'previous-error)
(define-key global-map [f3]   'previous-error) ; in some XEmacs versions, shift in not recognized

(define-key global-map [f2]   'call-last-kbd-macro)
(define-key global-map [f6]   'other-window)     ;; Jump to other window
(define-key global-map [f7]   'repeat-complex-command)
(define-key global-map [f9]   'manual-entry)     ;; man <command>  
(define-key global-map [f11]  'grep)
(define-key global-map [f12]  'compile)
(global-set-key [(control f11)]   'isearch-toggle-case-fold)
(global-set-key (kbd "C-.") 'dabbrev-expand) ;; M-/ takes 3 keys - too inconvenient!
(global-set-key (kbd "C-#") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

(define-key global-map [C-kp-6]  'previous-buffer)
(define-key global-map [C-kp-4]  'next-buffer)

(set-variable 'compile-command "rake ")
(set-variable 'grep-command "mlgrep -S ")

;;; Use "%" to jump to the matching parenthesis.
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t                    (self-insert-command (or arg 1))) ))
(global-set-key "%" `goto-match-paren)


;; --- JONAS ---
(require 'clearcase)
(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.make\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.rcw\\'" . conf-unix-mode))


(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "cmake-mode.el" t)

;; uniquify.el is a helper routine to help give buffer names a better unique name.
(when (load "uniquify" 'NOERROR)
  (require 'uniquify)
  ;(setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  )

(column-number-mode t)

(custom-set-variables
  '(py-pychecker-command "pychecker.sh")
  '(py-pychecker-command-args (quote ("")))
  '(python-check-command "pychecker.sh"))


(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/home/xtsjarv/src/check_python_syntax_edge.sh" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [f5] 'flymake-display-err-menu-for-current-line)


;; enhancements for displaying flymake errors
(require 'flymake-cursor)

(setq flymake-gui-warnings-enabled nil)

;; Change between hortizontal and vertical split.
(require 'transpose-frame)
(define-key global-map [f8]   'rotate-frame-clockwise)
(define-key global-map [C-f8] 'flop-frame)
(define-key global-map [M-f8] 'toggle-truncate-lines)

(load "clearcase.el")
(require 'clearcase)


(require 'fontize)
;; (global-set-key [?\C-+] 'inc-font-size)
;; (global-set-key [?\C--] 'dec-font-size)
(global-set-key [?\M-+] 'font-next)
(global-set-key [?\M--] 'font-prev)

;; (setq load-path (cons (expand-file-name "/usr/share/doc/git-core/contrib/emacs") load-path))
;; (require 'vc-git)
;; (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
;; (require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)

;(add-to-list 'load-path "~/scala/misc/scala-tool-support/emacs")
;(require 'scala-mode-auto)

;; clojure-mode
;(require 'clojure-mode)
;(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(defun update-heading-with-view ()
  (interactive)
  (setq frame-title-format
        (concat "["
                (substring (or (getenv "CLEARCASE_ROOT") "      ") 6) "] %b (%f)")))

(update-heading-with-view)

(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))

(defun reset-cut ()
  "toggle the intprogram cut function to avoid odd x hangup"
  (interactive)
  (message "Cut is %s" (setq interprogram-cut-function
                             (xor interprogram-cut-function
                                  'x-select-text))))

(defun reset-paste ()
  "toggle the intprogram paste function to avoid odd x hangup"
  (interactive)
  (message "Paste is %s" (setq interprogram-paste-function
                               (xor interprogram-paste-function
                                    'x-cut-buffer-or-selection-value))))

;;(setq interprogram-cut-function nil)

(setq cperl-extra-newline-before-brace t
      cperl-brace-offset              -2
      cperl-merge-trailing-else        nil)


(add-hook 'cperl-mode-hook
      (lambda ()
       (font-lock-add-keywords nil
        '(("^=\\w+.*\\(.*\n\\)*?=cut" 0 font-lock-comment-face)
          ))))
(require 'php-mode)
