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

(define-key global-map [f1]   'next-error)

(define-key global-map [dead-tilde] "~")
(define-key global-map [S-dead-circumflex] "^")
(define-key global-map [S-dead-grave] "`")

(global-set-key [(shift f1)]  'previous-error)
(define-key global-map [f3]   'previous-error) ; in some XEmacs versions, shift in not recognized

(define-key global-map [f2]   'call-last-kbd-macro)
(define-key global-map [f5]   'occur)            ;; Search within buffer
(define-key global-map [f6]   'other-window)     ;; Jump to other window
(define-key global-map [f7]   'repeat-complex-command)
(define-key global-map [f9]   'manual-entry)     ;; man <command>  
(define-key global-map [f10]  'cleanuptabs-buffer)
(define-key global-map [f11]  'grep)
(define-key global-map [f12]  'compile)
(global-set-key [(control f11)]   'isearch-toggle-case-fold)
(global-set-key (kbd "C-.") 'dabbrev-expand) ;; M-/ takes 3 keys - tooinconvenient!
(global-set-key (kbd "C-#") 'comment-or-uncomment-region)

(set-variable 'compile-command "rake ")
(set-variable 'compile-command "mlgrep -S ")

(global-set-key [(shift f1)]  'previous-error)


(set-variable 'compile-command "rake ")
(set-variable 'grep-command "mlgrep -S ")

;; (require 'fontize)
;; (global-set-key [?\C-+] 'inc-font-size)
;; (global-set-key [?\C--] 'dec-font-size)
;; (global-set-key [?\M-+] 'font-next)
;; (global-set-key [?\M--] 'font-prev)

;; (setq load-path (cons (expand-file-name "/usr/share/doc/git-core/contrib/emacs") load-path))
;; (require 'vc-git)
;; (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
;; (require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)

;; (add-to-list 'load-path "~/scala/misc/scala-tool-support/emacs")
;; (require 'scala-mode-auto)

;; clojure-mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.mirah\\'" . ruby-mode))

(require 'linum)
;; (require 'pycomplexity)
;; (add-hook 'python-mode-hook
;;     (function (lambda ()
;;       (pycomplexity-mode)
;;       (linum-mode))))
