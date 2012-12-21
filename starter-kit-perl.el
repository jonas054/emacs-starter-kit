;;; starter-kit-perl.el --- Some helpful Perl code
;;
;; Part of the Emacs Starter Kit

(eval-after-load 'cperl-mode
  '(progn
     (define-key cperl-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key cperl-mode-map (kbd "C-M-h") 'backward-kill-word)))

(global-set-key (kbd "C-h P") 'perldoc)

(add-to-list 'auto-mode-alist '("\\.p[lm]$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))

(setq cperl-extra-newline-before-brace t
      cperl-brace-offset              -2
      cperl-merge-trailing-else        nil)

;; Finding POD is supposedly expensive, so cperl avoids doing so to
;; keep editing responsive.
;; From: http://www.perlmonks.org/bare/index.pl/?node_id=624541
(defun my-perl-refontify ()
  (interactive)
  (cperl-find-pods-heres)
  (font-lock-fontify-buffer))
(add-hook 'cperl-mode-hook
      (lambda ()
       (my-perl-refontify)))

;; TODO: flymake
;; TODO: electric bugaloo 

(provide 'starter-kit-perl)
;; starter-kit-perl.el ends here
