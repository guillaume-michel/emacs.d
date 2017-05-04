;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              0.5
        company-minimum-prefix-length   3
        company-show-numbers            nil
        company-tooltip-limit           20
        company-dabbrev-downcase        nil
        company-backends                '((company-irony company-gtags))
        )
  :bind ("C-TAB" . company-complete-common)
  )

;; Yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

(defcustom python-shell-interpreter "python3"
  "Default Python interpreter for shell."
  :type 'string
  :group 'python)

(defun orilla/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'orilla/python-mode-hook)
(add-hook 'python-mode-hook 'run-python-internal)

(provide 'setup-completion)
