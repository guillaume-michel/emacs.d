(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  ;;:commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (setq lsp-enable-indentation t
        lsp-semantic-tokens-enable nil
        lsp-auto-guess-root t
        lsp-prefer-flymake nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-max-width 200)
  (lsp-ui-doc-max-height 13)
  (lsp-ui-doc-delay 2)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Header completion
(use-package company-c-headers
  :ensure t
  :config
  (push 'company-c-headers company-backends))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

(provide 'setup-completion)
