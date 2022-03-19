(use-package julia-mode
  :ensure t
  :hook (julia-mode . lsp-deferred))

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)
  :config
  (set-language-environment "UTF-8")
  (julia-repl-set-terminal-backend 'vterm)
  (setq vterm-kill-buffer-on-exit nil))

(use-package lsp-julia
  :ensure t
  :config
  (setq lsp-julia-package-dir nil
        lsp-julia-default-environment "~/.julia/environments/v1.7"))

(use-package flycheck-julia
  :config (add-hook 'flycheck-mode-hook #'flycheck-julia-setup))


(provide 'setup-julia)
